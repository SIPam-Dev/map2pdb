unit debug.map.reader;

interface

uses
  Classes,
  debug.info;

type
  TMapReader = class
  public
    procedure ImportMap(Stream: TStream; DebugInfo: TDebugInfo); overload;
    procedure ImportMap(const Filename: string; DebugInfo: TDebugInfo); overload;
  end;

implementation

uses
  SysUtils;

{ TMapReader }

procedure TMapReader.ImportMap(const Filename: string; DebugInfo: TDebugInfo);
begin
  var Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try

    ImportMap(Stream, DebugInfo);

  finally
    Stream.Free;
  end;
end;

procedure TMapReader.ImportMap(Stream: TStream; DebugInfo: TDebugInfo);
var
  MapLineNumber: integer;
  LineBuffer: string;
  PeekBuffer: string;

  function PeekLine(Reader: TStreamReader; Skip: boolean = False): string;
  begin
    if (PeekBuffer = '') then
    begin
      while (not Reader.EndOfStream) do
      begin
        PeekBuffer := Reader.ReadLine.TrimLeft;
        Inc(MapLineNumber);
        if (not Skip) or (not PeekBuffer.IsEmpty) then
          break;
      end;
    end;

    Result := PeekBuffer;
  end;

  function CurrentLine(Reader: TStreamReader; Skip: boolean = False): string;
  begin
    if (LineBuffer = '') then
    begin
      if (PeekBuffer = '') then
      begin
        while (not Reader.EndOfStream) do
        begin
          LineBuffer := Reader.ReadLine.TrimLeft;
          Inc(MapLineNumber);
          if (not Skip) or (not LineBuffer.IsEmpty) then
            break;
        end;
      end else
      begin
        LineBuffer := PeekBuffer;
        PeekBuffer := '';
      end;
    end;

    Result := LineBuffer;
  end;

  function NextLine(Reader: TStreamReader; Skip: boolean = False): string;
  begin
    LineBuffer := '';
    Result := CurrentLine(Reader, Skip);
  end;

  function HasData(Reader: TStreamReader): boolean;
  begin
    Result := (LineBuffer <> '') or (PeekBuffer <> '') or (not Reader.EndOfStream);
  end;

  function HexToNibble(c: char): Cardinal;
  begin
    case c of
      '0'..'9': Result := Ord(c)-Ord('0');
      'a'..'f': Result := 10 + Ord(c)-Ord('a');
      'A'..'F': Result := 10 + Ord(c)-Ord('A');
    else
      Result := $FF;
    end;
  end;

  function HexToInt16(const s: string; Offset: integer): Word;
  begin
    Result := 0;
    for var i := 1 to SizeOf(Result)*2 do
      if (i+Offset <= Length(s)) then
      begin
        var Nibble := HexToNibble(s[i+Offset]);
        if (Nibble = $FF) then
          raise Exception.CreateFmt('Invalid hex number: "%s"', [Copy(s, 1+Offset, 4)]);
        Result := (Result SHL 4) or Nibble
      end else
        raise Exception.CreateFmt('Invalid hex number: "%s"', [Copy(s, 1+Offset, 4)]);
  end;

  function HexToInt32(const s: string; Offset: integer): Cardinal;
  begin
    Result := 0;
    for var i := 1 to SizeOf(Result)*2 do
      if (i+Offset <= Length(s)) then
      begin
        var Nibble := HexToNibble(s[i+Offset]);
        if (Nibble = $FF) then
          raise Exception.CreateFmt('Invalid hex number: "%s"', [Copy(s, 1+Offset, 8)]);
        Result := (Result SHL 4) or Nibble
      end else
        raise Exception.CreateFmt('Invalid hex number: "%s"', [Copy(s, 1+Offset, 8)]);
  end;

begin
  var Reader := TStreamReader.Create(Stream);
  try
    MapLineNumber := 0;

    (*
    ** Segments
    *)

    // " Start         Length     Name                   Class"
    while (HasData(Reader)) and (not CurrentLine(Reader, True).StartsWith('Start')) and (not LineBuffer.EndsWith('Class')) do
      NextLine(Reader, True);

    // Skip empty lines and exit if no more lines
    if (NextLine(Reader, True).IsEmpty) then
      Exit;

    // " 0001:00401000 000F47FCH .text                   CODE"
    while (not CurrentLine(Reader).IsEmpty) do
    begin
      var n := Pos(':', LineBuffer);
      var SegmentID: TDebugInfoSegment := HexToInt16(LineBuffer, 0);
      var Offset: TDebugInfoOffset := HexToInt32(LineBuffer, n);
      n := Pos(' ', LineBuffer, n+1);
      var Size: TDebugInfoOffset := HexToInt32(LineBuffer, n);

      n := Pos('.', LineBuffer, n+1);
      var n2 := Pos(' ', LineBuffer, n+1);
      var Name := Copy(LineBuffer, n, n2-n);

      var ClassName := Copy(LineBuffer, n2+1, MaxInt).Trim;

      var SegmentClass := DebugInfo.SegmentClasses.Add(SegmentID, ClassName, Name);

      SegmentClass.Offset := Offset;
      SegmentClass.Size := Size;

      NextLine(Reader);
    end;


    (*
    ** Modules
    *)

    // "Detailed map of segments"
    while (HasData(Reader)) and (CurrentLine(Reader, True) <> 'Detailed map of segments') do
      NextLine(Reader, True);

    // Skip empty lines and exit if no more lines
    if (NextLine(Reader, True).IsEmpty) then
      Exit;

    // " 0001:00000000 0000F684 C=CODE     S=.text    G=(none)   M=System   ACBP=A9"
    while (not CurrentLine(Reader).IsEmpty) do
    begin
      var n := Pos(' C=', LineBuffer);
      var Address := Copy(LineBuffer, 1, n-1);

      n := Pos('M=', LineBuffer);
      var Name := Copy(LineBuffer, n+2, Pos(' ', LineBuffer, n+2)-n-2);

      var Segment: TDebugInfoSegment := HexToInt16(Address, 0);
      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);
      n := Pos(' ', Address);
      var Size: TDebugInfoOffset := HexToInt32(Address, n);

      var SegmentClass := DebugInfo.SegmentClasses.FindByValue(Segment);

      var Module := DebugInfo.Modules.Add(Name, SegmentClass, Offset);

      Module.Size := Size;

      NextLine(Reader);
    end;

    (*
    ** Symbols - sorted by name
    *)

    // "  Address             Publics by Name"
    while (HasData(Reader)) and (not CurrentLine(Reader, True).EndsWith('Publics by Name')) do
      NextLine(Reader, True);

    // Skip empty lines and exit if no more lines
    if (NextLine(Reader, True).IsEmpty) then
      Exit;

    // " 0001:000E99AC       debug.info..TDebugInfo"
    while (not CurrentLine(Reader).IsEmpty) do
    begin
      var n := Pos(' ', LineBuffer);
      var Address := Copy(LineBuffer, 1, n-1);
      var Name := Copy(LineBuffer, n+1, MaxInt).TrimLeft;

      var Segment: TDebugInfoSegment := HexToInt16(Address, 0);
      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);

      var SegmentClass := DebugInfo.SegmentClasses.FindByValue(Segment);

      var Symbol := DebugInfo.Symbols.Add(Name, SegmentClass, Offset);

      NextLine(Reader);
    end;

    (*
    ** Symbols - sorted by address
    *)

    // "  Address             Publics by Value"
    while (HasData(Reader)) and (not CurrentLine(Reader, True).EndsWith('Publics by Value')) do
      NextLine(Reader, True);

    // Skip this section - it duplicates the previous

    (*
    ** Line numbers - grouped by module & segment
    *)

    // Rest of file is:
    // "Line numbers for System(WindowsAPIs.INC) segment .text"
    while (HasData(Reader)) do
    begin
      const sPrefix = 'Line numbers for ';
      while (HasData(Reader)) and (not CurrentLine(Reader, True).StartsWith(sPrefix)) do
        NextLine(Reader, True);

      if (not HasData(Reader)) then
        break;

      var n := Pos('(', LineBuffer);
      if (n = 0) then
        raise Exception.CreateFmt('[%d] Source file start marker "(" not found: %s', [MapLineNumber, LineBuffer]);
      var ModuleName := Copy(LineBuffer, Length(sPrefix)+1, n-1-Length(sPrefix));

      var n2 := Pos(')', LineBuffer, n+1);
      if (n2 = 0) then
        raise Exception.CreateFmt('[%d] Source file end marker ")" not found: %s', [MapLineNumber, LineBuffer]);
      var Filename := Copy(LineBuffer, n+1, n2-n-1);

      n := Pos('segment', LineBuffer, n2+1);
      if (n = 0) then
        raise Exception.CreateFmt('[%d] Source file segment marker "segment" not found: %s', [MapLineNumber, LineBuffer]);
      Inc(n, 7);
      while (LineBuffer[n] = ' ') do
        Inc(n);

      var SegmentName := Copy(LineBuffer, n, MaxInt);
      var SegmentClass := DebugInfo.SegmentClasses.FindByName(SegmentName);
      if (SegmentClass = nil) then
        raise Exception.CreateFmt('[%d] Unknown segment name: %s (%s)', [MapLineNumber, SegmentName, LineBuffer]);

      var Module := DebugInfo.Modules.FindByName(ModuleName, SegmentClass);
      if (Module <> nil) then
      begin
        var SourceFile := Module.SourceFiles.Add(Filename);

        // Skip empty lines and exit if no more lines
        if (NextLine(Reader, True).IsEmpty) then
          break;

        // "   335 0001:00004068   338 0001:00004070   343 0001:00004078   349 0001:00004080"
        while (not CurrentLine(Reader).IsEmpty) do
        begin
          var Ofs := 1;

          while (Ofs <= LineBuffer.Length) do
          begin
            // Get line number
            n := Pos(' ', LineBuffer, Ofs);
            var s := Copy(LineBuffer, Ofs, n-Ofs);
            var LineNumber: integer;
            if (not integer.TryParse(s, LineNumber)) then
              raise Exception.CreateFmt('[%d] Invalid line number: %s (%s)', [MapLineNumber, s, LineBuffer]);

            // Get segment type (we already have that info from the header)
            var Segment: TDebugInfoSegment := HexToInt16(LineBuffer, n);
            Inc(n, 4+1);

            // Get offset
            var Offset: TDebugInfoOffset := HexToInt32(LineBuffer, n);
            Inc(n, 8+1);

            if (Offset <> 0) then
              Module.SourceLines.Add(SourceFile, LineNumber, Offset);

            while (n <= LineBuffer.Length) and (LineBuffer[n] = ' ') do
              Inc(n);

            Ofs := n;
          end;

          if (NextLine(Reader).IsEmpty) then
            break;
        end;
      end else
      begin
        // Ignore empty modules. E.g.:
        // Line numbers for System.RTLConsts(System.RTLConsts.pas) segment .text
        //   611 0001:00000000

        // raise Exception.CreateFmt('[%d] Module not found: %s (%s)', [MapLineNumber, ModuleName, LineBuffer]);

        // Skip empty lines and exit if no more lines
        if (NextLine(Reader, True).IsEmpty) then
          break;

        // Skip non-empty lines
        while (not CurrentLine(Reader).IsEmpty) do
        begin
          if (NextLine(Reader).IsEmpty) then
            break;
        end;
      end;

    end;

  finally
    Reader.Free;
  end;
end;

end.

