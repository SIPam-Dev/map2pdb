unit debug.info.reader.map;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Classes,
  debug.info,
  debug.info.reader;

type
  TDebugInfoMapReader = class(TDebugInfoReader)
  private
    function Demangle(Module: TDebugInfoModule; const Name: string): string;
  protected
  public
    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); override;
  end;

implementation

uses
  System.SysUtils;

type
  TLineReader = class
  strict private
    FReader: TStreamReader;
    FLineNumber: integer;
    FLineBuffer: string;
    FHasLineBuffer: boolean;
    FPeekBuffer: string;
    FHasPeekBuffer: boolean;
  private
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function CurrentLine(Skip: boolean = False): string;
    function HasData: boolean;
    function NextLine(Skip: boolean = False): string;
    function PeekLine(Skip: boolean = False): string;

    property LineBuffer: string read FLineBuffer;
    property LineNumber: integer read FLineNumber;
  end;

constructor TLineReader.Create(Stream: TStream);
begin
  inherited Create;

  FReader := TStreamReader.Create(Stream);
end;

destructor TLineReader.Destroy;
begin
  FReader.Free;

  inherited;
end;

function TLineReader.PeekLine(Skip: boolean): string;
begin
  while (not FHasPeekBuffer) and (not FReader.EndOfStream) do
  begin
    FPeekBuffer := FReader.ReadLine.TrimLeft;
    Inc(FLineNumber);
    if (not Skip) or (not FPeekBuffer.IsEmpty) then
      FHasPeekBuffer := True;
  end;

  if (FHasPeekBuffer) then
    Result := FPeekBuffer
  else
    Result := '';
end;

function TLineReader.CurrentLine(Skip: boolean): string;
begin
  if (not FHasLineBuffer) then
  begin
    if (not FHasPeekBuffer) then
    begin
      while (not FHasLineBuffer) and (not FReader.EndOfStream) do
      begin
        FLineBuffer := FReader.ReadLine.TrimLeft;
        Inc(FLineNumber);
        if (not Skip) or (not FLineBuffer.IsEmpty) then
          FHasLineBuffer := True;
      end;
    end else
    begin
      FLineBuffer := FPeekBuffer;
      FHasPeekBuffer := False;
      FPeekBuffer := '';
    end;
  end;

  if (FHasLineBuffer) then
    Result := FLineBuffer
  else
    Result := '';
end;

function TLineReader.NextLine(Skip: boolean): string;
begin
  FHasLineBuffer := False;
  FLineBuffer := '';
  Result := CurrentLine(Skip);
end;

function TLineReader.HasData: boolean;
begin
  Result := (FHasLineBuffer) or (FHasPeekBuffer) or (not FReader.EndOfStream);
end;

{ TDebugInfoMapReader }

function TDebugInfoMapReader.Demangle(Module: TDebugInfoModule; const Name: string): string;
begin
  // More a beautyfier than a demangler since the MAP symbols aren't really mangled

  var n := 1;

  // Strip module name from symbol name
  if (Name.StartsWith(Module.Name)) then
    Inc(n, Module.Name.Length+1); // +1 to get rid of the separating "."

//  if (Pos('$thunk_', Name, n) = n) then
//    Inc(n, '$thunk_'.Length);

  // Types start with "."
  if (Name[n] = '.') then
    Exit('');

  // Skip {module}
  if (Name[n] = '{') then
    n := Pos('}', Name, n+1)+1;

  // Strip leading "@"
  if (Name[n] = '@') then
    Inc(n);

  if (n > 1) then
    Result := Copy(Name, n, MaxInt)
  else
    Result := Name;

  // Remove type unit scopes
  //   TList<System.Generics.Collections.TPair<System.TypInfo.PTypeInfo,System.string>>.GetList
  //   TList<TPair<PTypeInfo,string>>.GetList
  n := Pos('<', Result);
  while (n < Result.Length) and (n <> 0) do
  begin

    // Find next '<' or ','
    while (n <= Result.Length) and (Result[n] <> '<') and (Result[n] <> ',') do
      Inc(n);
    Inc(n); // Skip it

    if (n > Result.Length) then
      break;

    // Find last '.'
    var n2 := n;
    var LastDot := -1;
    while (Ord(Result[n2]) <= 255) and (AnsiChar(Result[n2]) in ['a'..'z', 'A'..'Z', '.']) do
    begin
      if (Result[n2] = '.') then
        LastDot := n2;
      inc(n2);
    end;
    if (LastDot <> -1) then
      Delete(Result, n, LastDot-n+1);

    inc(n);
  end;
end;

procedure TDebugInfoMapReader.LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
var
  Reader: TLineReader;

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
          Error(Reader.LineNumber, 'Invalid hex number: "%s"', [Copy(s, 1+Offset, 4)]);

        Result := (Result SHL 4) or Nibble
      end else
        Error(Reader.LineNumber, 'Invalid hex number: "%s"', [Copy(s, 1+Offset, 4)]);
  end;

  function HexToInt32(const s: string; Offset: integer): Cardinal;
  begin
    Result := 0;
    for var i := 1 to SizeOf(Result)*2 do
      if (i+Offset <= Length(s)) then
      begin
        var Nibble := HexToNibble(s[i+Offset]);
        if (Nibble = $FF) then
          Error(Reader.LineNumber, 'Invalid hex number: "%s"', [Copy(s, 1+Offset, 8)]);
        Result := (Result SHL 4) or Nibble
      end else
        Error(Reader.LineNumber, 'Invalid hex number: "%s"', [Copy(s, 1+Offset, 8)]);
  end;

begin
  Log('Reading MAP file');

  Reader := TLineReader.Create(Stream);
  try

    (*
    ** Segments
    *)

    Log('- Segments');
    // " Start         Length     Name                   Class"
    while (Reader.HasData) and (not Reader.CurrentLine(True).StartsWith('Start')) and (not Reader.LineBuffer.EndsWith('Class')) do
      Reader.NextLine(True);

    // Skip empty lines and exit if no more lines
    if (Reader.NextLine(True).IsEmpty) then
      Exit;

    // " 0001:00401000 000F47FCH .text                   CODE"
    while (not Reader.CurrentLine.IsEmpty) do
    begin
      var n := Pos(':', Reader.LineBuffer);

      var SegmentID: Cardinal := HexToInt16(Reader.LineBuffer, 0);

      var Offset: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);

      n := Pos(' ', Reader.LineBuffer, n+1);
      var Size: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);

      n := Pos('.', Reader.LineBuffer, n+1);
      var n2 := Pos(' ', Reader.LineBuffer, n+1);
      var Name := Copy(Reader.LineBuffer, n, n2-n);
      if (Name.IsEmpty) then
        Error(Reader.LineNumber, 'Invalid segment name'#13#10'%s', [Reader.LineBuffer]);

      var ClassName := Copy(Reader.LineBuffer, n2+1, MaxInt).Trim;
      if (ClassName.IsEmpty) then
        Error(Reader.LineNumber, 'Invalid segment class name'#13#10'%s', [Reader.LineBuffer]);

      var Segment := DebugInfo.Segments.Add(SegmentID, ClassName, Name);

      Segment.Offset := Offset;
      Segment.Size := Size;

      // We previously ignored empty segments. E.g.:
      //   "0005:00000000 00000000H .tls                    TLS"
      //   "0006:00400000 00000000H .pdata                  PDATA"
      // but we need to allow them so symbols or lines referencing the segments doesn't cause errors. E.g.:
      //   "0005:00000000       OtlCommon.Utils.LastThreadName"
      //   "0005:00000100       SysInit.TlsLast"
      if (Size = 0) then
        Warning(Reader.LineNumber, 'Empty segment: %s [%.4X:%.8X]', [Name, SegmentID, Segment.Offset]);

      Reader.NextLine;
    end;


    (*
    ** Modules
    *)

    Log('- Modules');
    // "Detailed map of segments"
    while (Reader.HasData) and (Reader.CurrentLine(True) <> 'Detailed map of segments') do
      Reader.NextLine(True);

    // Skip empty lines and exit if no more lines
    if (Reader.NextLine(True).IsEmpty) then
      Exit;

    // " 0001:00000000 0000F684 C=CODE     S=.text    G=(none)   M=System   ACBP=A9"
    while (not Reader.CurrentLine.IsEmpty) do
    begin
      var n := Pos(' C=', Reader.LineBuffer);
      var Address := Copy(Reader.LineBuffer, 1, n-1);

      n := Pos('M=', Reader.LineBuffer);
      var Name := Copy(Reader.LineBuffer, n+2, Pos(' ', Reader.LineBuffer, n+2)-n-2);
      if (Name.IsEmpty) then
        Error(Reader.LineNumber, 'Invalid module name'#13#10'%s', [Reader.LineBuffer]);

      var SegmentID: Cardinal := HexToInt16(Address, 0);

      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);

      n := Pos(' ', Address);
      var Size: TDebugInfoOffset := HexToInt32(Address, n);
      if (Size = 0) then
        Error(Reader.LineNumber, 'Invalid module size'#13#10'%s', [Reader.LineBuffer]);

      var Segment := DebugInfo.Segments.FindByIndex(SegmentID);
      if (Segment = nil) then
        Error(Reader.LineNumber, 'Unknown segment index: %.4X'#13#10'%s', [SegmentID, Reader.LineBuffer]);

      // Look for existing module
      var Module := DebugInfo.Modules.FindOverlap(Segment, Offset, Size);
      if (Module <> nil) then
        Error(Reader.LineNumber, 'Modules overlap: %s, %s'#13#10'%s', [Module.Name, Name, Reader.LineBuffer]);

      // Add new module
      if (Offset + Size <= Segment.Size) then
        DebugInfo.Modules.Add(Name, Segment, Offset, Size)
      else
        Warning(Reader.LineNumber, 'Module exceed segment bounds - ignored: %s [%.4X:%.8X+%d]', [Name, SegmentID, Offset, Size]);

      Reader.NextLine;
    end;


    (*
    ** Symbols - sorted by name
    *)

    Log('- Symbols');
    // "  Address             Publics by Name"
    while (Reader.HasData) and (not Reader.CurrentLine(True).EndsWith('Publics by Name')) do
      Reader.NextLine(True);

    // Skip empty lines and exit if no more lines
    if (Reader.NextLine(True).IsEmpty) then
      Exit;

    // " 0001:000E99AC       debug.info..TDebugInfo"
    while (not Reader.CurrentLine.IsEmpty) do
    begin
      var n := Pos(' ', Reader.LineBuffer);
      var Address := Copy(Reader.LineBuffer, 1, n-1);

      var Name := Copy(Reader.LineBuffer, n+1, MaxInt).TrimLeft;
      if (Name.IsEmpty) then
        Error(Reader.LineNumber, 'Invalid symbol name'#13#10'%s', [Reader.LineBuffer]);

      var SegmentID: Cardinal := HexToInt16(Address, 0);

      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);

      var Segment := DebugInfo.Segments.FindByIndex(SegmentID);
      if (Segment = nil) then
        Error(Reader.LineNumber, 'Unknown segment index: %.4X'#13#10'%s', [SegmentID, Reader.LineBuffer]);

      var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);

      if (Module <> nil) then
      begin

        Name := Demangle(Module, Name);

        if (Name <> '') then
        begin
          // Offset is relative to segment. Make it relative to module
          Dec(Offset, Module.Offset);

          var Symbol := Module.Symbols.Add(Name, Offset);
          if (Symbol = nil) then
            Warning(Reader.LineNumber, 'Symbol with duplicate offset ignored: [%.4X:%.8X] %s', [SegmentID, Offset, Name]);
        end;

      end else
        Warning(Reader.LineNumber, 'Failed to resolve symbol to module: [%.4X:%.8X] %s', [SegmentID, Offset, Name]);

      Reader.NextLine;
    end;

    // Once all symbols has been loaded we can calculate their size
    for var Module in DebugInfo.Modules do
    begin
      Module.Symbols.CalculateSizes;

      for var Symbol in Module.Symbols do
        if (Symbol.Size = 0) then
          Warning('Zero size symbol: %s.%s', [Module.Name, Symbol.Name]);
    end;


    (*
    ** Symbols - sorted by address
    *)

    // "  Address             Publics by Value"
    while (Reader.HasData) and (not Reader.CurrentLine(True).EndsWith('Publics by Value')) do
      Reader.NextLine(True);

    // Skip this section - it duplicates the previous

    (*
    ** Line numbers - grouped by module & segment
    *)

    Log('- Line numbers');
    // Rest of file is:
    // "Line numbers for System(WindowsAPIs.INC) segment .text"
    while (Reader.HasData) do
    begin
      const sPrefix = 'Line numbers for ';
      while (Reader.HasData) and (not Reader.CurrentLine(True).StartsWith(sPrefix)) do
        Reader.NextLine(True);

      if (not Reader.HasData) then
        break;

      var n := Pos('(', Reader.LineBuffer);
      if (n = 0) then
        Error(Reader.LineNumber, 'Source file start marker "(" not found'#13#10'%s', [Reader.LineBuffer]);
      var ModuleName := Copy(Reader.LineBuffer, Length(sPrefix)+1, n-1-Length(sPrefix));

      var n2 := Pos(')', Reader.LineBuffer, n+1);
      if (n2 = 0) then
        Error(Reader.LineNumber, 'Source file end marker ")" not found'#13#10'%s', [Reader.LineBuffer]);
      var Filename := Copy(Reader.LineBuffer, n+1, n2-n-1);

      n := Pos('segment', Reader.LineBuffer, n2+1);
      if (n = 0) then
        Error(Reader.LineNumber, 'Source file segment marker "segment" not found'#13#10'%s', [Reader.LineBuffer]);
      Inc(n, 7);
      while (Reader.LineBuffer[n] = ' ') do
        Inc(n);

      var SegmentName := Copy(Reader.LineBuffer, n, MaxInt);
      var Segment := DebugInfo.Segments.FindByName(SegmentName);
      if (Segment = nil) then
        Error(Reader.LineNumber, 'Unknown segment name: %s'#13#10'%s', [SegmentName, Reader.LineBuffer]);

      var Module := DebugInfo.Modules.FindByName(ModuleName, Segment);
      if (Module <> nil) then
      begin
        var SourceFile := Module.SourceFiles.Add(Filename);

        // Skip empty lines and exit if no more lines
        if (Reader.NextLine(True).IsEmpty) then
          break;

        // "   335 0001:00004068   338 0001:00004070   343 0001:00004078   349 0001:00004080"
        while (not Reader.CurrentLine.IsEmpty) do
        begin
          var Ofs := 1;

          while (Ofs <= Reader.LineBuffer.Length) do
          begin
            // Get line number
            n := Pos(' ', Reader.LineBuffer, Ofs);
            var s := Copy(Reader.LineBuffer, Ofs, n-Ofs);
            var LineNumber: integer;
            if (not integer.TryParse(s, LineNumber)) then
              Error(Reader.LineNumber, 'Invalid line number: %s'#13#10'%s', [s, Reader.LineBuffer]);

            // Get segment index (we already have that info from the header)
            var SegmentID: Cardinal := HexToInt16(Reader.LineBuffer, n);
            if (SegmentID <> Segment.Index) then
              Error(Reader.LineNumber, 'Segment mismatch. Module segment:%d (%s), Line segment:%d'#13#10'%s', [Segment.Index, Segment.Name, SegmentID, Reader.LineBuffer]);
            Inc(n, 4+1);

            // Get offset
            var Offset: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);
            Inc(n, 8+1);

            // Ignore line numbers with offset=0
            if (Offset <> 0) then
            begin
              if (Offset < Module.Offset) then
              begin
                Warning(Reader.LineNumber, 'Line number offset out of range for module. Offset:%.8X, Module:%s [%.8X - %.8X]', [Offset, Module.Name, Module.Offset, Module.Offset+Module.Size]);
              end else
              if (Offset < Module.Offset + Module.Size) then
              begin
                // Validate module
                var ModuleByOffset := DebugInfo.Modules.FindByOffset(Module.Segment, Offset);
                if (Module <> ModuleByOffset) then
                  Error(Reader.LineNumber, 'Module mismatch: Offset=%.8X, Module=%s, Found module:%s'#13#10'%s', [Offset, Module.Name, ModuleByOffset.Name, Reader.LineBuffer]);

                // Offset is relative to segment. Make it relative to module
                Dec(Offset, Module.Offset);
                Assert(Offset < Module.Size);

                Module.SourceLines.Add(SourceFile, LineNumber, Offset);
              end else
              begin
                // This is typically the last "end." of the unit. The offset corresponds to the start of the next module.

                // We can get *a lot* of these so I've disabled output of them for now
                // Warning(Reader.LineNumber, 'Line number offset out of range for module: Offset=%.8X, Module=%s', [Offset, Module.Name]);
              end;
            end else
              Warning(Reader.LineNumber, 'Line number with zero offset ignored. Module:%s, Segment:%.4X, Source:%s, Line:%d', [Module.Name, SegmentID, SourceFile.Filename, LineNumber]);

            while (n <= Reader.LineBuffer.Length) and (Reader.LineBuffer[n] = ' ') do
              Inc(n);

            Ofs := n;
          end;

          if (Reader.NextLine.IsEmpty) then
            break;
        end;
      end else
      begin
        // Ignore empty modules. E.g.:
        // Line numbers for System.RTLConsts(System.RTLConsts.pas) segment .text
        //   611 0001:00000000

        // Error(Reader.LineNumber, 'Module not found: %s'#13#10'%s', [ModuleName, Reader.LineBuffer]);

        // Skip empty lines and exit if no more lines
        if (Reader.NextLine(True).IsEmpty) then
          break;

        // Skip non-empty lines
        while (not Reader.CurrentLine.IsEmpty) do
        begin
          if (Reader.NextLine.IsEmpty) then
            break;
        end;
      end;

    end;

  finally
    Reader.Free;
  end;
end;

end.

