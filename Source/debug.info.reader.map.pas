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
  Classes,
  debug.info;

type
  TDebugInfoMapReader = class
  protected
    function Demangle(Module: TDebugInfoModule; const Name: string): string;
  public
    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
    procedure LoadFromFile(const Filename: string; DebugInfo: TDebugInfo);
  end;

implementation

uses
  SysUtils;

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

procedure TDebugInfoMapReader.LoadFromFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  var Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try

    LoadFromStream(Stream, DebugInfo);

  finally
    Stream.Free;
  end;
end;

procedure TDebugInfoMapReader.LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
begin
  var Reader := TLineReader.Create(Stream);
  try

    (*
    ** Segments
    *)

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

      var SegmentID: TDebugInfoSegment := HexToInt16(Reader.LineBuffer, 0);

      var Offset: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);

      n := Pos(' ', Reader.LineBuffer, n+1);
      var Size: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);

      // Ignore empty segments
      if (Size > 0) then
      begin

        n := Pos('.', Reader.LineBuffer, n+1);
        var n2 := Pos(' ', Reader.LineBuffer, n+1);
        var Name := Copy(Reader.LineBuffer, n, n2-n);
        if (Name.IsEmpty) then
          raise Exception.CreateFmt('[%d] Invalid segment name (%s)', [Reader.LineNumber, Reader.LineBuffer]);

        var ClassName := Copy(Reader.LineBuffer, n2+1, MaxInt).Trim;
        if (ClassName.IsEmpty) then
          raise Exception.CreateFmt('[%d] Invalid segment class name (%s)', [Reader.LineNumber, Reader.LineBuffer]);

        var SegmentClass := DebugInfo.SegmentClasses.Add(SegmentID, ClassName, Name);

        SegmentClass.Offset := Offset;
        SegmentClass.Size := Size;

      end;
        //raise Exception.CreateFmt('[%d] Invalid segment size (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      Reader.NextLine;
    end;


    (*
    ** Modules
    *)

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
        raise Exception.CreateFmt('[%d] Invalid module name (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      var Segment: TDebugInfoSegment := HexToInt16(Address, 0);

      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);

      n := Pos(' ', Address);
      var Size: TDebugInfoOffset := HexToInt32(Address, n);
      if (Size = 0) then
        raise Exception.CreateFmt('[%d] Invalid module size (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      var SegmentClass := DebugInfo.SegmentClasses.FindByValue(Segment);
      if (SegmentClass = nil) then
        raise Exception.CreateFmt('[%d] Unknown segment (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      // Look for existing module
      var Module := DebugInfo.Modules.FindByOffset(SegmentClass, Offset);
      if (Module <> nil) then
        raise Exception.CreateFmt('[%d] Module offset collision (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      Module := DebugInfo.Modules.FindByOffset(SegmentClass, Offset+Size-1);
      if (Module <> nil) then
        raise Exception.CreateFmt('[%d] Modules overlap: %s, %s (%s)', [Reader.LineNumber, Module.Name, Name, Reader.LineBuffer]);

      // Add new module
      DebugInfo.Modules.Add(Name, SegmentClass, Offset, Size);

      Reader.NextLine;
    end;


    (*
    ** Symbols - sorted by name
    *)

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
        raise Exception.CreateFmt('[%d] Invalid symbol name (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      var Segment: TDebugInfoSegment := HexToInt16(Address, 0);

      n := Pos(':', Address);
      var Offset: TDebugInfoOffset := HexToInt32(Address, n);

      var SegmentClass := DebugInfo.SegmentClasses.FindByValue(Segment);
      if (SegmentClass = nil) then
        raise Exception.CreateFmt('[%d] Unknown segment (%s)', [Reader.LineNumber, Reader.LineBuffer]);

      var Module := DebugInfo.Modules.FindByOffset(SegmentClass, Offset);

      if (Module <> nil) then
      begin

        Name := Demangle(Module, Name);

        if (Name <> '') then
        begin
          // Offset is relative to module
          Dec(Offset, Module.Offset);

          var Symbol := Module.Symbols.Add(Name, Offset);
          if (Symbol = nil) then
            WriteLn(Format('[%d] Symbol with duplicate offset ignored: %.4X:%.8X %s', [Reader.LineNumber, Segment, Offset, Name]));
        end;

      end else
        WriteLn(Format('[%d] Failed to resolve symbol to module: %.4X:%.8X %s', [Reader.LineNumber, Segment, Offset, Name]));

      Reader.NextLine;
    end;

    // Once all symbols has been loaded we can calculate their size
    for var Module in DebugInfo.Modules do
    begin
      Module.Symbols.CalculateSizes;

      for var Symbol in Module.Symbols do
        if (Symbol.Size = 0) then
          WriteLn(Format('Zero size symbol will be ignored: %s.%s', [Module.Name, Symbol.Name]));
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
        raise Exception.CreateFmt('[%d] Source file start marker "(" not found: %s', [Reader.LineNumber, Reader.LineBuffer]);
      var ModuleName := Copy(Reader.LineBuffer, Length(sPrefix)+1, n-1-Length(sPrefix));

      var n2 := Pos(')', Reader.LineBuffer, n+1);
      if (n2 = 0) then
        raise Exception.CreateFmt('[%d] Source file end marker ")" not found: %s', [Reader.LineNumber, Reader.LineBuffer]);
      var Filename := Copy(Reader.LineBuffer, n+1, n2-n-1);

      n := Pos('segment', Reader.LineBuffer, n2+1);
      if (n = 0) then
        raise Exception.CreateFmt('[%d] Source file segment marker "segment" not found: %s', [Reader.LineNumber, Reader.LineBuffer]);
      Inc(n, 7);
      while (Reader.LineBuffer[n] = ' ') do
        Inc(n);

      var SegmentName := Copy(Reader.LineBuffer, n, MaxInt);
      var SegmentClass := DebugInfo.SegmentClasses.FindByName(SegmentName);
      if (SegmentClass = nil) then
        raise Exception.CreateFmt('[%d] Unknown segment name: %s (%s)', [Reader.LineNumber, SegmentName, Reader.LineBuffer]);

      var Module := DebugInfo.Modules.FindByName(ModuleName, SegmentClass);
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
              raise Exception.CreateFmt('[%d] Invalid line number: %s (%s)', [Reader.LineNumber, s, Reader.LineBuffer]);

            // Get segment type (we already have that info from the header)
            // var Segment: TDebugInfoSegment := HexToInt16(Reader.LineBuffer, n);
            Inc(n, 4+1);

            // Get offset
            var Offset: TDebugInfoOffset := HexToInt32(Reader.LineBuffer, n);
            Inc(n, 8+1);

            // Ignore line numbers with offset=0
            if (Offset <> 0) then
            begin
              if (Offset < Module.Offset) then
                raise Exception.CreateFmt('[%d] Line number offset out of range for module: Offset=%.8X, Module=%s (%s)', [Reader.LineNumber, Offset, Module.Name, Reader.LineBuffer]);

              if (Offset < Module.Offset + Module.Size) then
              begin
                // Validate module
                var ModuleByOffset := DebugInfo.Modules.FindByOffset(Module.SegmentClass, Offset);
                if (Module <> ModuleByOffset) then
                  raise Exception.CreateFmt('[%d] Module mismatch: Offset=%.8X, Module=%s, Found module:%s (%s)', [Reader.LineNumber, Offset, Module.Name, ModuleByOffset.Name, Reader.LineBuffer]);

                // Offset is relative to module
                Dec(Offset, Module.Offset);

                Module.SourceLines.Add(SourceFile, LineNumber, Offset);
              end else
              begin
                // This is typically the last "end." of the unit. The offset corresponds to the start of the next module.

                // We can get *a lot* of these so I've disabled output of them for now
                // WriteLn(Format('[%d] Line number offset out of range for module: Offset=%.8X, Module=%s', [Reader.LineNumber, Offset, Module.Name]));
              end;
            end;

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

        // raise Exception.CreateFmt('[%d] Module not found: %s (%s)', [Reader.LineNumber, ModuleName, Reader.LineBuffer]);

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
    FreeAndNil(Reader);
  end;
end;

end.

