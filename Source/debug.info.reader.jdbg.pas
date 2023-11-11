unit debug.info.reader.jdbg;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Generics.Collections,
  System.Classes,
  debug.info,
  debug.info.reader;

type
  TJDBGHeader = packed record
    Signature: Cardinal;
    Version: Byte;
    Units: Integer;
    SourceNames: Integer;
    Symbols: Integer;
    LineNumbers: Integer;
    Words: Integer;
    ModuleName: Integer;
    CheckSum: Integer;
    CheckSumValid: Boolean;
  end;
  PJDBGHeader = ^TJDBGHeader;

// -----------------------------------------------------------------------------
//
//      TDebugInfoJdbgReader
//
// -----------------------------------------------------------------------------
// Debug info reader for JDBG files
// -----------------------------------------------------------------------------
type
  TDebugInfoJdbgReader = class(TDebugInfoReader)
  strict private type
    TSourceFragment = record
      SourceFile: TDebugInfoSourceFile;
      StartOffset: TDebugInfoOffset;
      EndOffset: TDebugInfoOffset;
    end;

  strict private
    FSourceFragments: TList<TSourceFragment>;
  private
    class function ReadString(const Header: PJDBGHeader; Offset: Cardinal): string; static;

    procedure LoadModules(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
    procedure LoadSourceFiles(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
    procedure LoadLineNumbers(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
    procedure LoadSymbols(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);

    procedure LoadDebugInfo(DebugInfo: TDebugInfo; Stream: TMemoryStream);
  protected
  public
    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); override;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Generics.Defaults,
  System.Math,
  System.SysUtils,
  debug.info.reader.map;


// JCL binary debug format string encoding/decoding routines
{ Strings are compressed to following 6bit format (A..D represents characters) and terminated with }
{ 6bit #0 char. First char = #1 indicates non compressed text, #2 indicates compressed text with   }
{ leading '@' character                                                                            }
{                                                                                                  }
{ 7   6   5   4   3   2   1   0  |                                                                 }
{---------------------------------                                                                 }
{ B1  B0  A5  A4  A3  A2  A1  A0 | Data byte 0                                                     }
{---------------------------------                                                                 }
{ C3  C2  C1  C0  B5  B4  B3  B2 | Data byte 1                                                     }
{---------------------------------                                                                 }
{ D5  D4  D3  D2  D1  D0  C5  C4 | Data byte 2                                                     }
{---------------------------------                                                                 }

function SimpleCryptString(const S: UTF8String): UTF8String;
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  SetLength(Result, Length(S));
  P := PByte(Result);
  for I := 1 to Length(S) do
  begin
    C := Ord(S[I]);
    if C <> $AA then
      C := C xor $AA;
    P^ := C;
    Inc(P);
  end;
end;

// -----------------------------------------------------------------------------

function DecodeNameString(const S: PAnsiChar): string;
var
  I, B: Integer;
  C: Byte;
  P: PByte;
  Buffer: array [0..255] of AnsiChar;
begin
  Result := '';
  B := 0;
  P := PByte(S);
  case P^ of
    1:
      begin
        Inc(P);
        Result := string(SimpleCryptString(PAnsiChar(P)));
        Exit;
      end;
    2:
      begin
        Inc(P);
        Buffer[B] := '@';
        Inc(B);
      end;
  end;
  I := 0;
  C := 0;
  repeat
    case I and $03 of
      0:
        C := P^ and $3F;
      1:
        begin
          C := (P^ shr 6) and $03;
          Inc(P);
          Inc(C, (P^ and $0F) shl 2);
        end;
      2:
        begin
          C := (P^ shr 4) and $0F;
          Inc(P);
          Inc(C, (P^ and $03) shl 4);
        end;
      3:
        begin
          C := (P^ shr 2) and $3F;
          Inc(P);
        end;
    end;
    case C of
      $00:
        Break;
      $01..$0A:
        Inc(C, Ord('0') - $01);
      $0B..$24:
        Inc(C, Ord('A') - $0B);
      $25..$3E:
        Inc(C, Ord('a') - $25);
      $3F:
        C := Ord('_');
    end;
    Buffer[B] := AnsiChar(C);
    Inc(B);
    Inc(I);
  until B >= SizeOf(Buffer) - 1;
  Buffer[B] := #0;
  Result := UTF8ToString(@Buffer[0]);
end;

// -----------------------------------------------------------------------------

function ReadValue(var P: Pointer; var Value: Integer): Boolean;
var
  N: Integer;
  I: Integer;
  B: Byte;
begin
  N := 0;
  I := 0;
  repeat
    B := PByte(P)^;
    Inc(PByte(P));
    Inc(N, (B and $7F) shl I);
    Inc(I, 7);
  until B and $80 = 0;
  Value := N;
  Result := (N <> MaxInt);
end;


// -----------------------------------------------------------------------------
//
//      TDebugInfoJdbgReader
//
// -----------------------------------------------------------------------------
class function TDebugInfoJdbgReader.ReadString(const Header: PJDBGHeader; Offset: Cardinal): string;
begin
  if (Offset = 0) then
    Result := ''
  else
  begin
    var p := PAnsiChar(PByte(Header) + Header.Words + Offset - 1);
    Result := DecodeNameString(p);
  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
begin
  Logger.Info('Reading JDBG file');

  var OwnedStream: TMemoryStream := nil;
  try

    var MemoryStream: TMemoryStream;

    if (not (Stream is TMemoryStream)) or (Stream.Position <> 0) then
    begin
      OwnedStream := TMemoryStream.Create;
      MemoryStream := OwnedStream;

      MemoryStream.CopyFrom(Stream, Stream.Size - Stream.Position); // Specify size so we can support loading from Pos>0
      MemoryStream.Position := 0;
    end else
      MemoryStream := TMemoryStream(Stream);

    LoadDebugInfo(DebugInfo, MemoryStream);

  finally
    OwnedStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadModules(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
begin
  Logger.Info('- Modules');

  var Offset: TDebugInfoOffset := 0;
  var NameOffset: integer := 0;
  var i := 0;

  var Data: pointer := PByte(Header) + Header.Units;

  var Size: integer;
  var DoContinue := ReadValue(Data, Size);
  while DoContinue do
  begin
    Inc(i);
    Inc(Offset, Size);

    var NameDelta: integer;
    ReadValue(Data, NameDelta);
    Inc(NameOffset, NameDelta);

    var Name := ReadString(Header, NameOffset);

    // Get the offset to the next module. This gives us the size of this one.
    DoContinue := ReadValue(Data, Size);

    Logger.Debug('  Module[%6d]: %.8X (Size:%.4X) %s', [i, Offset, Size, Name]);

    if (not DoContinue) and (Size = MaxInt) then
      Logger.Warning('[%6d] Filler module skipped: %s at %.8X', [i, Name, Offset])
    else
    if (Size > 0) then
    begin
      // Look for existing module
      var Module := DebugInfo.Modules.FindOverlap(Segment, Offset, Size);
      if (Module <> nil) then
        Logger.Error('[%6d] Modules overlap: %s and %s', [i, Module.Name, Name]);

      DebugInfo.Modules.Add(Name, Segment, Offset, Size);
    end else
      Logger.Warning('[%6d] Empty module skipped: %s at %.8X', [i, Name, Offset]);

  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadSourceFiles(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
begin
  Logger.Info('- Source files');

  var Offset: TDebugInfoOffset := 0;
  var NameOffset: integer := 0;
  var i := 0;

  var Data: pointer := PByte(Header) + Header.SourceNames;

  var Size: integer;
  var DoContinue := ReadValue(Data, Size);
  while DoContinue do
  begin
    Inc(i);
    Inc(Offset, Size);

    var NameDelta: integer;
    ReadValue(Data, NameDelta);
    Inc(NameOffset, NameDelta);

    var Name := ReadString(Header, NameOffset);

    // Get the offset to the next module. This gives us the size of this one.
    DoContinue := ReadValue(Data, Size);

    var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);

    if (Module <> nil) then
    begin
      Logger.Debug('  Source[%6d]: %.8X (Size:%.4X) %s, Module: %s', [i, Offset, Size, Name, Module.Name]);

      if (Size > 0) then
      begin
        var FragmentSize := Size;
        if (Offset+Cardinal(FragmentSize) > Module.Offset+Module.Size) then
        begin
          FragmentSize := Module.Offset+Module.Size - Offset;
          // This problem is so common that we're logging it at debug level instead of warning level
          Logger.Debug('[%6d] Source fragment at %.8X (Size:%.4X) exceeds module %s at %.8X (Size:%.4X) for source file %s. Truncated to %.4X', [i, Offset, Size, Module.Name, Module.Offset, Module.Size, Name, FragmentSize]);
        end;

        var SourceFile := Module.SourceFiles.Add(Name);

        var SourceFragment: TSourceFragment;
        SourceFragment.SourceFile := SourceFile;
        // Offset is not relative to module because we need later on to
        // map line numbers (which are read with absolute offset) to
        // source files.
        SourceFragment.StartOffset := Offset;
        SourceFragment.EndOffset := Offset+Cardinal(FragmentSize)-1;
        FSourceFragments.Add(SourceFragment);
      end else
        Logger.Warning('[%6d] Zero size source file ignored: %s at %.8X', [i, Name, Offset, Name]);

    end else
      Logger.Warning('[%6d] Module not found at offset %.8X for source file: %s', [i, Offset, Name]);

  end;

  // Sort source fragments by offset.
  // We will use them later to map line number by offset to source file
  FSourceFragments.Sort(
    IComparer<TSourceFragment>(
      function(const Left, Right: TSourceFragment): Integer
      begin
        Result := integer(Left.StartOffset) - integer(Right.StartOffset);
        Assert((Result <> 0) or (Left.SourceFile = Right.SourceFile));
      end));

  // Check for duplicate or overlapping fragments
  var PreviousSourceFragment: TSourceFragment := Default(TSourceFragment);
  var First := True;
  for var SourceFragmentX in FSourceFragments do
  begin
    if (not First) then
    begin
      if (SourceFragmentX.StartOffset <= PreviousSourceFragment.EndOffset) then
        Logger.Warning('Overlapping source file fragments: %s at %.8X-%.8X and %s at %.8X-%.8X',
          [SourceFragmentX.SourceFile.Filename, SourceFragmentX.StartOffset, SourceFragmentX.EndOffset,
           PreviousSourceFragment.SourceFile.Filename, PreviousSourceFragment.StartOffset, PreviousSourceFragment.EndOffset]);
    end;

    PreviousSourceFragment := SourceFragmentX;
    First := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadLineNumbers(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
begin
  Logger.Info('- Line numbers');

  var Offset: TDebugInfoOffset := 0;
  var LineNumber: integer := 0;
  var i := 0;

  var Data: pointer := PByte(Header) + Header.LineNumbers;

  var Delta: integer;
  var DoContinue := ReadValue(Data, Delta);
  while DoContinue do
  begin
    Inc(i);
    Inc(Offset, Delta);

    var LineDelta: integer;
    ReadValue(Data, LineDelta);
    Inc(LineNumber, LineDelta);

    DoContinue := ReadValue(Data, Delta);

    var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);
    if (Module <> nil) then
    begin
      // Find the source file from the offset
      var SourceFile: TDebugInfoSourceFile := nil;
      // Binary search
      var L := 0;
      var H := FSourceFragments.Count-1;

      while (L <= H) and (SourceFile = nil) do
      begin
        var mid := L + (H - L) shr 1;

        var SourceFragment := FSourceFragments[mid];

        if (Offset < SourceFragment.StartOffset) then
          H := mid - 1
        else
        if (Offset > SourceFragment.EndOffset) then
          L := mid + 1
        else
          SourceFile := SourceFragment.SourceFile;
      end;

      if (SourceFile <> nil) then
      begin
        Logger.Debug('  Line number[%6d]: %6d at %.8X, Module/source: %s(%s)', [i, LineNumber, Offset, Module.Name, SourceFile.Filename]);

        if (Module.SourceFiles.Contains(SourceFile)) then
        begin
          // Offset is relative to segment. Make it relative to module
          var RelativeOffset := Offset - Module.Offset;
          Module.SourceLines.Add(SourceFile, LineNumber, RelativeOffset);
        end else
          Logger.Warning('[%6d] Module and source file mismatched for line number %d at %8X. Module: %s, Source: %s ', [i, LineNumber, Offset, Module.Name, SourceFile.Filename]);
      end else
        Logger.Warning('[%6d] Failed to locate source file for line number %d at %.8X, module: %s', [i, LineNumber, Offset, Module.Name]);
    end else
      Logger.Warning('[%6d] Failed to locate module for line number %d at %.8X', [i, LineNumber, Offset]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadSymbols(const Header: PJDBGHeader; DebugInfo: TDebugInfo; Segment: TDebugInfoSegment);
begin
  Logger.Info('- Symbols');

  var Offset: TDebugInfoOffset := 0;
  var FirstWordOffset: integer := 0;
  var SecondWordOffset: integer := 0;
  var i := 0;

  var Data: pointer := PByte(Header) + Header.Symbols;

  var Delta: integer;
  var DoContinue := ReadValue(Data, Delta);
  while DoContinue do
  begin
    Inc(i);
    Inc(Offset, Delta);

    var NameDelta: integer;
    ReadValue(Data, NameDelta);
    Inc(FirstWordOffset, NameDelta);
    ReadValue(Data, NameDelta);
    Inc(SecondWordOffset, NameDelta);

    var Name := ReadString(Header, FirstWordOffset);
    if (SecondWordOffset <> 0) then
      Name := Name + '.' + ReadString(Header, SecondWordOffset);

    // Get the offset to the next symbol. This gives us the size of this one.
    DoContinue := ReadValue(Data, Delta);
    Assert(Delta >= 0, 'Symbol with negative size');

    var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);
    if (Module <> nil) then
    begin
      Name := DemangleMapSymbol(Module, Name);
      Logger.Debug('  Symbol[%6d]: %.8X (%.4X) %s, Module: %s', [i, Offset, Delta, Name, Module.Name]);

      // Offset is relative to segment. Make it relative to module
      var RelativeOffset := Offset - Module.Offset;
      var Symbol := Module.Symbols.Add(Name, RelativeOffset);
      if (Symbol <> nil) then
      begin
        // This problem is so common that we're logging it at debug level instead of warning level
        var SymbolSize := Delta;
        if (Symbol.Offset + Cardinal(Delta) > Module.Size) then
        begin
          SymbolSize := Module.Size - Symbol.Offset;
          Logger.Debug('[%6d] Symbol %s at %.8X (Size:%.4X) exceeds module %s at %.8X (Size:%.4X). Truncated to %.4X', [i, Name, Offset, Delta, Module.Name, Module.Offset, Module.Size, SymbolSize]);
        end;

        // We assume that symbols are ordered by offset. Otherwise the size will be wrong.
        Symbol.Size := SymbolSize;
      end else
        Logger.Warning('[%6d] Symbol with duplicate offset ignored: [%.8X] %s', [i, Offset, Name]);

    end else
      Logger.Warning('[%6d] Failed to locate module for symbol %s at %.8X', [i, Name, Offset]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TDebugInfoJdbgReader.LoadDebugInfo(DebugInfo: TDebugInfo; Stream: TMemoryStream);
begin
  Logger.Debug('- Synthesizing .text segment');
  var Segment := DebugInfo.Segments.Add(1, '.text', sctCODE);
  Segment.Offset := $00000000;
  Segment.Size := 0;

  FSourceFragments := TList<TSourceFragment>.Create;
  try
    var Header := PJDBGHeader(Stream.Memory);

    LoadModules(Header, DebugInfo, Segment);
    LoadSourceFiles(Header, DebugInfo, Segment);
    LoadLineNumbers(Header, DebugInfo, Segment);
    LoadSymbols(Header, DebugInfo, Segment);

  finally
    FSourceFragments.Free;
  end;

  // Determine max size of segment
  var MaxSegmentSize := Segment.Size;
  for var Module in DebugInfo.Modules do
  begin
    // Determine max size of module
    Module.CalculateSize;

    MaxSegmentSize := Max(MaxSegmentSize, Module.Offset + Module.Size);
  end;
  Segment.Size := MaxSegmentSize;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.

