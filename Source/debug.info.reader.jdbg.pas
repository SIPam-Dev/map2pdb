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
  System.Classes,
  debug.info,
  debug.info.reader;

type
  TDebugInfoJdbgReader = class(TDebugInfoReader)
  private
    procedure LoadDebugInfo(DebugInfo: TDebugInfo; Stream: TMemoryStream);
  protected
  public
    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); override;
  end;



implementation

uses
  Generics.Collections,
  Generics.Defaults,
  Windows,
  SysUtils;


type
  TJDBGHeader = packed record
    Signature: DWORD;
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

procedure TDebugInfoJdbgReader.LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
begin
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

procedure TDebugInfoJdbgReader.LoadDebugInfo(DebugInfo: TDebugInfo; Stream: TMemoryStream);
type
  TSourceFragment = record
    SourceFile: TDebugInfoSourceFile;
    Offset: TDebugInfoOffset;
    Size: TDebugInfoOffset;
  end;

var
  SourceFragments: TList<TSourceFragment>;

  function MakePtr(A: Integer): Pointer;
  begin
    Result := Pointer(NativeInt(Stream.Memory) + NativeInt(A));
  end;

  function DataToStr(A: Integer): string;
  var
    P: PAnsiChar;
  begin
    if A = 0 then
      Result := ''
    else
    begin
      P := PAnsiChar(NativeInt(Stream.Memory) + NativeInt(A) + NativeInt(PJDBGHeader(Stream.Memory)^.Words) - 1);
      Result := DecodeNameString(P);
    end;
  end;

  procedure LoadModules(Segment: TDebugInfoSegment);
  begin
    Logger.Info('jdbg: Processing modules...');

    var Offset: TDebugInfoOffset := 0;
    var NameOffset: integer := 0;
    var i := 0;

    var P := MakePtr(PJDBGHeader(Stream.Memory)^.Units);

    var Size: integer;
    var DoContinue := ReadValue(P, Size);
    while DoContinue do
    begin
      Inc(i);
      Inc(Offset, Size);

      var NameDelta: integer;
      ReadValue(P, NameDelta);
      Inc(NameOffset, NameDelta);

      var Name := DataToStr(NameOffset);

      // Get the offset to the next module. This gives us the size of this one.
      DoContinue := ReadValue(P, Size);

      Logger.Debug('jdbg:   Module[%6d]: %.8X (Size:%.4X) %s', [i, Offset, Size, Name]);
      if (Size <> 0) then
        DebugInfo.Modules.Add(Name, Segment, Offset, Size)
      else
        Logger.Warning('[%6d] Empty module skipped: %s at %.8X', [i, Name, Offset]);

    end;
  end;

  procedure LoadSourceFiles(Segment: TDebugInfoSegment);
  begin
    Logger.Info('jdbg: Processing source files...');

    var Offset: TDebugInfoOffset := 0;
    var NameOffset: integer := 0;
    var i := 0;

    var P := MakePtr(PJDBGHeader(Stream.Memory)^.SourceNames);
    
    var Size: integer;
    var DoContinue := ReadValue(P, Size);
    while DoContinue do
    begin
      Inc(i);
      Inc(Offset, Size);

      var NameDelta: integer;
      ReadValue(P, NameDelta);
      Inc(NameOffset, NameDelta);

      var Name := DataToStr(NameOffset);

      // Get the offset to the next module. This gives us the size of this one.
      DoContinue := ReadValue(P, Size);

      var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);

      if (Module <> nil) then
      begin

        var SourceFile := Module.SourceFiles.Add(Name);

        // Offset is not relative to module because we need to use it to
        // map line numbers which are read with absolute offset.
        var SourceFragment: TSourceFragment;
        SourceFragment.SourceFile := SourceFile;
        SourceFragment.Offset := Offset;
        SourceFragment.Size := Size;
        SourceFragments.Add(SourceFragment);

        Logger.Debug('jdbg:   Source[%6d]: %.8X (Size:%.4X) %s, Module: %s', [i, Offset, Size, Name, Module.Name]);

      end else
        Logger.Warning('[%6d] Module not found at offset %.8X for source file: %s', [i, Offset, Name]);

    end;

    SourceFragments.Sort(
      IComparer<TSourceFragment>(
        function(const Left, Right: TSourceFragment): Integer
        begin
          Result := integer(Left.Offset) - integer(Right.Offset);
        end));

  end;

  procedure LoadLineNumbers(Segment: TDebugInfoSegment);
  begin
    Logger.Info('jdbg: Processing line numbers...');

    var Offset: TDebugInfoOffset := 0;
    var LineNumber: integer := 0;
    var i := 0;

    var P := MakePtr(PJDBGHeader(Stream.Memory)^.LineNumbers);

    var Delta: integer;
    var DoContinue := ReadValue(P, Delta);
    while DoContinue do
    begin
      Inc(i);
      Inc(Offset, Delta);

      var LineDelta: integer;
      ReadValue(P, LineDelta);
      Inc(LineNumber, LineDelta);

      DoContinue := ReadValue(P, Delta);

      var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);
      if (Module <> nil) then
      begin
        // Find the source file from the offset
        var SourceFile: TDebugInfoSourceFile := nil;
        // Binary search
        var L := 0;
        var H := SourceFragments.Count-1;

        while (L <= H) and (SourceFile = nil) do
        begin
          var mid := L + (H - L) shr 1;

          var SourceFragment := SourceFragments[mid];

          if (Offset < SourceFragment.Offset) then
            H := mid - 1
          else
          if (Offset >= SourceFragment.Offset+SourceFragment.Size) then
            L := mid + 1
          else
            SourceFile := SourceFragment.SourceFile;
        end;

        if (SourceFile <> nil) then
        begin
          Logger.Debug('jdbg:   Line numbers[%6d]: %6d at %.8X, Module/source: %s(%s)', [i, LineNumber, Offset, Module.Name, SourceFile.Filename]);
          // Offset is relative to segment. Make it relative to module
          var RelativeOffset := Offset - Module.Offset;
          Module.SourceLines.Add(SourceFile, LineNumber, RelativeOffset);
        end else
          Logger.Warning('[%6d] Failed to locate source file for line number %d at %.8X', [i, LineNumber, Offset]);
      end else
        Logger.Warning('[%6d] Failed to locate module for line number %d at %.8X', [i, LineNumber, Offset]);
    end;
  end;

  procedure LoadSymbols(Segment: TDebugInfoSegment);
  begin
    Logger.Info('jdbg: Processing symbols...');

    var Offset: TDebugInfoOffset := 0;
    var FirstWordOffset: integer := 0;
    var SecondWordOffset: integer := 0;
    var i := 0;

    var P := MakePtr(PJDBGHeader(Stream.Memory)^.Symbols);

    var Delta: integer;
    var DoContinue := ReadValue(P, Delta);
    while DoContinue do
    begin
      Inc(i);
      Inc(Offset, Delta);

      var NameDelta: integer;
      ReadValue(P, NameDelta);
      Inc(FirstWordOffset, NameDelta);
      ReadValue(P, NameDelta);
      Inc(SecondWordOffset, NameDelta);

      var Name := DataToStr(FirstWordOffset);
      if (SecondWordOffset <> 0) then
        Name := Name + '.' + DataToStr(SecondWordOffset);

      // Get the offset to the next symbol. This gives us the size of this one.
      DoContinue := ReadValue(P, Delta);

      var Module := DebugInfo.Modules.FindByOffset(Segment, Offset);
      if (Module <> nil) then
      begin
        Logger.Debug('jdbg:   Symbol[%6d]: %.8X (%.4X) %s, Module: %s', [i, Offset, Delta, Name, Module.Name]);
        // Offset is relative to segment. Make it relative to module
        var RelativeOffset := Offset - Module.Offset;
        var Symbol := Module.Symbols.Add(Name, RelativeOffset);

        // We assume that symbols are ordered by offset. Otherwise the size will be wrong.
        Symbol.Size := Delta;
      end else
        Logger.Warning('[%6d] Failed to locate module for symbol %s at %.8X', [i, Name, Offset]);
    end;
  end;

begin
  Logger.Debug('jdbg: Synthesizing .text segment');
  var Segment := DebugInfo.Segments.Add(1, '.text', sctCODE);
  Segment.Offset := $00000000;
  Segment.Size := 0;

  SourceFragments := TList<TSourceFragment>.Create;
  try

    LoadModules(Segment);
    LoadSourceFiles(Segment);
    LoadLineNumbers(Segment);
    LoadSymbols(Segment);

  finally
    SourceFragments.Free;
  end;

//  var Segment := DebugInfo.SegmentClasses.Add(2, sctDATA, 'FOOBAR');
//  Segment.Offset := $00171717;
//  Segment.Size := $00272727;

  var Module := DebugInfo.Modules.Add('modmodmodmodmod', Segment, $121212, $202020);
  Module.ObjectName := 'objobjobjob.obj';

  var SourceFile := Module.SourceFiles.Add('foofoofoofo.pas');
  Module.SourceLines.Add(SourceFile, $0077, $0000);
  Module.SourceLines.Add(SourceFile, $0099, $0011);

  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');
  Module.SourceLines.Add(SourceFile, $0066, $0022);

  SourceFile := Module.SourceFiles.Add('barbarbarba.pas');
  Module.SourceLines.Add(SourceFile, $0044, $0033);
  Module.SourceLines.Add(SourceFile, $0055, $0044);



  Module := DebugInfo.Modules.Add('zzzzzzzzzzzzzzz', Segment, $101010, $202020);

  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');
  Module.SourceLines.Add(SourceFile, $1111, $0101);


  Module.Symbols.Add('aaaaaaaa', $0f0f0f);
//  Module.Symbols.Add('bbbbbbbb', $200);

//  Module.ObjectName := 'objobjobjob.obj';
//  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');

end;


end.

