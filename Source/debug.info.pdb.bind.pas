unit debug.info.pdb.bind;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

procedure PatchPE(const Filename, PdbFilename: string; Logging: boolean = False);

implementation

uses
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  debug.info.codeview,
  debug.info.pdb;

procedure PatchPE(const Filename, PdbFilename: string; Logging: boolean = False);

  procedure Log(const Msg: string);
  begin
    if (Logging) then
      Writeln(Msg);
  end;

  procedure Error(const Msg: string); overload;
  begin
    Writeln(Msg);
    Halt(1);
  end;

  procedure Error(const Fmt: string; const Args: array of const); overload;
  begin
    Error(Format(Fmt, Args));
  end;

type
  TByteArray = array[0..MaxInt-1] of Byte;
  PByteArray = ^TByteArray;
begin
  Log('Patching PE file');

  var Stream := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);
  try
    (*
    ** DOS header
    *)
    var DosHeader: TImageDosHeader;
    Stream.ReadBuffer(DosHeader, SizeOf(DosHeader));

    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      Error('Invalid DOS file signature');


    (*
    ** NT header
    *)
    // The DOS header gives us the offset to the NT header
    Stream.Seek(DosHeader._lfanew, soBeginning);

    var Signature: DWORD;
    var FileHeader: TImageFileHeader;
    Stream.ReadBuffer(Signature, SizeOf(Signature));
    Stream.ReadBuffer(FileHeader, SizeOf(FileHeader));


    // After the Signature and FileHeader follows a structure that differs
    // between PE32 and PE32+.
    // The first Word of the structure tells us what kind it is: Either
    // TImageOptionalHeader32 or TImageOptionalHeader64.
    var OptionalHeader32: TImageOptionalHeader32;
    var OptionalHeader64: TImageOptionalHeader64;
    Stream.ReadBuffer(OptionalHeader32.Magic, SizeOf(OptionalHeader32.Magic));

    var PE32Plus := False;

    case OptionalHeader32.Magic of
      IMAGE_NT_OPTIONAL_HDR32_MAGIC:
        begin
          Log('- PE32 image (32-bit)');
          PE32Plus := False;
          Stream.ReadBuffer(OptionalHeader32.MajorLinkerVersion, SizeOf(TImageOptionalHeader32)-SizeOf(Word));
        end;

      IMAGE_NT_OPTIONAL_HDR64_MAGIC:
        begin
          Log('- PE32+ image (64-bit)');
          PE32Plus := True;
          OptionalHeader64.Magic := OptionalHeader32.Magic;
          Stream.ReadBuffer(OptionalHeader64.MajorLinkerVersion, SizeOf(TImageOptionalHeader64)-SizeOf(Word));
        end;
    else
      Error('Invalid or unsupprted PE image type: %.4X', [OptionalHeader32.Magic]);
    end;

    var DebugRVA: Cardinal;
    if (PE32Plus) then
      DebugRVA := OptionalHeader64.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress
    else
      DebugRVA := OptionalHeader32.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;

    if (DebugRVA = 0) then
      Error('Image does not contain a debug directory address - please link with debug info enabled');

    if (PE32Plus) then
    begin
      if (OptionalHeader64.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size = 0) then
        Error('Image does not contain a debug directory - please link with debug info enabled');
    end else
    begin
      if (OptionalHeader32.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size = 0) then
        Error('Image does not contain a debug directory - please link with debug info enabled');
    end;


    (*
    ** Section table
    *)
    var DebugOffset := 0;

    // Read through section headers to locate the debug section header
    for var i := 0 to FileHeader.NumberOfSections-1 do
    begin
      var SectionHeader: TImageSectionHeader;
      Stream.ReadBuffer(SectionHeader, SizeOf(SectionHeader));

      if (DebugRVA >= SectionHeader.VirtualAddress) and (DebugRVA < SectionHeader.VirtualAddress+SectionHeader.Misc.VirtualSize) then
      begin
        DebugOffset := DebugRVA - SectionHeader.VirtualAddress + SectionHeader.PointerToRawData;
        break;
      end;
    end;

    if (DebugOffset = 0) then
      Error('Failed to locate debug directory section - please link with debug info enabled');


    (*
    ** Debug directory
    *)
    Stream.Seek(DebugOffset, soBeginning);

    // Get the filename as UTF-8
    var Bytes := TEncoding.UTF8.GetBytes(PdbFilename);

    // Calculate size of block, including the filename and terminating zero
    var DebugSize: Cardinal := SizeOf(TCodeViewInfoPDB70) + Length(Bytes);

    // Even if the debug directory contains multiple entries we only mess with the first
    var DebugDirectory: TImageDebugDirectory;
    Stream.ReadBuffer(DebugDirectory, SizeOf(DebugDirectory));

    if (DebugDirectory.SizeOfData < DebugSize) then
      Error('Size of debug data too small: %d bytes available, %d required', [DebugDirectory.SizeOfData, DebugSize]);


    (*
    ** Debug data
    *)
    if (DebugDirectory._Type <> IMAGE_DEBUG_TYPE_CODEVIEW) then
    begin
      DebugDirectory._Type := IMAGE_DEBUG_TYPE_CODEVIEW;

      // Write updated debug directory
      Stream.Seek(DebugOffset, soBeginning);
      Stream.WriteBuffer(DebugDirectory, SizeOf(DebugDirectory));
      Log('- Type of debug directory entry has been set to IMAGE_DEBUG_TYPE_CODEVIEW.');
    end;

    // Populate and write a CV_INFO_PDB70 block
    var CodeViewInfoPDB: PCodeViewInfoPDB70;
    GetMem(CodeViewInfoPDB, DebugSize);
    try

      CodeViewInfoPDB.CvSignature := $53445352; // RSDS
      CodeViewInfoPDB.Signature := PdbBuildSignature; // GUID - must be the same as the one in the PDB
      CodeViewInfoPDB.Age := PdbBuildAge; // Generation counter - must be same value as the one in the PDB
      Move(Bytes[0], CodeViewInfoPDB.PdbFileName, Length(Bytes));
      PByteArray(@CodeViewInfoPDB^.PdbFileName)^[Length(Bytes)] := 0;

      // Write updated debug data
      Stream.Seek(DebugDirectory.PointerToRawData, soBeginning);
      Stream.WriteBuffer(CodeViewInfoPDB^, DebugSize);
      Log('- PDB file name has been stored in debug data.');

    finally
      FreeMem(CodeViewInfoPDB);
    end;


    if (PE32Plus) then
    begin
      if (OptionalHeader64.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size < SizeOf(TImageDebugDirectory)) then
      begin
        OptionalHeader64.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := SizeOf(TImageDebugDirectory);
        Log('- Debug directory size has been reset to correct value.');
      end;

      // Clear the checksum so file doesn't appear corrupt
      OptionalHeader64.CheckSum := 0;
      Log('- Header checksum has been cleared.');
    end else
    begin
      if (OptionalHeader32.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size < SizeOf(TImageDebugDirectory)) then
      begin
        OptionalHeader32.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := SizeOf(TImageDebugDirectory);
        Log('- Debug directory size has been reset to correct value.');
      end;

      // Clear the checksum so file doesn't appear corrupt
      OptionalHeader32.CheckSum := 0;
      Log('- Header checksum has been cleared.');
    end;


    // Write updated NT header
    Stream.Seek(DosHeader._lfanew + SizeOf(Signature) + SizeOf(FileHeader), soBeginning);
    if (PE32Plus) then
      Stream.WriteBuffer(OptionalHeader64, SizeOf(TImageOptionalHeader64))
    else
      Stream.WriteBuffer(OptionalHeader32, SizeOf(TImageOptionalHeader32));

    Log('- PE file has been updated.');
  finally
    Stream.Free;
  end;
end;

end.

