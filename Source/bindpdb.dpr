program bindpdb;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  Windows,
  IOUtils;

type
  CV_INFO_PDB70 = record
    CvSignature: DWORD;
    Signature: TGUID;
    Age: DWORD;
    PdbFileName: array[0..0] of BYTE;
  end;
  TCodeViewInfoPDB70 = CV_INFO_PDB70;
  PCodeViewInfoPDB70 = ^TCodeViewInfoPDB70;

const
  Signature: TGUID = '{CBB17264-89FA-4AED-A2D7-814EE276EF3E}';

procedure PatchFile(const Filename, PdbFilename: string);
begin
  var Stream := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);
  try
    (*
    ** DOS header
    *)
    var DosHeader: TImageDosHeader;
    Stream.ReadBuffer(DosHeader, SizeOf(DosHeader));

    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      raise Exception.Create('Invalid DOS file signature');

    (*
    ** NT header
    *)
    // The DOS header gives us the offset to the NT header
    Stream.Seek(DosHeader._lfanew, soBeginning);

    var NtHeaders32: TImageNtHeaders32;
    Stream.ReadBuffer(NtHeaders32, SizeOf(NtHeaders32));

    if (NtHeaders32.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC) then
      raise Exception.Create('PE64 image not supported yet');

    if (NtHeaders32.OptionalHeader.Magic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC) then
      raise Exception.Create('Invalid PE32 image');

    var DebugRVA := NtHeaders32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;

    if (DebugRVA = 0) then
      raise Exception.Create('Image does not contain a debug directory address - please link with debug info enabled');

    if (NtHeaders32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size = 0) then
      raise Exception.Create('Image does not contain a debug directory - please link with debug info enabled');

    var DebugOffset := 0;

    // Read through section headers to locate the debug section header
    for var i := 0 to NtHeaders32.FileHeader.NumberOfSections-1 do
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
      raise Exception.Create('Failed to locate debug directory section - please link with debug info enabled');

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
      raise Exception.CreateFmt('Size of debug data too small: %d bytes available, %d required', [DebugDirectory.SizeOfData, DebugSize]);

    (*
    ** Debug data
    *)
    if (DebugDirectory._Type <> IMAGE_DEBUG_TYPE_CODEVIEW) then
    begin
      DebugDirectory._Type := IMAGE_DEBUG_TYPE_CODEVIEW;

      // Write updated debug directory
      Stream.Seek(DebugOffset, soBeginning);
      Stream.WriteBuffer(DebugDirectory, SizeOf(DebugDirectory));
      Writeln('- Type of debug directory entry has been set to IMAGE_DEBUG_TYPE_CODEVIEW.');
    end;

    // Populate and write a CV_INFO_PDB70 block
    var CodeViewInfoPDB: PCodeViewInfoPDB70;
    GetMem(CodeViewInfoPDB, DebugSize);
    try

      CodeViewInfoPDB.CvSignature := $53445352; // RSDS
      CodeViewInfoPDB.Signature := Signature; // GUID - must be the same as the one in the PDB
      CodeViewInfoPDB.Age := 1; // Generation counter - must be same value as the one in the PDB
      Move(Bytes[0], CodeViewInfoPDB.PdbFileName, Length(Bytes));
      CodeViewInfoPDB.PdbFileName[Length(Bytes)] := 0;

      // Write updated debug data
      Stream.Seek(DebugDirectory.PointerToRawData, soBeginning);
      Stream.WriteBuffer(CodeViewInfoPDB^, DebugSize);
      Writeln('- PDB file name has been stored in debug data.');

    finally
      FreeMem(CodeViewInfoPDB);
    end;

    if (NtHeaders32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size < SizeOf(TImageDebugDirectory)) then
    begin
      NtHeaders32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := SizeOf(TImageDebugDirectory);
      Writeln('- Debug directory size has been reset to correct value.');
    end;

    // Clear the checksum so file doesn't appear corrupt
    NtHeaders32.OptionalHeader.CheckSum := 0;
    Writeln('- Header checksum has been cleared.');

    // Write updated NT header
    Stream.Seek(DosHeader._lfanew, soBeginning);
    Stream.WriteBuffer(NtHeaders32, SizeOf(NtHeaders32));
    Writeln('- File has been updated.');
  finally
    Stream.Free;
  end;
end;

begin
  try
    Writeln('bindpdb - Copyright (c) 2021 Anders Melander');

    if (ParamCount < 1) then
    begin
      Writeln('Patches a Delphi compiled exe file to include a reference to a pdb file.');
      Writeln;
      Writeln('Usage: bindpdb <exe-filename> [<pdb-filename>]');
      Exit;
    end;

    var Filename := ParamStr(1);
    var PdbFilename := ParamStr(2);

    if (PdbFilename = '') then
      PdbFilename := TPath.ChangeExtension(TPath.GetFileName(Filename), '.pdb');

    PatchFile(Filename, PdbFilename);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

