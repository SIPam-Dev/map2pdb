unit debug.info.pdb.bind;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

procedure PatchPE(const Filename, PdbFilename: string);

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  WinApi.Windows,
  debug.info.codeview,
  debug.info.pdb,
  debug.info.log;

procedure PatchPE(const Filename, PdbFilename: string);

  function AlignTo(Value, Alignment: Cardinal): Cardinal;
  begin
    // Note: Alignment must be power of 2
    Result := (Value + Alignment - 1) and not(Alignment - 1);
  end;

const
  sSectionDebug: array[0..IMAGE_SIZEOF_SHORT_NAME-1] of AnsiChar = '.debug';
begin
  var Logger := RegisterDebugInfoModuleLogger('bind');
  Logger.Info('Patching PE file');

  // Get the PDB filename as UTF-8
  var DebugBytes := TEncoding.UTF8.GetBytes(PdbFilename);

  // Calculate size of the CodeView info block, including the filename and terminating zero
  var DebugSize: Cardinal := SizeOf(TCodeViewInfoPDB70) + Length(DebugBytes) + 1;
  // And the size of the debug section that contains the CodeView block
  var DebugSectionSize: Cardinal := DebugSize + SizeOf(TImageDebugDirectory);

  var Stream: TStream := nil;
  try
    try

      Stream := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);

    except
      on E: EFOpenError do
      begin
        Logger.Error(E.Message);
        Exit;
      end;
    end;

    (*
    ** DOS header
    *)
    var DosHeader: TImageDosHeader;
    Stream.ReadBuffer(DosHeader, SizeOf(DosHeader));

    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      Logger.Error('Invalid DOS file signature');


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
    // We start by reading the word that tells us what we're dealing with.
    var OptionalHeader32: TImageOptionalHeader32;
    var OptionalHeader64: TImageOptionalHeader64;
    Stream.ReadBuffer(OptionalHeader32.Magic, SizeOf(OptionalHeader32.Magic));


    // Then we read the 32-/64-bit part but leave out the data directory at the end.
    var PE32Plus := False;
    var NumberOfRvaAndSizes: DWORD := 0;

    case OptionalHeader32.Magic of
      IMAGE_NT_OPTIONAL_HDR32_MAGIC:
        begin
          Logger.Info('- PE32 image (32-bit)');
          PE32Plus := False;

          Stream.ReadBuffer(OptionalHeader32.MajorLinkerVersion,
            SizeOf(TImageOptionalHeader32)-SizeOf(Word)-SizeOf(OptionalHeader32.DataDirectory));

          NumberOfRvaAndSizes := OptionalHeader32.NumberOfRvaAndSizes;
          // Clear the checksum so file doesn't appear corrupt after we have modified it.
          OptionalHeader32.CheckSum := 0;
        end;

      IMAGE_NT_OPTIONAL_HDR64_MAGIC:
        begin
          Logger.Info('- PE32+ image (64-bit)');
          PE32Plus := True;
          OptionalHeader64.Magic := OptionalHeader32.Magic;

          Stream.ReadBuffer(OptionalHeader64.MajorLinkerVersion,
            SizeOf(TImageOptionalHeader64)-SizeOf(Word)-SizeOf(OptionalHeader64.DataDirectory));

          NumberOfRvaAndSizes := OptionalHeader64.NumberOfRvaAndSizes;
          // Clear the checksum so file doesn't appear corrupt after we have modified it.
          OptionalHeader64.CheckSum := 0
        end;
    else
      Logger.Error('Invalid or unsupported PE image type: %.4X', [OptionalHeader32.Magic]);
    end;


    (*
    ** Data directory
    *)
    // The data directory follows and is the same for 32- and 64-bit but the size of it
    // is dynamic (contrary to what the header structure suggests).
    // Read the data directory but limit the size to something usable (and reasonable).
    if (NumberOfRvaAndSizes < IMAGE_DIRECTORY_ENTRY_DEBUG-1) or (NumberOfRvaAndSizes > $100) then
      Logger.Error('Invalid or unsupported PE directory size: %d', [NumberOfRvaAndSizes]);
    var DataDirectory: TArray<TImageDataDirectory>;
    SetLength(DataDirectory, NumberOfRvaAndSizes);
    Stream.ReadBuffer(DataDirectory[0], NumberOfRvaAndSizes * SizeOf(TImageDataDirectory));


    (*
    ** Section table
    *)
    // If the directory already contains a pointer to a .debug section, and there's
    // enough room in that section, then we will use the section. Otherwise we need
    // to add a new section.
    var DebugOffset := 0; // File offset of debug data
    var DebugVirtualAddress: Cardinal := DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;

    // Get all the existing section headers.
    var SectionHeaderOffset := Stream.Position;
    var SectionHeaders: TArray<TImageSectionHeader>;
    SetLength(SectionHeaders, FileHeader.NumberOfSections);
    if (FileHeader.NumberOfSections > 0) then
      Stream.ReadBuffer(SectionHeaders[0], FileHeader.NumberOfSections * SizeOf(TImageSectionHeader));

    var HasDebugSection := False;
    var UseExistingDebugSection := False;

    if (DebugVirtualAddress <> 0) and (FileHeader.NumberOfSections > 0) then
    begin

      // Iterate the section headers to locate the debug section header.
      for var i := 0 to High(SectionHeaders) do
      begin
        if (DebugVirtualAddress >= SectionHeaders[i].VirtualAddress) and (DebugVirtualAddress < SectionHeaders[i].VirtualAddress+SectionHeaders[i].Misc.VirtualSize) then
        begin
          // Found it.

          // SectionHeaderOffset now contains the file offset of the section header.
          // We will use that when we need to update it later.

          DebugOffset := DebugVirtualAddress - SectionHeaders[i].VirtualAddress + SectionHeaders[i].PointerToRawData;
          HasDebugSection := True;

          // Verify that there's room in the section for our debug directory structure
          // and the CodeView info. Note the existing directory structure need not be
          // at the start of the section and that the section could contain information
          // which we do not want to overwrite.
          // If there's no room then we overwrite the whole section, so unless the
          // section is the .debug section this could really mess things up.
          if (SectionHeaders[i].SizeOfRawData >= (DebugVirtualAddress - SectionHeaders[i].VirtualAddress) + DebugSectionSize) then
            UseExistingDebugSection := True;

          break;
        end;
        Inc(SectionHeaderOffset, SizeOf(TImageSectionHeader));
      end;

      // If we didn't find a debug section then the PE is corrupt
      if (not HasDebugSection) then
        Logger.Error('Failed to locate debug section referenced in PE directory');

    end else
      // There was no existing section entry.
      // We will be adding a new one just past the existing section table.
      SectionHeaderOffset := Stream.Position;

    // If there was no .debug section or it was too small then we need to
    // add a new .debug section.
    if (not HasDebugSection) or (not UseExistingDebugSection) then
    begin

      if (not HasDebugSection) then
      begin
        Logger.Info('- Adding .debug section.');

        if (FileHeader.NumberOfSections > 0) then
        begin
          // Is there room in the section header list to add an entry?
          // The space available is from the current position (end of the existing section list)
          // to the first byte of the first section.
          if (SectionHeaders[0].PointerToRawData - SectionHeaderOffset < SizeOf(TImageSectionHeader)) then
            Logger.Error('There is no room left for an additional section header. Expansion has not been implemented');
        end;

        // We will be adding the debug data to the end of the file.
        DebugOffset := Stream.Size;

        // Add a new section header
        Inc(FileHeader.NumberOfSections);

        // When we add a new section we also need to update the image size
        if (PE32Plus) then
          Inc(OptionalHeader64.SizeOfImage, AlignTo(DebugSectionSize, OptionalHeader64.SectionAlignment))
        else
          Inc(OptionalHeader32.SizeOfImage, AlignTo(DebugSectionSize, OptionalHeader32.SectionAlignment));
      end else
        Logger.Info('- Updating existing .debug section.');

      var SectionHeader := Default(TImageSectionHeader);
      // Name: An 8-byte, null-padded UTF-8 encoded string. If the string is exactly 8 characters
      // long, there is no terminating null.
      Move(sSectionDebug, SectionHeader.Name, SizeOf (SectionHeader.Name));

      // VirtualAddress: For executable images, the address of the first byte of the section
      // relative to the image base when the section is loaded into memory.
      // The VirtualAddress of our new section is located after the last of the existing sections
      // and must be aligned.
      for var i := 0 to High(SectionHeaders) do
        SectionHeader.VirtualAddress := Max(SectionHeader.VirtualAddress, SectionHeaders[i].VirtualAddress + SectionHeaders[i].Misc.VirtualSize);
      if (PE32Plus) then
        SectionHeader.VirtualAddress := AlignTo(SectionHeader.VirtualAddress, OptionalHeader64.SectionAlignment)
      else
        SectionHeader.VirtualAddress := AlignTo(SectionHeader.VirtualAddress, OptionalHeader32.SectionAlignment);
      // SizeOfRawData: The size of the initialized data on disk. For executable images, this must
      // be a multiple of FileAlignment from the optional header. If this is less than VirtualSize,
      // the remainder of the section is zero-filled. Because the SizeOfRawData field is rounded
      // but the VirtualSize field is not, it is possible for SizeOfRawData to be greater than
      // VirtualSize as well.
      SectionHeader.SizeOfRawData := DebugSectionSize; // Loader doesn't seem to care about aligment, so we don't either
      // VirtualSize: The total size of the section when loaded into memory. If this value is greater
      // than SizeOfRawData, the section is zero-padded.
      SectionHeader.Misc.VirtualSize := DebugSectionSize;
      // PointerToRawData: The file pointer to the first page of the section within the COFF file.
      // For executable images, this must be a multiple of FileAlignment from the optional header.
      // When a section contains only uninitialized data, this field should be zero.
      // Offset in file of section data (i.e. debug directory)
      SectionHeader.PointerToRawData := DebugOffset; // Unaligned; The loader doesn't care
      // Characteristics: The flags that describe the characteristics of the section.
      SectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_DISCARDABLE;

      DebugVirtualAddress := SectionHeader.VirtualAddress;

      // Update the existing section header or write a new one.
      Stream.Position := SectionHeaderOffset;
      Stream.WriteBuffer(SectionHeader, SizeOf(SectionHeader));
    end;


    // Update data directory
    DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress := DebugVirtualAddress;
    DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := SizeOf(TImageDebugDirectory); // Just a single entry


    (*
    ** Debug directory
    *)
    Stream.Seek(DebugOffset, soBeginning);

    // We overwrite any existing entries there might be in the debug directory and replace it all with our own.
    {
      MSDN: Each debug directory entry identifies the location and size of a block of debug information.
      The specified RVA can be zero if the debug information is not covered by a section header (that is,
      it resides in the image file and is not mapped into the run-time address space). If it is mapped,
      the RVA is its address.
    }
    var DebugDirectory := Default(TImageDebugDirectory);
    // Type: The format of debugging information.
    DebugDirectory._Type := IMAGE_DEBUG_TYPE_CODEVIEW;
    // SizeOfData: The size of the debug data (not including the debug
    // directory itself).
    DebugDirectory.SizeOfData := DebugSize;
    // AddressOfRawData: The address of the debug data when loaded, relative
    // to the image base.
    DebugDirectory.AddressOfRawData := DebugVirtualAddress + SizeOf(DebugDirectory);
    // PointerToRawData: The file pointer to the debug data.
    DebugDirectory.PointerToRawData := Stream.Position + SizeOf(TImageDebugDirectory);
    Stream.WriteBuffer(DebugDirectory, SizeOf(DebugDirectory));

    (*
    ** Debug data
    *)
    // Populate and write a CV_INFO_PDB70 block
    var CodeViewInfoPDB := Default(TCodeViewInfoPDB70);
    CodeViewInfoPDB.CvSignature := $53445352; // RSDS
    CodeViewInfoPDB.Signature := PdbBuildSignature; // GUID - must be the same as the one in the PDB
    CodeViewInfoPDB.Age := PdbBuildAge; // Generation counter - must be same value as the one in the PDB
    Stream.WriteBuffer(CodeViewInfoPDB, SizeOf(CodeViewInfoPDB));
    Stream.WriteData(DebugBytes, Length(DebugBytes));
    const Zero: Byte = 0;
    Stream.WriteBuffer(Zero, 1);
    Logger.Info('- PDB file name has been stored in debug data.');


    // Write updated headers...
    Stream.Seek(DosHeader._lfanew + SizeOf(Signature), soBeginning);

    Stream.WriteBuffer(FileHeader, SizeOf(FileHeader));

    if (PE32Plus) then
      Stream.WriteBuffer(OptionalHeader64, SizeOf(TImageOptionalHeader64)-SizeOf(OptionalHeader64.DataDirectory))
    else
      Stream.WriteBuffer(OptionalHeader32, SizeOf(TImageOptionalHeader32)-SizeOf(OptionalHeader32.DataDirectory));

    // ...and the data directory
    Stream.WriteBuffer(DataDirectory[0], NumberOfRvaAndSizes * SizeOf(TImageDataDirectory));
    Logger.Info('- PE file has been updated.');


    // TODO : Clear certificate if PE has one. Our modification renders the certificate invalid anyway.

  finally
    Stream.Free;
  end;
end;

end.

