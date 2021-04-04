unit debug.info.writer.pdb;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

(*
 * This implementation of a PDB writer was inspired by Microsoft's PDB
 * source, the Epoch linker's half finished PDB emitter and the LLVM
 * documentation of the PDB and MSF file format. Many of the comments
 * have been copied verbatim from one of these sources.
 *
 *   - The LLVM Project
 *     Apache License v2.0 with LLVM Exceptions
 '     https://releases.llvm.org/11.0.0/LICENSE.TXT
 *
 *   - The microsoft-pdb repository
 *     Copyright (c) 2015 Microsoft Corporation. All rights reserved.
 *     MIT License
 '     https://github.com/microsoft/microsoft-pdb/blob/master/LICENSE
 *
 *   - Epoch Language Project
 *     Copyright (c) 2008-2018, Michael A. Lewis. All rights reserved.
 *     https://github.com/apoch/epoch-language/blob/renewal/License.txt
 *
 *)

interface

{$SCOPEDENUMS ON}

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Classes,
  Winapi.Windows,
  debug.info,
  debug.info.msf,
  debug.info.pdb,
  debug.info.writer;


// -----------------------------------------------------------------------------
//
//      TDebugInfoPdbWriter
//
// -----------------------------------------------------------------------------
// PDB file writer
// -----------------------------------------------------------------------------
type
  TDebugInfoPdbWriter = class(TDebugInfoWriter)
  strict private type

    TPDBFFileLayout = record
      StreamPDB: TMSFStream;
      StreamTPI: TMSFStream;
      StreamDBI: TMSFStream;
      StreamIPI: TMSFStream;
      StreamDBIGlobals: TMSFStream;
      StreamDBIPublics: TMSFStream;
      StreamDBISymbols: TMSFStream;
    end;

    TModuleLayout = record
      Stream: TMSFStream;
      SymByteSize: Cardinal;
      C13ByteSize: Cardinal;
    end;

    TModuleLayoutList = TDictionary<TDebugInfoModule, TModuleLayout>;

    TNamedStreamEntry = record
      Name: AnsiString;
      Stream: TMSFStream;
    end;

    TNamedStreamList = class(TList<TNamedStreamEntry>)
    private
      FReadOnly: boolean;
    public
      function Add(const Name: AnsiString; Stream: TMSFStream): integer; overload;
      property ReadOnly: boolean read FReadOnly write FReadOnly;
    end;

  strict private

    FBlockSize: Cardinal;
    FDebugInfo: TDebugInfo;

    FFiler: TMSFFile;
    FLayout: TPDBFFileLayout;
    FModuleLayout: TModuleLayoutList;
    FLinkerModule: TDebugInfoModule;
    FNamedStreams: TNamedStreamList;
    FStringTable: TStringList;

  strict private

    procedure PopulateStringList;

    function WritePDBInfoStream: TMSFStream; // Verified
    function WriteTPIStream: TMSFStream; // Verified
      function EmitTPIStream(Writer: TBinaryBlockWriter): Cardinal;
    function WriteDBIStream: TMSFStream;
      procedure PopulateDBISectionContribution(var SectionContribEntry: TSectionContribEntry; Module: TDebugInfoModule);
      function EmitDBISubstreamModules(Writer: TBinaryBlockWriter): Cardinal;
      function EmitDBISubstreamSectionContributions(Writer: TBinaryBlockWriter): Cardinal;
      function EmitDBISubstreamSectionMap(Writer: TBinaryBlockWriter): Cardinal;
      function EmitDBISubstreamFiles(Writer: TBinaryBlockWriter): Cardinal;
      function EmitDBISubstreamEC(Writer: TBinaryBlockWriter): Cardinal;
        function EmitStringTable(Writer: TBinaryBlockWriter; Strings: TStrings): Cardinal;
      function EmitDBISubstreamDebugHeader(Writer: TBinaryBlockWriter): Cardinal;
    function WriteIPIStream: TMSFStream; // Verified
    procedure WriteSymbols;
    procedure WriteDBIModuleSymbols; // Verified
      function EmitDBIModuleSymbol(Writer: TBinaryBlockWriter; Module: TDebugInfoModule; var ModuleLayout: TModuleLayout): Cardinal;
        function EmitModuleSymbols(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;
        function EmitSubsectionFileChecksums(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;
        function EmitSubsectionC13SourceFileLines(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;
    function WritePDBStrings: TMSFStream; // Verified

  public

    constructor Create; overload; override;
    constructor Create(ABlockSize: Cardinal); reintroduce; overload;

    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream; DebugInfo: TDebugInfo); override;

  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.AnsiStrings,
  System.SysUtils,
  System.Math,
  debug.info.codeview;

function AlignTo(Value, Alignment: Cardinal): Cardinal;
begin
  Result := (Value + Alignment - 1) and (not (Alignment - 1));
end;

function IsAligned(Value, Alignment: Cardinal): boolean;
begin
  Result := (Value and (Alignment - 1) = 0);
end;

function IsAsciiString(const S: AnsiString): boolean;
begin
  for var c in S do
    if (Ord(c) >= $80) then
      Exit(False);
  Result := True;
end;

function MemCmp(A, B: pointer; Len: Cardinal): integer;
begin
  Result := 0;
  while (Len > 0) and (Result = 0) do
  begin
    Result := PByte(A)^ - PByte(B)^;
    Dec(Len);
    Inc(PByte(A));
    Inc(PByte(B));
  end;
end;

function GsiRecordCmp(const S1, S2: AnsiString): integer;
begin
  // Shorter strings always compare less than longer strings.
  if (Length(S1) <> Length(S2)) then
    Exit(Length(S1) - Length(S2));

  // If either string contains non ascii characters, memcmp them.
  if (not IsAsciiString(S1)) or (not IsAsciiString(S2)) then
    Exit(MemCmp(pointer(S1), pointer(S2), Length(S1)));

  // Both strings are ascii, perform a case-insensitive comparison.
  Result := System.AnsiStrings.CompareText(S1, S2);
end;


// -----------------------------------------------------------------------------
//
//      TDebugInfoPdbWriter
//
// -----------------------------------------------------------------------------
const
  sLinkerModuleName = '* Linker *';

type
  TDebugInfoLinkerModule = class(TDebugInfoModule);

constructor TDebugInfoPdbWriter.Create(ABlockSize: Cardinal);
begin
  inherited Create;

  FBlockSize := ABlockSize;
  FModuleLayout := TModuleLayoutList.Create;
  FNamedStreams := TNamedStreamList.Create;
  FStringTable := TStringList.Create(dupIgnore, True, True);

end;

constructor TDebugInfoPdbWriter.Create;
begin
  Create(4096);
end;

destructor TDebugInfoPdbWriter.Destroy;
begin
  FStringTable.Free;
  FNamedStreams.Free;
  FModuleLayout.Free;

  inherited;
end;


function TDebugInfoPdbWriter.WritePDBInfoStream: TMSFStream;
var
  PDBStreamHeader: TPDBStreamHeader;
const
  EmptyBucket = Cardinal(-1);
begin
  Result := FFiler.FixedStreams[PDBStreamIndex.StreamPDB];

  // Named stream list is now being persisted and can no longer be modified
  FNamedStreams.ReadOnly := True;

  // Note: The LLVM documentation states that the hash table size is "Capacity" buckets
  // which means that it contains both full and empty buckets. However from examining
  // the output of llvm-pdbutil it seems that empty buckets are not written. In addition
  // an extra zero DWORD value is written after the hash table.

  Result.BeginStream;
  begin

    // Setup hash table with a load factor of 80%
    var Buckets: Cardinal := Ceil(FNamedStreams.Count * 1.25);
    var HashTable: TArray<Cardinal>;
    SetLength(HashTable, Buckets);
    for var Index := Low(HashTable) to High(HashTable) do
      HashTable[Index] := EmptyBucket;

    // Load hash table
    for var Index := 0 to FNamedStreams.Count-1 do
    begin
      var Hash := HashStringV1(FNamedStreams[Index].Name) mod Buckets;

      while (HashTable[Hash] <> EmptyBucket) do
        Hash := (Hash + 1) mod Buckets; // Collision - linear probe for next bucket

      HashTable[Hash] := Index;
    end;


    PDBStreamHeader.Version := Ord(PDBStreamVersion.VC70);
    PDBStreamHeader.Signature := 0;
    PDBStreamHeader.Age := PdbBuildAge;
    PDBStreamHeader.UniqueId := PdbBuildSignature;

    Result.Writer.Write<TPDBStreamHeader>(PDBStreamHeader);

    // Following the header is a serialized hash table whose key type is a string, and whose value
    // type is an integer. The existence of a mapping X -> Y means that the stream with the name X
    // has stream index Y in the underlying MSF file. Note that not all streams are named (for
    // example, the TPI Stream has a fixed index and as such there is no need to look up its index
    // by name). In practice, there are usually only a small number of named streams and these are
    // enumerated in the table of streams in The PDB File Format. A corollary of this is if a
    // stream does have a name (and as such is in the named stream map) then consulting the Named
    // Stream Map is likely to be the only way to discover the stream's MSF stream index. Several
    // important streams (such as the global string table, which is called /names) can only be
    // located this way, and so it is important to both produce and consume this correctly as tools
    // will not function correctly without it.

    // Named Stream Map follows:
    // The on-disk layout of the Named Stream Map consists of 2 components:
    //
    //   1) A buffer of zero terminated strings prefixed by a 32-bit length.
    //
    //   2) A serialized hash table whose key and value types are both DWORDs.
    //      The key is the offset of a null-terminated string in the string data buffer
    //      specifying the name of the stream, and the value is the MSF stream index of
    //      the stream with said name.
    //      Note that although the key is an integer, the hash function used to find the
    //      right bucket hashes the string at the corresponding offset in the string
    //      data buffer.

    // Find the size of all names combined and save their individual offsets
    var NameOffsets: TArray<Cardinal>;
    SetLength(NameOffsets, FNamedStreams.Count);
    var StringSize: Cardinal := 0;
    for var Index := 0 to FNamedStreams.Count-1 do
    begin
      NameOffsets[Index] := StringSize;
      Inc(StringSize, Length(FNamedStreams[Index].Name)+1);
    end;

    // Write size of strings
    Result.Writer.Write(StringSize);

    // Write the strings
    for var Index := 0 to FNamedStreams.Count-1 do
      Result.Writer.Write(FNamedStreams[Index].Name); // Zero terminated string

    // Hash table:
    // +--------------------+-- +0
    // |        Size        |
    // +--------------------+-- +4
    // |      Capacity      |
    // +--------------------+-- +8
    // | Present Bit Vector |
    // +--------------------+-- +N
    // | Deleted Bit Vector |
    // +--------------------+-- +M                  -+
    // |        Key         |                        |
    // +--------------------+-- +M+4                 |
    // |       Value        |                        |
    // +--------------------+-- +M+4+sizeof(Value)   |
    //          ...                                  +- |Capacity| Bucket entries
    // +--------------------+                        |
    // |        Key         |                        |
    // +--------------------+                        |
    // |       Value        |                        |
    // +--------------------+                       -+
    //
    // Size: The number of values contained in the hash table.
    Result.Writer.Write(Cardinal(FNamedStreams.Count));

    // Capacity: The number of buckets in the hash table.
    Result.Writer.Write(Buckets);

    // Present and Deleted Bit Vectors:
    // +--------------------+-- +0
    // |     Word Count     |
    // +--------------------+-- +4
    // |        Word_0      |        -+
    // +--------------------+-- +8    |
    // |        Word_1      |         |
    // +--------------------+-- +12   +- |Word Count| values
    //          ...                   |
    // +--------------------+         |
    // |       Word_N       |         |
    // +--------------------+        -+
    // Note that although we use the term Word, the data type is actually a 32-bit value.

    // Present Bit Vector: A serialized bit vector which contains information about which
    // buckets have valid values. If the bucket has a value, the corresponding bit will be
    // set, and if the bucket doesn't have a value (either because the bucket is empty or
    // because the value is a tombstone value) the bit will be unset.

    // Write bit vector count
    var BitVectorWords: Cardinal := (Buckets + SizeOf(Cardinal) * 8 - 1) div (SizeOf(Cardinal) * 8);
    Result.Writer.Write(BitVectorWords);

    var BitVectorCount: Cardinal := 0;
    var BitVector: Cardinal := 0;
    var BitMask: Cardinal := 1;
    for var Bucket := Low(HashTable) to High(HashTable) do
    begin
      if (HashTable[Bucket] <> EmptyBucket) then
        BitVector := BitVector or BitMask;

      BitMask := BitMask shl 1;

      if (BitMask = 0) then
      begin
        Result.Writer.Write(BitVector);
        BitVector := 0;
        BitMask := 1;
        Inc(BitVectorCount);
      end;
    end;
    if (BitVector <> 0) then
    begin
      Result.Writer.Write(BitVector);
      Inc(BitVectorCount);
    end;
    Assert(BitVectorCount = BitVectorWords);

    // Deleted Bit Vector: A serialized bit vector which contains information about which
    // buckets have tombstone values. If the entry in this bucket is deleted, the bit will
    // be set, otherwise it will be unset.
    while (BitVectorCount > 0) do
    begin
      Result.Writer.Write(Cardinal(0)); // None deleted
      Dec(BitVectorCount);
    end;

    // Keys and Values: A list of Capacity hash buckets, where the first entry is the key
    // (always a DWORD), and the second entry is the value. The state of each bucket
    // (valid, empty, deleted) can be determined by examining the present and deleted bit
    // vectors.
    for var Bucket := Low(HashTable) to High(HashTable) do
    begin
      var Index := HashTable[Bucket];

      if (Index <> EmptyBucket) then
      begin
        Result.Writer.Write(Cardinal(NameOffsets[Index]));
        Result.Writer.Write(Cardinal(FNamedStreams[Index].Stream.Index));
      end else
      // Empty buckets are not written. See note at top.
      {
      begin
        Result.Writer.Write(Cardinal(0));
        Result.Writer.Write(Cardinal(0));
      end;
      }
    end;
    // Write extra DWORD. See note at top.
    Result.Writer.Write(Cardinal(0));

    // PDB Feature Code(s)
    Result.Writer.Write(Cardinal(Ord(PDBRawFeatureSig.VC110)));

  end;
  Result.EndStream;
end;


function TDebugInfoPdbWriter.WriteTPIStream: TMSFStream;
begin
  // TPI & IPI streams have the same layout - and we don't use either of them
  Result := FFiler.FixedStreams[PDBStreamIndex.StreamTPI];

  Result.BeginStream;
  begin

    EmitTPIStream(Result.Writer);

  end;
  Result.EndStream;
end;


function TDebugInfoPdbWriter.EmitTPIStream(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  var TPIStreamHeader := Default(TTPIStreamHeader);
  TPIStreamHeader.Version := Ord(TPIStreamVersion.V80);
  TPIStreamHeader.HeaderSize := SizeOf(TPIStreamHeader);
  TPIStreamHeader.TypeIndexBegin := $1000;
  TPIStreamHeader.TypeIndexEnd := $1000;
  TPIStreamHeader.HashStreamIndex := TMSFStream.NullStreamIndex;
  TPIStreamHeader.HashAuxStreamIndex := TMSFStream.NullStreamIndex;
  // These values appears fixed:
  TPIStreamHeader.HashKeySize := 4;
  TPIStreamHeader.NumHashBuckets := $0003FFFF;// 262143

  Writer.Write<TTPIStreamHeader>(TPIStreamHeader);

  Result := Writer.BaseStream.Position - Result;
end;


procedure TDebugInfoPdbWriter.PopulateDBISectionContribution(var SectionContribEntry: TSectionContribEntry; Module: TDebugInfoModule);
begin
  SectionContribEntry := Default(TSectionContribEntry);
  SectionContribEntry.Section := Module.Segment.Index;
  SectionContribEntry.Offset := Module.Offset;
  SectionContribEntry.Size := Module.Size;
  SectionContribEntry.Characteristics := Module.Segment.Characteristics;
  SectionContribEntry.ModuleIndex := 0; // Caller must set this value
  SectionContribEntry.DataCrc := 0;
  SectionContribEntry.RelocCrc := 0;
end;

procedure TDebugInfoPdbWriter.PopulateStringList;
begin

  // Disable sort order while we bulk insert
  FStringTable.Sorted := False;

  // Add an empty string so we can resolve source filename for modules that doesn't have source lines.
  // This means that the on-disk list will contain two empty names: This one and the implicit one
  // we add when building the hash table.
  FStringTable.AddObject('', Pointer(0));

  // Add all source file names
  for var SourceFile in FDebugInfo.SourceFiles do
    FStringTable.Add(SourceFile.Filename);

  // Enable sort order again
  FStringTable.Sorted := True;

  // Precompute the offset each string will have in the string table when
  // we write the string in WritePDBStrings.
  var Offset := 1; // First entry is an empty string
  for var Index := 0 to FStringTable.Count-1 do
  begin
    // Store offset in string table for use by TCVFileChecksumEntry.FileNameOffset when
    // writing source files/lines module info.
    FStringTable.Objects[Index] := pointer(Offset);
    Inc(Offset, Length(FStringTable[Index])+1);
  end;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamModules(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  var ModuleIndex := 0;
  for var Module in FDebugInfo.Modules do
  begin

    // Get precalculated layout values from WriteDBIModuleSymbols
    var ModuleLayout := FModuleLayout[Module];

    var ModInfo := Default(TModInfo);
    ModInfo := Default(TModInfo);
    PopulateDBISectionContribution(ModInfo.SectionContrib, Module);
    ModInfo.SectionContrib.ModuleIndex := ModuleIndex;

    ModInfo.ModuleSymStreamIndex := ModuleLayout.Stream.Index;  // Stream number of debug info
    ModInfo.SymByteSize := ModuleLayout.SymByteSize;            // Note that size includes the stream header field
                                                                // TModiStream.Signature regardless of any symbols
                                                                // written.
    ModInfo.C11ByteSize := 0;                                   // We do not support C11 format
    ModInfo.C13ByteSize := ModuleLayout.C13ByteSize;            // Bytes of C13 line number info
    ModInfo.SourceFileCount := Module.SourceFiles.Count;

    ModInfo.SourceFileNameIndex := 0;

    ModInfo.Unused1 := ModuleIndex; // LLVM appears to place a sequential value in this field
    Inc(ModuleIndex);

    Writer.Write<TModInfo>(ModInfo);


    // Variable length file names follows the fixed size structure
    Writer.Write(Module.Name);
    Writer.Write(Module.ObjectName);

    // Align to 4 bytes
    Writer.PadToAligment(4);
  end;


  // Write "* Linker *" module
  begin
    var ModInfo := Default(TModInfo);
    var ModuleLayout := FModuleLayout[FLinkerModule];
    ModuleLayout := FModuleLayout[FLinkerModule];
    ModInfo.SectionContrib.ModuleIndex := ModuleIndex;
    ModInfo.ModuleSymStreamIndex := ModuleLayout.Stream.Index;
    ModInfo.SymByteSize := ModuleLayout.SymByteSize;
    ModInfo.C13ByteSize := ModuleLayout.C13ByteSize;
    ModInfo.Unused1 := ModuleIndex;
    Writer.Write<TModInfo>(ModInfo);
    Writer.Write(AnsiString(sLinkerModuleName));
    Writer.Write(Byte(0)); // ObjectName
  end;


  // Align to 4 bytes
  Writer.PadToAligment(4);

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamSectionContributions(Writer: TBinaryBlockWriter): Cardinal;

  procedure EmitDBISectionContribution(Module: TDebugInfoModule; ModuleIndex: Word);
  var
    SectionContribEntry: TSectionContribEntry;
  begin
    PopulateDBISectionContribution(SectionContribEntry, Module);
    SectionContribEntry.ModuleIndex := ModuleIndex;

    Writer.Write<TSectionContribEntry>(SectionContribEntry);
  end;

begin
  Result := Writer.BaseStream.Position;

  Writer.Write(Cardinal(Ord(SectionContrSubstreamVersion.Ver60)));

  var ModuleIndex := 0;
  for var Module in FDebugInfo.Modules do
  begin
    EmitDBISectionContribution(Module, ModuleIndex);
    Inc(ModuleIndex);
  end;

  EmitDBISectionContribution(FLinkerModule, ModuleIndex);

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamSectionMap(Writer: TBinaryBlockWriter): Cardinal;

  function CharacteristicsToSectionMapEntryFlags(Characteristics: Cardinal): Cardinal;
  begin
    Result := Ord(SectionMapEntryFlags.IsSelector); // According to LLVM this is always set

    if (Characteristics and IMAGE_SCN_MEM_READ <> 0) then
      Result := Result or Ord(SectionMapEntryFlags.Read);

    if (Characteristics and IMAGE_SCN_MEM_WRITE <> 0) then
      Result := Result or Ord(SectionMapEntryFlags.Write);

    if (Characteristics and IMAGE_SCN_MEM_EXECUTE <> 0) then
      Result := Result or Ord(SectionMapEntryFlags.Execute);

    if (Characteristics and IMAGE_SCN_MEM_16BIT = 0) then
      Result := Result or Ord(SectionMapEntryFlags.AddressIs32Bit);
  end;

begin
  Result := Writer.BaseStream.Position;

  var SectionMapHeader: TSectionMapHeader;

  SectionMapHeader.Count := FDebugInfo.Segments.Count + 1; // +1 for the "magic" entry
  SectionMapHeader.LogCount := FDebugInfo.Segments.Count;
  Writer.Write<TSectionMapHeader>(SectionMapHeader);

  var SectionMapEntry := Default(TSectionMapEntry);

  SectionMapEntry.SectionName := $FFFF;
  SectionMapEntry.ClassName := $FFFF;

  // Note: SectionMapEntry.Offset must be zero!
  // Offsets appears to be relative to this value.

  for var Segment in FDebugInfo.Segments do
  begin

    SectionMapEntry.Frame := Segment.Index; // LLVM assigns a sequential number to this field, starting from 1
    SectionMapEntry.Flags := CharacteristicsToSectionMapEntryFlags(Segment.Characteristics);
    SectionMapEntry.SectionLength := Segment.Size;

    Writer.Write<TSectionMapEntry>(SectionMapEntry);
  end;

  // Magic section for absolute symbols
  SectionMapEntry.Frame := FDebugInfo.Segments.Count+1;
  SectionMapEntry.Flags := Ord(SectionMapEntryFlags.AddressIs32Bit) or Ord(SectionMapEntryFlags.IsAbsoluteAddress);
  SectionMapEntry.SectionLength := Cardinal(-1);
  Writer.Write<TSectionMapEntry>(SectionMapEntry);

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamEC(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  var Strings := TStringList.Create;
  try

    // We don't use the EC substream but emit it so our output matches that of LLVM. Also LLVM
    // chokes on the file if it doesn't contain an EC substream...
    EmitStringTable(Writer, Strings);

  finally
    Strings.Free;
  end;

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamFiles(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  var FileInfoSubstream: TFileInfoSubstream;

  FileInfoSubstream.NumModules := FDebugInfo.Modules.Count + 1; // +1 for "* Linker *" module

  // According to LLVM NumSourceFiles value is ignored to avoid overflow but they populate it regardless.
  FileInfoSubstream.NumSourceFiles := FDebugInfo.SourceFiles.Count;

  Writer.Write<TFileInfoSubstream>(FileInfoSubstream);


  // After the fixed part above follows the arrays that, among other things, contains
  // offsets into the string list that follows the arrays.

  // ModIndices: array[NumModules] of Word;
  // Ignored according to LLVM. LLVM however writes a sequential value to the field.
  for var i := 0 to FileInfoSubstream.NumModules-1 do
    Writer.Write(Word(i));

  // ModFileCounts: array[NumModules] of Word;
  for var Module in FDebugInfo.Modules do
    Writer.Write(Word(Module.SourceFiles.Count));
  Writer.Write(Word(0)); // "* Linker *" module


  // In order to get the offset values we first skip the offset array, write the
  // strings while saving the offsets, and then go back and write the offset array.
  var ArrayPos := Writer.BaseStream.Position;

  // Compute the size of the arrays so we can skip them
  var OffsetCount := 0;
  for var Module in FDebugInfo.Modules do
    Inc(OffsetCount, Module.SourceFiles.Count);
  var OffsetArraySize := OffsetCount * SizeOf(Cardinal);

  // Skip the offset array
  Writer.BaseStream.Position := Writer.BaseStream.Position + OffsetArraySize;


  // Write the strings and save the offsets along the way
  // We can index by source file under the assumption that filenames are unique since
  // TDebugInfoSourceFiles enforces this.
  var Offsets := TDictionary<TDebugInfoSourceFile, Cardinal>.Create(FDebugInfo.SourceFiles.Count);
  try

    var Offset: Cardinal := 0;

    // Apart from the real file names we write one empty file name for those
    // cases where we don't want a file name output.
//    Offsets.Add(nil, Offset);
//    Writer.Write(Byte(0)); // Empty, zero terminated string
//    Inc(Offset);

    for var SourceFile in FDebugInfo.SourceFiles do
    begin

      // Save offset for this filename
      Offsets.Add(SourceFile, Offset);

      Writer.Write(SourceFile.Filename); // Zero terminated string

      Inc(Offset, Length(SourceFile.Filename)+1);

    end;

    // Align to 4 bytes
    Writer.PadToAligment(4);


    // Go back and write the offset array
    var SavePos := Writer.BaseStream.Position;
    Writer.BaseStream.Position := ArrayPos;

    // FileNameOffsets: array[NumSourceFiles] of Cardinal;
    for var Module in FDebugInfo.Modules do
      for var SourceFile in Module.SourceFiles do
      begin
        Offset := Offsets[SourceFile];
        Writer.Write(Offset);
      end;

    // Continue after the string list
    Writer.BaseStream.Position := SavePos;

  finally
    Offsets.Free;
  end;

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBISubstreamDebugHeader(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  // We don't provide any of the optional debug streams yet
  for var HeaderType := Low(PDBDbgHeaderType) to High(PDBDbgHeaderType) do
    Writer.Write(Word(TMSFStream.NullStreamIndex));

  // TODO : I believe the SectionHdr stream contains the segment names
  (*
  for var Segment in FDebugInfo.Segments do
  begin
    xxx.Add(Segment.SegClassName);
    xxx.Add(Segment.Name);
  end;
  *)

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.WriteDBIStream: TMSFStream;
begin
  Result := FFiler.FixedStreams[PDBStreamIndex.StreamDBI];

  Result.BeginStream;
  begin

    var DBIStreamHeader := Default(TDBIStreamHeader);

    DBIStreamHeader.VersionSignature := -1;
    DBIStreamHeader.VersionHeader := Ord(DbiStreamVersion.V70);
    DBIStreamHeader.Age := PdbBuildAge;
    DBIStreamHeader.GlobalStreamIndex := FLayout.StreamDBIGlobals.Index;
    DBIStreamHeader.BuildNumber := 0;
    DBIStreamHeader.PublicStreamIndex := FLayout.StreamDBIPublics.Index;
    DBIStreamHeader.PdbDllVersion := 0;
    DBIStreamHeader.SymRecordStreamIndex := FLayout.StreamDBISymbols.Index;
    DBIStreamHeader.PdbDllRbld := MakeWord(11, 14); // Fake LINK 14.11
    DBIStreamHeader.ModInfoSize := 0;             // Updated below
    DBIStreamHeader.SectionContributionSize := 0; // Updated below
    DBIStreamHeader.SectionMapSize := 0;          // Updated below
    DBIStreamHeader.SourceInfoSize := 0;          // Updated below
    DBIStreamHeader.TypeServerMapSize := 0;       // Type Server Map substream not present
    DBIStreamHeader.ECSubstreamSize := 0;         // Updated below
    DBIStreamHeader.OptionalDbgHeaderSize := 0;   // Updated below
    DBIStreamHeader.Flags := 0;
    // - The LLVM docs state we should use a CV_CPU_TYPE_e but then uses
    //   an IMAGE_FILE_HEADER.Machine value as an example.
    // - dbg2pdg uses an IMAGE_FILE_HEADER value.
    // - The Epoch linker uses an IMAGE_FILE_HEADER value.
    // IMAGE_FILE_MACHINE_AMD64 = $8664
    // CV_CPU_TYPE_e.CV_CFL_X64 = $D0
    DBIStreamHeader.Machine := IMAGE_FILE_MACHINE_AMD64;

    // Seek past the header. We will write it once the substreams has been written.
    var HeaderPos := Result.Writer.BaseStream.Position;
    Result.Writer.BaseStream.Seek(SizeOf(DBIStreamHeader), soCurrent);


    // Emit substreams and store their sizes in the header
    // Note: The order is significant!

    DBIStreamHeader.ModInfoSize                 := EmitDBISubstreamModules(Result.Writer);
    DBIStreamHeader.SectionContributionSize     := EmitDBISubstreamSectionContributions(Result.Writer);
    DBIStreamHeader.SectionMapSize              := EmitDBISubstreamSectionMap(Result.Writer);
    DBIStreamHeader.SourceInfoSize              := EmitDBISubstreamFiles(Result.Writer);
    DBIStreamHeader.ECSubstreamSize             := EmitDBISubstreamEC(Result.Writer); // LLVM has an extra empty bucket
    DBIStreamHeader.OptionalDbgHeaderSize       := EmitDBISubstreamDebugHeader(Result.Writer);


    // Now that we have the size of all the substreams and the header is complete we can write it
    var SavePos := Result.Writer.BaseStream.Position;
    Result.Writer.BaseStream.Position := HeaderPos;
    Result.Writer.Write<TDBIStreamHeader>(DBIStreamHeader);
    Result.Writer.BaseStream.Position := SavePos;

    Assert(SavePos - HeaderPos = SizeOf(TDBIStreamHeader) +
      DBIStreamHeader.ModInfoSize +
      DBIStreamHeader.SectionContributionSize +
      DBIStreamHeader.SectionMapSize +
      DBIStreamHeader.SourceInfoSize +
      DBIStreamHeader.TypeServerMapSize +
      DBIStreamHeader.ECSubstreamSize +
      DBIStreamHeader.OptionalDbgHeaderSize);
  end;
  Result.EndStream;
end;


function TDebugInfoPdbWriter.WriteIPIStream: TMSFStream;
begin
  // TPI & IPI streams have the same layout - and we don't use either of them
  Result := FFiler.FixedStreams[PDBStreamIndex.StreamIPI];

  Result.BeginStream;
  begin

    EmitTPIStream(Result.Writer);

  end;
  Result.EndStream;
end;


type
  // This struct is equivalent to TCVPublicSym32, but it has been
  // optimized to ease serialization and sorting operations during
  // PDB writing.
  TPublicSym32ex = record
    PublicSym32: TCVPublicSym32;
    Symbol: TDebugInfoSymbol;
    Name: AnsiString;
    SymOffset: Cardinal;        // Offset of the symbol record in the publics stream.
    BucketIndex: Word;          // GSI hash table bucket index. The maximum value is IPHR_HASH.
  end;

  TPublicSymArray = TArray<TPublicSym32ex>;
  PPublicSymArray = ^TPublicSymArray;

type
  TGSIHashStreamBuilder = record
  strict private type
    // The hash bitmap has |ceil(IPHR_HASH/32)+ 1| slots in it, a slot being 32
    // bits. The reference implementation builds a hash table with IPHR_HASH
    // buckets in it.
    // The last bucket is used to link together free hash table cells in a linked
    // list, but it is always empty in the compressed, on-disk format. However,
    // the bitmap must have a bit for it.
    THashBitmap = array[0..(IPHR_HASH + 32) div 32-1] of Cardinal;

    THashRecords = TArray<TPSHashRecord>;

    TBuckets = array[0..IPHR_HASH] of Cardinal;

  public
    HashRecords: THashRecords;
    HashBitmap: THashBitmap;
    BucketOffsets: TArray<Cardinal>;

    function CalculateSerializedLength: Cardinal;

    // Assign public and global symbol records into hash table buckets.
    // Modifies the list of records to store the bucket index, but does not
    // change the order.
    procedure BuildBuckets(RecordZeroOffset: Cardinal; var Records: TPublicSymArray);

    function Emit(Writer: TBinaryBlockWriter): Cardinal;
  end;

function TGSIHashStreamBuilder.Emit(Writer: TBinaryBlockWriter): Cardinal;
begin
  Result := Writer.BaseStream.Position;

  // Based on GSI1::fWriteHash
  // https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L590

  // Emit header
  var GSIHashHeader := Default(TGSIHashHeader);
  GSIHashHeader.VerSignature := GSIHashHeaderSignature;
  GSIHashHeader.VerHdr := GSIHashHeaderVersionV70;
  GSIHashHeader.HrSize := Length(HashRecords) * SizeOf(TPSHashRecord);
  if (GSIHashHeader.HrSize > 0) then
    GSIHashHeader.NumBuckets := SizeOf(THashBitmap) + Length(BucketOffsets) * SizeOf(Cardinal);
  Assert(IsAligned(SizeOf(GSIHashHeader), 4));
  Assert(IsAligned(GSIHashHeader.HrSize, 4));
  Assert(IsAligned(GSIHashHeader.NumBuckets, 4));

  Writer.Write<TGSIHashHeader>(GSIHashHeader);


  // Emit records
  Writer.WriteArray<TPSHashRecord>(HashRecords);


  // Emit bitmap
  if (GSIHashHeader.HrSize > 0) then // See note in CalculateSerializedLength about why this condition needs to be here
  begin
    Writer.Write<THashBitmap>(HashBitmap);
    Assert(SizeOf(HashBitmap) = AlignTo(IPHR_HASH + 1, 32) div 8); // IPHR_HASH = size in bits, +1 for the extra empty bucket


    // Emit bucket offsets
    Writer.WriteArray<Cardinal>(BucketOffsets);


    // Verify that number of bits set in bitmap equals number of buckets
    var BitCount := 0;
    for var Row := 0 to High(HashBitmap) do
      for var Bit := 0 to SizeOf(Cardinal)*8-1 do
        if (HashBitmap[Row] and (1 shl Bit) <> 0) then
          Inc(BitCount);
    Assert(BitCount = Length(BucketOffsets));
    Assert(Length(BucketOffsets) <= Length(HashRecords));
  end;

  Result := Writer.BaseStream.Position - Result;

  Assert(Result = SizeOf(TGSIHashHeader) + GSIHashHeader.HrSize + GSIHashHeader.NumBuckets);
end;

function TGSIHashStreamBuilder.CalculateSerializedLength: Cardinal;
begin
  Result := SizeOf(TGSIHashHeader);

  // The LLVM implementation does not contain the conditional below; It includes the
  // SizeOf(HashBitmap) in the result even when Length(HashRecords)=0. This will however
  // cause the LLVM PDB reader to fail with a "Corrupted publics stream." error if the
  // Publics stream is saved with no symbols (which is legal).
  if (Length(HashRecords) > 0) then
    Result := Result + Cardinal(
      Length(HashRecords) * SizeOf(TPSHashRecord) +
      SizeOf(HashBitmap) +
      Length(BucketOffsets) * SizeOf(Cardinal));
end;

procedure TGSIHashStreamBuilder.BuildBuckets(RecordZeroOffset: Cardinal; var Records: TPublicSymArray);
type
  TBucketChain = TArray<integer>;
  THashTable = array[0..IPHR_HASH-1] of TBucketChain;
  TChainLength = array[0..IPHR_HASH-1] of integer;
begin

  // The reference implementation is GSI1::fWriteHash
  // https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L590


  // Precompute all hash keys.
  for var Index := 0 to High(Records) do
    Records[Index].BucketIndex := HashStringV1(Records[Index].Name) mod IPHR_HASH;

  // Find the size of each bucket chain...
  var ChainLength := Default(TChainLength);
  for var Index := 0 to High(Records) do
    Inc(ChainLength[Records[Index].BucketIndex]);

  // ...and then allocate the chains
  var HashTable := Default(THashTable);
  for var Index := 0 to High(ChainLength) do
    SetLength(HashTable[Index], ChainLength[Index]);


  // Populate hash table with Records indices
  for var Index := 0 to High(Records) do
  begin
    var Bucket := Records[Index].BucketIndex;

    // We reuse ChainLength[Bucket] as a chain index which means that
    // we fill the chain from the end.
    HashTable[Bucket][ChainLength[Bucket]-1] := Index;

    Dec(ChainLength[Bucket]);
  end;


  // Local var so we can access the param in the comparer
  var RecordsAccess: PPublicSymArray := @Records;

  // Within the bucket chains, sort each bucket by memcmp of the symbol's name.
  // It's important that we use the same sorting algorithm as is used by the
  // reference implementation to ensure that the search for a record within a
  // bucket can properly early-out when it detects the record won't be found.
  // The algorithm used here corresponds to the function
  // caseInsensitiveComparePchPchCchCch in the reference implementation.
  var Comparer := TComparer<integer>.Construct(
    function(const A, B: integer): integer
    begin
      var L := RecordsAccess^[A];
      var R := RecordsAccess^[B];

      Result := GsiRecordCmp(L.Name, R.Name);

      // This comparison is necessary to make the sorting stable in the presence
      // of two static globals with the same name.
      if (Result = 0) then
        Result :=  integer(L.SymOffset) - integer(R.SymOffset);
    end);

  var ChainCount := 0;
  for var Index := 0 to High(HashTable) do
  begin
    if (Length(HashTable[Index]) = 0) then
      continue;

    TArray.Sort<integer>(HashTable[Index], Comparer);

    // Count number of chains while we're at it
    Inc(ChainCount);
  end;


  // Place symbols into the hash table in bucket order. This flattens the hash
  // structure to a sequential list of items with all empty buckets removed.
  SetLength(HashRecords, Length(Records));
  SetLength(BucketOffsets, ChainCount);
  HashBitmap := Default(THashBitmap);
  var BucketCursor := 0;
  var BucketOffsetIndex := 0;
  var ChainStartOff: Cardinal := 0;
  const SizeOfHROffsetCalc = 12; // SizeOf(HROffsetCalc)
  for var BucketIndex := 0 to High(HashTable) do
  begin
    if (Length(HashTable[BucketIndex]) = 0) then
      continue;

    // Calculate what the offset of the first hash record in the chain would
    // be if it were inflated to contain 32-bit pointers. On a 32-bit system,
    // each record would be 12 bytes. See HROffsetCalc in gsi.h.
    BucketOffsets[BucketOffsetIndex] := ChainStartOff;
    Inc(BucketOffsetIndex);

    // Mark bucket in bitmap
    var BitmapRow := BucketIndex div 32;
    var BitmapBit := BucketIndex mod 32;
    HashBitmap[BitmapRow] := HashBitmap[BitmapRow] or (1 shl BitmapBit);

    for var ChainIndex := 0 to High(HashTable[BucketIndex]) do
    begin
      var Index := HashTable[BucketIndex][ChainIndex];

      // The reference implementation states:
      //   "ptrs in the stream are offsets biased by one to distinguish null ptrs/offsets"
      //   https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L1979
      // As far as I can see LLVM doesn't do this on write but accounts for it on read.
      HashRecords[BucketCursor].Off := Records[Index].SymOffset + 1;
      HashRecords[BucketCursor].CRef := 1; // Always use a refcount of one for now.

      Inc(BucketCursor);
      Inc(ChainStartOff, SizeOfHROffsetCalc);
    end;
  end;
end;

procedure TDebugInfoPdbWriter.WriteSymbols;
var
  Publics: TPublicSymArray;
  Globals: TList<TCVSymbol>;

  procedure BuildPublicsHash(var HashStreamBuilder: TGSIHashStreamBuilder);
  begin
    HashStreamBuilder.BuildBuckets(0, Publics);
  end;

  procedure BuildGlobalsHash(var HashStreamBuilder: TGSIHashStreamBuilder; RecordZeroOffset: Cardinal);
  begin
    // Build up a list of globals to be bucketed. Use the TPublicSym32ex data
    // structure for this purpose, even though these are global records, not
    // public records. Most of the same fields are required:
    // - Name
    // - SymOffset
    // - BucketIdx
    // The dead fields are Offset, Segment, and Flags.
    var Records: TPublicSymArray;
    SetLength(Records, Globals.Count);

    var SymOffset := RecordZeroOffset;

    for var Index := 0 to Globals.Count-1 do
    begin
      Records[Index].Name := Globals[Index].Name;
      Records[Index].BucketIndex := 0;

      Records[Index].SymOffset := SymOffset;
      Inc(SymOffset, Globals[Index].Length);
    end;

    HashStreamBuilder.BuildBuckets(RecordZeroOffset, Records);
  end;

  function EmitPublics(Writer: TBinaryBlockWriter; var Records: TPublicSymArray): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    var Offset: Cardinal := 0;

    for var Index := 0 to High(Records) do
    begin
      var Symbol := Records[Index];

      // Ensure name doesn't make symbol too big
      var NameSize := Min(Length(Symbol.Name), CVMaxRecordLength - SizeOf(TCVPublicSym32) - 1); // -1 is zero termination
      var Size: Word := AlignTo(SizeOf(TCVPublicSym32) + NameSize + 1, 4); // +1 is zero termination

      var PublicSym32: TCVPublicSym32 := Symbol.PublicSym32;
      PublicSym32.Header.RecordLen := Size - SizeOf(Word); // Exclude SizeOf(RecordKind) from RecordLen
      Assert(PublicSym32.Header.RecordKind = Ord(CVSymbolRecordKind.S_PUB32));

      Writer.Write<TCVPublicSym32>(PublicSym32);

      // Name string
      if (NameSize > 0) then
        Writer.WriteBuffer(Symbol.Name[1], NameSize);

      Records[Index].SymOffset := Offset; // Save stream offset for use in hash table and address map
      Inc(Offset, Size);

      // Write zero termination and alignment padding
      Dec(Size, SizeOf(PublicSym32) + NameSize);
      while (Size > 0) do
      begin
        Writer.Write(Byte(0));
        Dec(Size);
      end;

      Assert(Writer.PadToAligment(4) = 0);
    end;

    Result := Writer.BaseStream.Position - Result;
    Assert(Result = Offset);
  end;

  function EmitGlobals(Writer: TBinaryBlockWriter; Records: TList<TCVSymbol>): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    for var Symbol in Records do
    begin
      // TODO : The following is just a guess.
      Writer.Write<TCVTypeRecordHeader>(Symbol.Header);
      Writer.Write(Symbol.Data);
    end;

    Result := Writer.BaseStream.Position - Result;
  end;

  function EmitGlobalsStream(Writer: TBinaryBlockWriter; const HashStreamBuilder: TGSIHashStreamBuilder): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    // Emit hash table
    HashStreamBuilder.Emit(Writer);

    Result := Writer.BaseStream.Position - Result;
  end;

  function EmitPublicsStream(Writer: TBinaryBlockWriter; const HashStreamBuilder: TGSIHashStreamBuilder; const Records: TPublicSymArray): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    // Emit header
    var Header := Default(TPublicsStreamHeader);

    Header.SymHash := HashStreamBuilder.CalculateSerializedLength;
    Header.AddrMap := Length(Records) * SizeOf(Cardinal);
    Assert(IsAligned(SizeOf(Header), 4));
    Assert(IsAligned(Header.SymHash, 4));
    Assert(IsAligned(Header.AddrMap, 4));

    Writer.Write<TPublicsStreamHeader>(Header);


    // Emit hash table
    var Size := HashStreamBuilder.Emit(Writer);

    Assert(Size = Header.SymHash);


    // Build an array of indices into the Publics array, and sort it by
    // address.
    var AddressMap: TArray<Cardinal>;
    SetLength(AddressMap, Length(Records));

    for var Index := 0 to High(Records) do
      AddressMap[Index] := Index;

    // Local var so we can access the param in the comparer
    var RecordsAccess: PPublicSymArray := @Records;

    // Sort address map by Segment.Index, Symbol.Offset - not by Symbol.Address
    // PSGSI1::sortBuf
    //   https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L1945
    // cmpAddrMapByAddrAndName
    //   https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L1476
    TArray.Sort<Cardinal>(AddressMap, TComparer<Cardinal>.Construct(
      function(const A, B: Cardinal): integer
      begin
        var SymA := RecordsAccess^[A];
        var SymB := RecordsAccess^[B];

        Result := integer(SymA.Symbol.Module.Segment.Index) - integer(SymB.Symbol.Module.Segment.Index);

        if (Result = 0) then
          Result := integer(SymA.Symbol.Offset) - integer(SymB.Symbol.Offset);

        if (Result = 0) then
          Result := System.AnsiStrings.CompareStr(SymA.Name, SymB.Name);
      end));

    // Rewrite the public symbol indices into symbol offsets.
    for var Index := 0 to High(AddressMap) do
      //
      // The reference implementation states:
      //   "ptrs in the stream are offsets biased by one to distinguish null ptrs/offsets"
      //   https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/gsi.cpp#L1979
      // However this only applies to the offsets in the hash table. Not the address map.
      //
      AddressMap[Index] := Records[AddressMap[Index]].SymOffset;


    // Emit address table
    Writer.WriteArray<Cardinal>(AddressMap);


    // We do not emit the "thunk map" and "section map" chunks... yet


    Result := Writer.BaseStream.Position - Result;

    Assert(Result = SizeOf(TPublicsStreamHeader) + Header.SymHash + Header.AddrMap);
  end;

  procedure LoadSymbols;
  begin
    var Symbols := TList<TDebugInfoSymbol>.Create;
    try

      // Create a list of symbols and sort it by name.
      // This also gives us the symbol count so we can preallocate the symbol array.
      for var Module in FDebugInfo.Modules do
      begin
        Module.Symbols.CalculateSizes;

        for var Symbol in Module.Symbols do
          if (Symbol.Size > 0) then
            Symbols.Add(Symbol);
      end;
      SetLength(Publics, Symbols.Count);

      Symbols.Sort(TComparer<TDebugInfoSymbol>.Construct(
        function(const A, B: TDebugInfoSymbol): integer
        begin
          // MS appears to sort by address or segment+offset - not by name
          Result := integer(A.Module.Segment.Index) - integer(B.Module.Segment.Index);

          if (Result = 0) then
            Result := integer(A.Module.Offset + A.Offset) - integer(B.Module.Offset + B.Offset);

          if (Result = 0) then
            Result := CompareStr(A.Name, B.Name);
        end));

      // Populate symbol array
      var Index := 0;
      for var Symbol in Symbols do
      begin
        Publics[Index].PublicSym32.Header.RecordKind := Ord(CVSymbolRecordKind.S_PUB32);
        Publics[Index].PublicSym32.Segment := Symbol.Module.Segment.Index;
        Publics[Index].PublicSym32.Offset := Symbol.Module.Offset + Symbol.Offset; // Make offset relative to segment
        Publics[Index].PublicSym32.Flags := Ord(CVPubSymFlags.cvpsfFunction);

        Publics[Index].Symbol := Symbol;
        Publics[Index].Name := AnsiString(Symbol.Name);

        Inc(Index);
      end;

    finally
      Symbols.Free;
    end;
  end;

begin
  FLayout.StreamDBIPublics := FFiler.AllocateStream;
  FLayout.StreamDBIGlobals := FFiler.AllocateStream;
  FLayout.StreamDBISymbols := FFiler.AllocateStream;

  Globals := TList<TCVSymbol>.Create; // Presently not populated but we need the instance anyway
  try

    // Add symbols
    LoadSymbols;


    // Emit symbols
    var PublicsSize: Cardinal;
    FLayout.StreamDBISymbols.BeginStream;
    begin
      // Write public symbol records first, followed by global symbol records.  This
      // must match the order of BuildPublicsHash/BuildGlobalsHash below.
      PublicsSize := EmitPublics(FLayout.StreamDBISymbols.Writer, Publics);
      EmitGlobals(FLayout.StreamDBISymbols.Writer, Globals);
    end;
    FLayout.StreamDBISymbols.EndStream;


    // Build Publics and Globals hash tables
    var PublicsHashStreamBuilder: TGSIHashStreamBuilder;
    var GlobalsHashStreamBuilder: TGSIHashStreamBuilder;

    BuildPublicsHash(PublicsHashStreamBuilder);
    BuildGlobalsHash(GlobalsHashStreamBuilder, PublicsSize);


    // Emit Globals Stream
    FLayout.StreamDBIGlobals.BeginStream;
    begin
      EmitGlobalsStream(FLayout.StreamDBIGlobals.Writer, GlobalsHashStreamBuilder);
    end;
    FLayout.StreamDBIGlobals.EndStream;


    // Emit Publics Stream
    FLayout.StreamDBIPublics.BeginStream;
    begin
      EmitPublicsStream(FLayout.StreamDBIPublics.Writer, PublicsHashStreamBuilder, Publics);
    end;
    FLayout.StreamDBIPublics.EndStream;

  finally
    Globals.Free;
  end;
end;


procedure TDebugInfoPdbWriter.WriteDBIModuleSymbols;

  procedure DoEmitModuleSymbols(Module: TDebugInfoModule);
  begin
    var ModuleLayout := Default(TModuleLayout);

    ModuleLayout.Stream := FFiler.AllocateStream;
    ModuleLayout.Stream.BeginStream;
    begin

      EmitDBIModuleSymbol(ModuleLayout.Stream.Writer, Module, ModuleLayout);

    end;
    ModuleLayout.Stream.EndStream;

    // Save module info for use in EmitDBISubstreamModules (i.e. the DBI Module Info Substream)
    FModuleLayout.Add(Module, ModuleLayout);
  end;

begin
  for var Module in FDebugInfo.Modules do
    DoEmitModuleSymbols(Module);

  // Emit special "* Linker *" module
  DoEmitModuleSymbols(FLinkerModule);
end;


function TDebugInfoPdbWriter.EmitModuleSymbols(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;

  procedure EmitSectionSymbol(Section: TDebugInfoSegment);
  begin
    var Symbol := Default(TCVTypeSectionSym);

    Symbol.Header.RecordLen := AlignTo(SizeOf(Symbol) + Length(Section.Name) + 1, 4) - SizeOf(Symbol.Header.RecordLen);
    Symbol.Header.RecordKind := Ord(CVSymbolRecordKind.S_SECTION);
    Symbol.SectionNumber := Section.Index;
    Symbol.Alignment := 12; // This is Log2(Alignment), 2^12 = 4096
    Symbol.Rva := Section.Offset;
    Symbol.Length := Section.Size;
    Symbol.Characteristics := Section.Characteristics;

    Writer.Write<TCVTypeSectionSym>(Symbol);
    Writer.Write(Section.Name);

    Writer.PadToAligment(4);
  end;

  procedure EmitLinkerSymbol;
  const
    sLinkerName: AnsiString = 'map2pdb';
  begin
    var Symbol := Default(TCVTypeCompile3Sym);

    Symbol.Header.RecordLen := AlignTo(SizeOf(Symbol) + Length(sLinkerName) + 1, 4) - SizeOf(Symbol.Header.RecordLen) ;
    Symbol.Header.RecordKind := Ord(CVSymbolRecordKind.S_COMPILE3);
    Symbol.Flags := Ord(CVSourceLanguage.Link);
    Symbol.Machine := Ord(CVCPUType.CV_CFL_X64);

    // Interestingly, if we set the backend version to 0.0.0.0, then when trying
    // to view local variables WinDbg emits an error that private symbols are not
    // present. By setting this to a valid MSVC linker version string, local
    // variables are displayed properly.
    Symbol.VersionBackendBuild := 25019;
    Symbol.VersionBackendMajor := 14;
    Symbol.VersionBackendMinor := 10;
    Symbol.VersionBackendQFE := 0;

    // MSVC also sets the frontend to 0.0.0.0 since this is specifically for the
    // linker module (which is by definition a backend), so we don't need to do
    // anything here.  Also, it seems we can use "FOOBAR Linker" for the linker
    // name without any problems.  Only the backend version has to be hardcoded
    // to a magic number.

    Writer.Write<TCVTypeCompile3Sym>(Symbol);
    Writer.Write(sLinkerName);

    Writer.PadToAligment(4);
  end;

  procedure EmitObjNameSymbol;
  begin
    var Symbol := Default(TCVTypeObjNameSym);

    Symbol.Header.RecordLen := Alignto(SizeOf(Symbol) + Length(Module.Name) + 1, 4) - SizeOf(Symbol.Header.RecordLen);
    Symbol.Header.RecordKind := Ord(CVSymbolRecordKind.S_OBJNAME);

    Writer.Write<TCVTypeObjNameSym>(Symbol);
    Writer.Write(Module.Name);

    Writer.PadToAligment(4);
  end;

begin
  Result := Writer.BaseStream.Position;

  EmitObjNameSymbol;

  if (Module is TDebugInfoLinkerModule) then
  begin
    // Emit S_COMPILE3
    EmitLinkerSymbol;

    // Emit S_SECTION for all segments
    for var Segment in FDebugInfo.Segments do
      EmitSectionSymbol(Segment);
  end else
//    EmitSectionSymbol(Module.Segment);
    {nothing yet...};
  // TODO : S_COFFGROUP
  // TODO : Method symbols: S_GPROC32

  // Both LLVM & MS aligns the symbol substream
  Writer.PadToAligment(4);

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitStringTable(Writer: TBinaryBlockWriter; Strings: TStrings): Cardinal;
const
  EmptyBucket: Cardinal = 0;
begin
  Result := Writer.BaseStream.Position;

  // Setup hash table with a load factor of 80%
  var Buckets: Cardinal := Ceil(FStringTable.Count * 1.25) + 1; // +1 because we have reserved slot 0
  // It seems LLVM uses a factor of 1.75 (load factor 57%) here instead of 1.25
  //var Buckets: Cardinal := Ceil(Strings.Count * 1.75) + 1; // +1 because we have reserved slot 0

  var HashTable: TArray<Cardinal>;
  SetLength(HashTable, Buckets);
  for var Index := Low(HashTable) to High(HashTable) do
    HashTable[Index] := EmptyBucket;


  // Header
  var StringTableHeader := Default(TPDBStringTableHeader);
  StringTableHeader.Signature := PDBStringTableSignature;
  // LLVM claim that they only support V2 hashing but llvm-pdbutil uses V1 here. Our implementation
  // supports both.
  StringTableHeader.HashVersion := 1;
  // LLVM starts with an empty string and places that in slot zero. Probably so they can use zero
  // as the empty bucket marker. This also means that we can use an offset of zero to mean "no name"
  // the places where offsets are used.
  StringTableHeader.ByteSize := 1;

  // Sum size of all source file names across all modules and populate hash
  // table while we do it
  for var Index := 0 to Strings.Count-1 do
  begin
    var Filename := AnsiString(Strings[Index]);

    var Hash: Cardinal;
    if (StringTableHeader.HashVersion = 1) then
      Hash := HashStringV1(Filename) mod Buckets
    else
      Hash := HashStringV2(Filename) mod Buckets;

    while (Hash = 0) or (HashTable[Hash] <> EmptyBucket) do
      Hash := (Hash + 1) mod Buckets; // Collision - linear probe for next bucket

    HashTable[Hash] := StringTableHeader.ByteSize; // Store string offset in bucket

    // Verify that the offset we previously precomputed is correct
    Assert(Cardinal(Strings.Objects[Index]) = StringTableHeader.ByteSize);

    // Sum size
    Inc(StringTableHeader.ByteSize, Length(Filename)+1);
  end;


  // Emit header
  Writer.Write<TPDBStringTableHeader>(StringTableHeader);


  // Emit names of all source files across all modules
  Writer.Write(Byte(0)); // First the empty string
  for var Filename in Strings do
    Writer.Write(Filename);


  // Emit hash table
  Writer.Write(Buckets); // Hash count
  Writer.WriteArray<Cardinal>(HashTable);


  // Emit epilogue
  Writer.Write(Cardinal(Strings.Count)); // Name count


  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitSubsectionC13SourceFileLines(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;

  function EmitSourceLines(SourceFile: TDebugInfoSourceFile; FileIndex: Cardinal): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    // Note that for simplicity we emit a header even for source files that have no lines.
    // This means that we can rely on that for any module we will emit Module.SourceFiles.Count
    // entries - for example when assigning a value to TModInfo.SourceFileCount.


    // Emit header
    var Header := Default(TCVLineBlockFragmentHeader);
    // NameIndex is an offset into the checksum array written in EmitSubsectionFileChecksums.
    // We can calculate the offset, instead of having to look it up in a shared list, because
    // we know that we use FileChecksumKind.None for all entries and that entries are written
    // in the same order here and in EmitSubsectionFileChecksums.
    Header.NameIndex := FileIndex * AlignTo(SizeOf(TCVFileChecksumEntryHeader), 4);
    for var SourceLine in Module.SourceLines do
      if (SourceLine.SourceFile = SourceFile) then
        Inc(Header.NumLines);
    Header.BlockSize := SizeOf(TCVLineBlockFragmentHeader) + Header.NumLines * SizeOf(TCVLineNumberEntry);

    Writer.Write<TCVLineBlockFragmentHeader>(Header);


    // FIXME: CVDUMP complains when we emit a source file's block of lines that overlap
    // with the lines of another source file. We should probably block lines so there's
    // no overlap but I haven't figured out how to output that properly.

    // Emit source lines. They *must* be ordered by line number.
    for var SourceLine in Module.SourceLines do
      if (SourceLine.SourceFile = SourceFile) then
      begin
        var Line: TCVLineNumberEntry;
        Line.Offset := SourceLine.Offset; // Line.Offset is relative to TCVLineFragmentHeader.RelocOffset (Module.Offset)
        Line.Flags := Cardinal(SourceLine.LineNumber or Ord(CVLineMask.StatementFlag));
        Writer.Write<TCVLineNumberEntry>(Line);
      end;

    Result := Writer.BaseStream.Position - Result;
  end;

begin
  if (Module is TDebugInfoLinkerModule) then
    Exit(0);

  Result := Writer.BaseStream.Position;


  // Module header
  var Header: TCVLineFragmentHeader;
  Header.RelocOffset := Module.Offset;
  Header.RelocSegment := Module.Segment.Index;
  Header.Flags := Ord(CVLineFragmentFlags.LF_None); // No columns
  Header.CodeSize := Module.Size;

  Writer.Write<TCVLineFragmentHeader>(Header);


  // Each source file and its lines
  var SourceFileIndex: Cardinal := 0;
  for var SourceFile in Module.SourceFiles do
  begin
    EmitSourceLines(SourceFile, SourceFileIndex);
    Inc(SourceFileIndex);
  end;

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitSubsectionFileChecksums(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;

  function EmitSourceFileChecksum(SourceFile: TDebugInfoSourceFile): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    // Find the source filename in the string table so we can get the offset
    // of the name in the stringlist.
    var FileIndex := FStringTable.IndexOf(SourceFile.Filename);
    Assert(FileIndex <> -1);

    // Emit header
    var Header := Default(TCVFileChecksumEntryHeader);
    Header.FileNameOffset := Cardinal(FStringTable.Objects[FileIndex]);
    Header.ChecksumSize := 0;
    Header.ChecksumKind := Ord(FileChecksumKind.None);

    Writer.Write<TCVFileChecksumEntryHeader>(Header);

    Writer.PadToAligment(4);

    Result := Writer.BaseStream.Position - Result;
  end;

begin
  if (Module is TDebugInfoLinkerModule) then
    Exit(0);

  Result := Writer.BaseStream.Position;

  // Each source file and its lines
  for var SourceFile in Module.SourceFiles do
    EmitSourceFileChecksum(SourceFile);

  Result := Writer.BaseStream.Position - Result;
end;

function TDebugInfoPdbWriter.EmitDBIModuleSymbol(Writer: TBinaryBlockWriter; Module: TDebugInfoModule; var ModuleLayout: TModuleLayout): Cardinal;
type
  TSubSectionEmitter = reference to function(Writer: TBinaryBlockWriter; Module: TDebugInfoModule): Cardinal;

  function EmitSubsection(Kind: DebugSubsectionKind; SubSectionEmitter: TSubSectionEmitter): Cardinal;
  begin
    Result := Writer.BaseStream.Position;

    // Write subsection data then go back and write header
    var HeaderPos := Writer.BaseStream.Position;
    Writer.BaseStream.Position := Writer.BaseStream.Position + SizeOf(TCVDebugSubsectionHeader);


    // Emit subsection
    var DataSize := SubSectionEmitter(Writer, Module);
    Assert(DataSize = Writer.BaseStream.Position - HeaderPos - SizeOf(TCVDebugSubsectionHeader));

    if (DataSize = 0) then
    begin
      Writer.BaseStream.Position := HeaderPos;
      Exit(0);
    end;

    Writer.PadToAligment(4);


    // Go back and write subsection header
    var SavePos := Writer.BaseStream.Position;
    Writer.BaseStream.Position := HeaderPos;


    var SubsectionHeader := Default(TCVDebugSubsectionHeader);
    SubsectionHeader.Kind := Ord(Kind);
    SubsectionHeader.Length := AlignTo(DataSize, 4);
    Writer.Write<TCVDebugSubsectionHeader>(SubsectionHeader);

    Writer.BaseStream.Position := SavePos;

    Result := Writer.BaseStream.Position - Result;
  end;

begin
  Result := Writer.BaseStream.Position;

  var ModiStream := Default(TModiStream);
  ModiStream.Signature := Ord(CVSignature.CV_SIGNATURE_C13);
  Writer.Write<TModiStream>(ModiStream);


  // TModiStream.Symbols: The CodeView Symbols Substream
  ModuleLayout.SymByteSize := EmitModuleSymbols(Writer, Module) + SizeOf(TModiStream);


  // TModiStream.C13LineInfo:
  // - CodeView line information in C13 format
  // - Source file checksums
  ModuleLayout.C13ByteSize :=
    EmitSubsection(DebugSubsectionKind.Lines, EmitSubsectionC13SourceFileLines) +
    EmitSubsection(DebugSubsectionKind.FileChecksums, EmitSubsectionFileChecksums);

  // TModiStream.GlobalRefs: Not supported so TModiStream.GlobalRefsSize=0
  Writer.Write(Cardinal(0));

  Result := Writer.BaseStream.Position - Result;

  Assert(Result = ModuleLayout.SymByteSize + ModuleLayout.C13ByteSize + SizeOf(Cardinal));
end;


function TDebugInfoPdbWriter.WritePDBStrings: TMSFStream;
begin
  Result := FFiler.AllocateStream;

  FNamedStreams.Add('/names', Result);

  // Writes the global string list
  Result.BeginStream;
  begin

    EmitStringTable(Result.Writer, FStringTable);

  end;
  Result.EndStream;

end;


procedure TDebugInfoPdbWriter.SaveToStream(Stream: TStream; DebugInfo: TDebugInfo);
begin
  FDebugInfo := DebugInfo;

  FModuleLayout.Clear;
  FNamedStreams.Clear;
  FLayout := Default(TPDBFFileLayout);

  Log('Constructing PDB file');

  FFiler := TMSFFile.Create(Stream, FBlockSize);
  try

    // Begin the MSF transaction
    FFiler.BeginFile;


    FLinkerModule := TDebugInfoLinkerModule.Create(FDebugInfo, sLinkerModuleName, FDebugInfo.Segments[1], 0, 0);
    try

      // Populate string list with all source file names. The list is emitted in WritePDBStrings.
      Log('- Collecting source file names');
      PopulateStringList;

      // Write the Module Symbols stream.
      // We need to write this before the DBI Info stream because the DBI Module Info Substream
      // contains values that are calculated when we write the symbols.
      Log('- Module streams');
      WriteDBIModuleSymbols;


      // Write the global string list
      // The indices and offsets of the string list must be stable before the DBI Module Info Substream
      // (WriteDBIStream->EmitDBISubstreamModules) and the Module info streams (WriteDBIModuleSymbols) are written
      // as they emit these values.
      Log('- Strings stream');
      WritePDBStrings;
// FIXME: MSF corrupts when an empty /LinkInfo stream is added
//!!!      FNamedStreams.Add('/LinkInfo', FFiler.AllocateStream);


      // Write the PDB info stream
      // This persists the FNamedStreams list and therefore marks it FNamedStreams read-only.
      Log('- PDB Info stream');
      FLayout.StreamPDB := WritePDBInfoStream;


      // Write the TPI stream
      Log('- TPI stream');
      FLayout.StreamTPI := WriteTPIStream;


      // Write the Symbols (DBI), Globals (DBI) and Publics (DBI) streams - they must be writte before the DBI stream
      Log('- Symbols stream');
      WriteSymbols;


      // Write the DBI stream
      Log('- DBI stream');
      FLayout.StreamDBI := WriteDBIStream;


      // Write the IPI stream
      Log('- IPI stream');
      FLayout.StreamIPI := WriteIPIStream;


    finally
      FLinkerModule.Free;
    end;

    // Commit the MSF transaction
    Log('- Finalizing PDB file');
    FFiler.EndFile;

  finally
    FreeAndNil(FFiler);
  end;

  if (Stream.Size > 4096*FBlockSize) then
    Warning(Format('The PDB file is too large: %.1n Mb. This version only supports PDB files no larger than %.1n Mb', [Stream.Size / 1024 / 1024, 4096 * FBlockSize / 1024 / 1024]));
end;


// -----------------------------------------------------------------------------
//
//      TDebugInfoPdbWriter.TNamedStreamList
//
// -----------------------------------------------------------------------------
function TDebugInfoPdbWriter.TNamedStreamList.Add(const Name: AnsiString; Stream: TMSFStream): integer;
begin
  Assert(not ReadOnly);

  var Entry: TNamedStreamEntry;

  Entry.Name := Name;
  Entry.Stream := Stream;

  Result := Add(Entry);
end;

end.

