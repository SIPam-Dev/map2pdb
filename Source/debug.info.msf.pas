unit debug.info.msf;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface


{$SCOPEDENUMS ON}

uses
  System.Generics.Collections,
  System.Classes,
  debug.info.pdb;


// -----------------------------------------------------------------------------
//
//      TBinaryBlockWriter
//
// -----------------------------------------------------------------------------
// A binary writer with block knowledge.
// -----------------------------------------------------------------------------
type
  TBinaryBlockWriter = class(TBinaryWriter)
  strict private
    FBlockSize: Cardinal;
  strict private
    function GetBlockCount: Cardinal;
    function GetIntervalIndex: Cardinal;
    function GetBlockIndex: Cardinal;
    procedure SetBlockIndex(const Value: Cardinal);
  public
    constructor Create(AStream: TStream; ABlockSize: Cardinal);

    // Writes strings as zero terminated ansi strings and does not write the length byte/word
    procedure Write(const Str: string); overload; override;
    procedure Write(const Str: AnsiString); overload;
    // Write a record (or any other typed value)
    procedure Write<T>(const Value: T); overload;
    // Write a dynamic array
    procedure WriteArray<T>(const Values: TArray<T>);
    // Shortcut to BaseStream.WriteBuffer
    procedure WriteBuffer(const Buffer; Count: NativeInt);

    function BeginBlock(Poison: boolean = False): Cardinal;
    procedure EndBlock(Expand: boolean = False);

    function PadToAligment(Alignment: Cardinal; Poison: boolean = False): Int64; // Returns amount written
    function WritePadding(DesiredPos: Int64; Poison: boolean = False): Int64; // Returns amount written

    property BlockSize: Cardinal read FBlockSize;
    property BlockCount: Cardinal read GetBlockCount;
    property BlockIndex: Cardinal read GetBlockIndex write SetBlockIndex;
    property IntervalIndex: Cardinal read GetIntervalIndex;
  end;


// -----------------------------------------------------------------------------
//
//      TMSFStream
//
// -----------------------------------------------------------------------------
// Encapsulates a MSF stream
// -----------------------------------------------------------------------------
type
  TMSFFile = class;

  // Take care when writing stream index values as some places we write the values
  // as DWORDs (e.g. hash tables).
  TMSFStreamIndex = Word;

  TMSFStreamState = (
    ssAllocated,        // Stream has been allocated
    ssIndex,            // A stream index has been assigned
    ssOpen,             // Stream has been opened
    ssClosed            // Stream has been closed
  );

  TMSFStream = class
  public
    const NullBlockIndex = $FFFFFFFF;
    const NullStreamIndex: TMSFStreamIndex = $FFFF;
  strict private
    FMSFFile: TMSFFile;
    FWriter: TBinaryBlockWriter;
    FState: TMSFStreamState;
    FIndex: TMSFStreamIndex;
    FBlockIndex: Cardinal;
    FLength: Cardinal;
    FOffset: Int64;
  strict private
    function GetIsFixed: boolean;
    function GetIsValid: boolean;
    function GetIndex: TMSFStreamIndex;
    function GetWriter: TBinaryBlockWriter;
    function GetHasIndex: boolean;
  public
    constructor Create(AMSFFile: TMSFFile);

    // Opens the stream.
    // Returns the start block index of the stream (same as the BlockIndex property).
    function BeginStream(Poison: boolean = False): Cardinal;

    // Closes the stream.
    // Returns the size of data writte to the stream (same as the Length property).
    function EndStream: Cardinal;

    // Mark the stream in-use.
    // This ensures that the stream index is allocated and that the stream
    // will be persisted in the directory even if the stream hasn't been opened
    // or written to.
    procedure Touch;

    property Writer: TBinaryBlockWriter read GetWriter;

    // Stream index
    property Index: TMSFStreamIndex read GetIndex;

    // Start byte offset within MSF file
    property Offset: Int64 read FOffset;
    // Start MSF block index
    property BlockIndex: Cardinal read FBlockIndex;
    // Length of stream in bytes
    property Length: Cardinal read FLength;

    // IsFixed: True if stream is one of the fixed streams
    property IsFixed: boolean read GetIsFixed;
    // IsValid: True if stream has been opened or the Index value referenced
    property IsValid: boolean read GetIsValid;

    property HasIndex: boolean read GetHasIndex;
    property State: TMSFStreamState read FState;
  end;


// -----------------------------------------------------------------------------
//
//      TMSFFile
//
// -----------------------------------------------------------------------------
// Encapsulates a MSF container file
// -----------------------------------------------------------------------------
  TMSFFile = class
  strict private type
    TFileState = (fsInit, fsOpen, fsClosed);
  strict private
    FState: TFileState;
    FStreams: TList<TMSFStream>;
    FWriter: TBinaryBlockWriter;
    FBlockSize: Cardinal;
    FCurrentStream: TMSFStream;
    FNextStreamIndex: TMSFStreamIndex;
  strict private
    procedure WriteSuperBlock(IndexStream, DirectoryStream: TMSFStream);
    procedure WriteBlockMap;
    procedure WriteStreamBlockList(Writer: TBinaryBlockWriter; Stream: TMSFStream);
    procedure WriteDirectoryIndex(IndexStream, DirectoryStream: TMSFStream);
    procedure WriteDirectory(DirectoryStream: TMSFStream);

    function GetCount: integer;
    function GetStream(Index: TMSFStreamIndex): TMSFStream;
    function GetFixedStream(Index: PDBStreamIndex): TMSFStream;
  protected

    function BeginStream(Stream: TMSFStream): TBinaryBlockWriter;
    procedure EndStream(Stream: TMSFStream);

    function AllocateStreamIndex: TMSFStreamIndex;
  public
    constructor Create(AStream: TStream; ABlockSize: Cardinal);
    destructor Destroy; override;

    procedure BeginFile;
    procedure EndFile;

    function AllocateStream: TMSFStream;

    property Writer: TBinaryBlockWriter read FWriter;
    property CurrentStream: TMSFStream read FCurrentStream;

    property Count: integer read GetCount;
    property Streams[Index: TMSFStreamIndex]: TMSFStream read GetStream; default;
    property FixedStreams[Index: PDBStreamIndex]: TMSFStream read GetFixedStream;

    function GetEnumerator: TEnumerator<TMSFStream>;
  end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.Generics.Defaults,
  System.Math;


// -----------------------------------------------------------------------------
//
//      TMSFFile
//
// -----------------------------------------------------------------------------
constructor TMSFFile.Create(AStream: TStream; ABlockSize: Cardinal);
begin
  inherited Create;

  FBlockSize := ABlockSize;
  FStreams := TObjectList<TMSFStream>.Create(True);
  FWriter := TBinaryBlockWriter.Create(AStream, FBlockSize);
end;

destructor TMSFFile.Destroy;
begin
  FWriter.Free;
  FStreams.Free;

  inherited;
end;

function TMSFFile.AllocateStreamIndex: TMSFStreamIndex;
begin
  Result := FNextStreamIndex;
  Inc(FNextStreamIndex);
end;

procedure TMSFFile.BeginFile;
begin
  Assert(FState = TFileState.fsInit);
  FState := TFileState.fsOpen;


  // Preallocate the fixed streams so their indices aren't used by other streams
  for var StreamIndex := Low(PDBStreamIndex) to High(PDBStreamIndex) do
    // Allocate and touch the stream so the stream index is assigned
    AllocateStream.Touch;


  // The MSF Superblock occupies block #0 - we write that last so skip it now
  FWriter.BlockIndex := 1;


  // Write the two Free Block Maps (FBM). Block #1 and #2
  WriteBlockMap;
  Assert(FWriter.BlockIndex = 2);

  WriteBlockMap;
  Assert(FWriter.BlockIndex = 3);

  // Write an empty block just so our layout matches LLVM.
  // This can be removed once our output is validated.
  FWriter.BeginBlock;
  FWriter.Write(Byte(0));
  FWriter.EndBlock;


  // The FBM exists either at block 1 or block 2 of the MSF.  However, this
  // allows for a maximum of BlockSize * 8 blocks bits in the FBM, and
  // thusly an equal number of total blocks in the file.  For a block size
  // of 4Kb (very common), this would yield 32Kb total blocks in file, for a
  // maximum file size of 32Kb * 4Kb = 128Mb.  Obviously this won't do, so
  // the FBM is split across the file at `BlockSize` intervals.  As a
  // result, every block whose index is of the form |{1,2} + BlockSize * k|
  // for any non-negative integer k is an FBM block.  In theory, we only really
  // need to reserve blocks of the form |{1,2} + BlockSize * 8 * k|, but
  // current versions of the MSF format already expect the FBM to be arranged
  // at BlockSize intervals, so we have to be compatible.
  // See the function fpmPn() for more information:
  // https://github.com/Microsoft/microsoft-pdb/blob/master/PDB/msf/msf.cpp#L489

end;

procedure TMSFFile.EndFile;
begin
  Assert(FState = TFileState.fsOpen);

  Assert(FCurrentStream = nil);

  // Create streams directly so they do not appear in the stream list
  var IndexStream := TMSFStream.Create(Self);
  var DirectoryStream := TMSFStream.Create(Self);
  try

    // Sort streams in Index order for the directory
    FStreams.Sort(TComparer<TMSFStream>.Construct(
      function(const A, B: TMSFStream): integer
      begin
        if (A.HasIndex) then
        begin
          if (not B.HasIndex) then
            Exit(-1);
        end else
        if (not B.HasIndex) then
          Exit(1);

        // Both indices are valid
        Result := integer(A.Index) - integer(B.Index);
      end));


    // Now that all other streams have been written we can write the MSF directory stream.
    WriteDirectory(DirectoryStream);


    // Write a pointer to the directory stream.
    // The MSF Superblock will point to this stream.
    WriteDirectoryIndex(IndexStream, DirectoryStream);


    // The above was the last block so make sure file is physically padded.
    FWriter.BeginBlock;
    FWriter.EndBlock(True);


    // Rewind and write the MSF Superblock at block #0.
    // At this point all streams, and the directory, must have been written.
    Writer.BlockIndex := 0;
    WriteSuperBlock(IndexStream, DirectoryStream);

  finally
    IndexStream.Free;
    DirectoryStream.Free;
  end;

  FState := TFileState.fsClosed;
end;

function TMSFFile.AllocateStream: TMSFStream;
begin
  Assert(FState = TFileState.fsOpen);

  Result := TMSFStream.Create(Self);

  FStreams.Add(Result);
end;

function TMSFFile.BeginStream(Stream: TMSFStream): TBinaryBlockWriter;
begin
  Assert(FState = TFileState.fsOpen);
  Assert(FCurrentStream = nil);

  FCurrentStream := Stream;

  Result := FWriter;
end;

procedure TMSFFile.EndStream(Stream: TMSFStream);
begin
  Assert(FState = TFileState.fsOpen);
  Assert(FCurrentStream = Stream);

  FCurrentStream := nil;
end;

function TMSFFile.GetCount: integer;
begin
  Result := FStreams.Count;
end;

function TMSFFile.GetEnumerator: TEnumerator<TMSFStream>;
begin
  Result := FStreams.GetEnumerator;
end;

function TMSFFile.GetFixedStream(Index: PDBStreamIndex): TMSFStream;
begin
  Result := nil;
  // The list contains streams in allocation order, not Index order.
  for var i := 0 to FStreams.Count-1 do
    if (FStreams[i].HasIndex) and (FStreams[i].Index = Word(Ord(Index))) then
      Exit(FStreams[i]);
  Assert(False);
end;

function TMSFFile.GetStream(Index: TMSFStreamIndex): TMSFStream;
begin
  Result := nil;
  // The list contains streams in allocation order, not Index order.
  for var i := 0 to FStreams.Count-1 do
    if (FStreams[i].HasIndex) and (FStreams[i].Index = Index) then
      Exit(FStreams[i]);
  Assert(False);
end;

procedure TMSFFile.WriteStreamBlockList(Writer: TBinaryBlockWriter; Stream: TMSFStream);
begin
  // Take advantage of the fact that in our case all blocks within a stream are contiguous
  var BlockCount := Ceil(Stream.Length / FBlockSize);
  var BlockIndex := Stream.BlockIndex;
  while (BlockCount > 0) do
  begin
    Assert(BlockIndex <> TMSFStream.NullBlockIndex);
    Writer.Write(BlockIndex);

    Dec(BlockCount);
    Inc(BlockIndex);
  end;
end;

procedure TMSFFile.WriteSuperBlock(IndexStream, DirectoryStream: TMSFStream);
var
  MSFSuperBlock: TMSFSuperBlock;
begin
  MSFSuperBlock.Magic := MSFMagic;
  MSFSuperBlock.BlockSize := FBlockSize;
  MSFSuperBlock.FreeBlockMapBlock := 1; // 1 or 2. In our case it's always block #1
  MSFSuperBlock.NumBlocks := FWriter.BlockCount;
  MSFSuperBlock.NumDirectoryBytes := DirectoryStream.Length;
  MSFSuperBlock.Unknown := 0;
  MSFSuperBlock.BlockMapAddr := IndexStream.BlockIndex;

  FWriter.Write<TMSFSuperBlock>(MSFSuperBlock);
end;

procedure TMSFFile.WriteBlockMap;
begin
  FWriter.BeginBlock;
  Assert(FWriter.BlockIndex in [1, 2]);

  // Mark all BlockSize*8 blocks occupied
  for var i := 0 to FBlockSize-1 do
    FWriter.Write(Byte($FF));

  FWriter.EndBlock;
end;


procedure TMSFFile.WriteDirectoryIndex(IndexStream, DirectoryStream: TMSFStream);
begin
  IndexStream.BeginStream;

  // Write list of blocks of where the directory actually lives
  WriteStreamBlockList(IndexStream.Writer, DirectoryStream);

  IndexStream.EndStream;
end;


procedure TMSFFile.WriteDirectory(DirectoryStream: TMSFStream);
begin
  DirectoryStream.BeginStream;

  // Disregard streams that has been allocated but not opened (except fixed streams).

  // Number of streams
  var Count: Cardinal := 0;
  for var Stream in FStreams do
    if (Stream.IsValid) or (Stream.IsFixed) then
    begin
      Assert(Stream.State in [TMSFStreamState.ssIndex, TMSFStreamState.ssClosed]);
      Inc(Count);
    end;
  DirectoryStream.Writer.Write(Cardinal(Count));

  // Stream sizes
  for var Stream in FStreams do
    if (Stream.IsValid) or (Stream.IsFixed) then
      DirectoryStream.Writer.Write(Stream.Length);

  // Stream blocks
  for var Stream in FStreams do
    if (Stream.IsValid) then
      WriteStreamBlockList(DirectoryStream.Writer, Stream);

  DirectoryStream.EndStream;
end;

// -----------------------------------------------------------------------------
//
//      TMSFStream
//
// -----------------------------------------------------------------------------
constructor TMSFStream.Create(AMSFFile: TMSFFile);
begin
  inherited Create;

  FMSFFile := AMSFFile;
  FIndex := NullStreamIndex;
  FBlockIndex := NullBlockIndex;
end;

function TMSFStream.BeginStream(Poison: boolean): Cardinal;
begin
  Assert(FWriter = nil);
  Assert(FState in [TMSFStreamState.ssAllocated, TMSFStreamState.ssIndex]);

  Touch;

  FWriter := FMSFFile.BeginStream(Self);

  FBlockIndex := FWriter.BeginBlock(Poison);
  FOffset := FWriter.BaseStream.Position;
  FState := TMSFStreamState.ssOpen;

  Result := FBlockIndex;
end;

function TMSFStream.EndStream: Cardinal;
begin
  Assert(FWriter <> nil);
  Assert(FState = TMSFStreamState.ssOpen);

  FLength := Cardinal(FWriter.BaseStream.Position - FOffset);
  FWriter.EndBlock;

  FMSFFile.EndStream(Self);
  FWriter := nil;
  FState := TMSFStreamState.ssClosed;

  Result := FLength;
end;

function TMSFStream.GetHasIndex: boolean;
begin
  Result := (FIndex <> NullStreamIndex);
end;

function TMSFStream.GetIndex: TMSFStreamIndex;
begin
  Touch;

  Result := FIndex;
end;

function TMSFStream.GetIsFixed: boolean;
begin
  Result := (integer(FIndex) <= Ord(High(PDBStreamIndex)));
end;

function TMSFStream.GetIsValid: boolean;
begin
  Result := (FState > TMSFStreamState.ssAllocated);
end;

function TMSFStream.GetWriter: TBinaryBlockWriter;
begin
  Assert(FState = TMSFStreamState.ssOpen);
  Assert(FIndex <> NullStreamIndex);
  Assert(FBlockIndex <> NullBlockIndex);

  Result := FWriter;
end;

procedure TMSFStream.Touch;
begin
  // Stream has now been referenced. Allocate a stream index to ensure the stream will
  // be written to the directory.
  if (FIndex = NullStreamIndex) then
  begin
    Assert(FState = TMSFStreamState.ssAllocated);

    FIndex := FMSFFile.AllocateStreamIndex;
    FState := TMSFStreamState.ssIndex;
  end;

  Assert(FState >= TMSFStreamState.ssIndex);
end;

// -----------------------------------------------------------------------------
//
//      TBinaryBlockWriter
//
// -----------------------------------------------------------------------------

constructor TBinaryBlockWriter.Create(AStream: TStream; ABlockSize: Cardinal);
begin
  inherited Create(AStream);
  FBlockSize := ABlockSize;
end;

function TBinaryBlockWriter.WritePadding(DesiredPos: Int64; Poison: boolean): Int64;
begin
  Result := DesiredPos - BaseStream.Position;
  Assert(Result >= 0);

  if (Result = 0) then
    Exit;

  // CodeView leaf records are either padded to 4 bytes (if this type stream appears in a TPI/IPI stream
  // of a PDB) or not padded at all (if this type stream appears in the .debug$T section of an object
  // file). Padding is implemented by inserting a decreasing sequence of <_padding_records> that
  // terminates with LF_PAD0.

{-$ifdef DEBUG}
  if (Poison) then
  begin
    Assert(Result <= $0F);
    var Count := Result;
    while (Count > 0) do
    begin
//      Write(Byte($0F));
      Write(Byte((Count and $0F) or $F0)); // http://moyix.blogspot.com/2007/10/types-stream.html
      Dec(Count);
    end;
  end else
{-$endif DEBUG}
    // We're allowed to seek past EOF. File will be physically expanded once we
    // actually write something or explicitly set the size.
    // Note however that if we expand the file this way and the close it with out
    // writing anything, then the file is truncated to the point where we last wrote
    // something. EndBlock(Expand=True) can be used to force the stream to be
    // expanded to the current position.
    BaseStream.Seek(Result, soCurrent);
end;

procedure TBinaryBlockWriter.Write(const Str: string);
begin
  // TODO : Handle UTF8 here
  if (Str <> '') then
    Write(AnsiString(Str))
  else
    Write(Byte(0));
end;

procedure TBinaryBlockWriter.Write(const Str: AnsiString);
begin
  if (Str <> '') then
    BaseStream.WriteBuffer(Str[1], Length(Str)+1) // Include terminating zero
  else
    Write(Byte(0));
end;

procedure TBinaryBlockWriter.Write<T>(const Value: T);
begin
  WriteBuffer(Value, SizeOf(T));
end;

procedure TBinaryBlockWriter.WriteArray<T>(const Values: TArray<T>);
begin
  if (Length(Values) = 0) then
    Exit;

  WriteBuffer(Values[0], Length(Values) * SizeOf(T));
end;

procedure TBinaryBlockWriter.WriteBuffer(const Buffer; Count: NativeInt);
begin
  BaseStream.WriteBuffer(Buffer, Count);
end;

function TBinaryBlockWriter.BeginBlock(Poison: boolean): Cardinal;
begin
  // Move up to next block. Return block index.
  PadToAligment(FBlockSize, Poison);
  Result := BlockIndex;
end;

procedure TBinaryBlockWriter.EndBlock(Expand: boolean);
begin
  var BlockIndex: Word;
  var Remainder: Word;

  // In case we have used Seek to align the position then the physical
  // size can be smaller than the current position in which case we will
  // need to move the position back and write something to actually
  // expand then stream.
  if (Expand) and (BaseStream.Position > BaseStream.Size) then
    BaseStream.Position := BaseStream.Size;

  DivMod(BaseStream.Position, FBlockSize, BlockIndex, Remainder);

  // Move up to nearest whole block
  if (Remainder > 0) then
  begin
    var Missing := FBlockSize - Remainder;
    if (Expand) then
    begin
      if (Missing > 1) then
        BaseStream.Position := BaseStream.Position + Missing - 1;
      // Physically expand file
      Write(Byte($00));
    end else
      // Just do a seek. File will be automatically expanded if we write anything
      BaseStream.Position := BaseStream.Position + Missing;
  end;
end;

function TBinaryBlockWriter.GetBlockCount: Cardinal;
begin
  Result := (BaseStream.Size + FBlockSize - 1) div FBlockSize;
end;

function TBinaryBlockWriter.GetBlockIndex: Cardinal;
begin
  Result := BaseStream.Position div FBlockSize;
end;

function TBinaryBlockWriter.GetIntervalIndex: Cardinal;
begin
  Result := BaseStream.Position div FBlockSize div FBlockSize;
end;

function TBinaryBlockWriter.PadToAligment(Alignment: Cardinal; Poison: boolean): Int64;
begin
  var Mask: UInt64 := UInt64(Alignment) - 1;
  Result := Alignment - (UInt64(BaseStream.Position) and Mask);
  if (Result <> Alignment) then
    Result := WritePadding(BaseStream.Position + Result, Poison)
  else
    Result := 0;
end;

procedure TBinaryBlockWriter.SetBlockIndex(const Value: Cardinal);
begin
  BaseStream.Position := Value * FBlockSize;
end;

end.

