unit debug.info;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TDebugInfoOffset = Cardinal;

type
  TDebugInfo = class;
  TDebugInfoModule = class;


  TDebugInfoSegmentClass = (sctCODE, sctICODE, sctDATA, sctBSS, sctTLS, sctPDATA);

  TDebugInfoSegments = class;

  TDebugInfoSegment = class
  private const
    sClassNames: array[TDebugInfoSegmentClass] of string = ('CODE', 'ICODE', 'DATA', 'BSS', 'TLS', 'PDATA');
  private
    FOwner: TDebugInfoSegments;
    FSegClassType: TDebugInfoSegmentClass;
    FName: string;
    FOffset: TDebugInfoOffset;
    FSize: TDebugInfoOffset;
    FIndex: Cardinal;
    FCharacteristics: Cardinal;
  private
    procedure CheckOverlap;
  protected
    function GetSegClassName: string;
    procedure SetOffset(const Value: TDebugInfoOffset);
    procedure SetSize(const Value: TDebugInfoOffset);
  public
    constructor Create(AOwner: TDebugInfoSegments; AIndex: Cardinal; const AClassName: string); overload;
    constructor Create(AOwner: TDebugInfoSegments; AIndex: Cardinal; AClassType: TDebugInfoSegmentClass); overload;

    class function ClassNameToClassType(const AClassName: string): TDebugInfoSegmentClass;

    property Index: Cardinal read FIndex; // 1 based value
    property SegClassType: TDebugInfoSegmentClass read FSegClassType;
    property SegClassName: string read GetSegClassName;
    property Name: string read FName write FName;
    property Offset: TDebugInfoOffset read FOffset write SetOffset;
    property Size: TDebugInfoOffset read FSize write SetSize;
    property Characteristics: Cardinal read FCharacteristics write FCharacteristics; // TODO
  end;

  TDebugInfoSegments = class
  private
    FSegments: TObjectList<TDebugInfoSegment>;
    FNames: TDictionary<string, TDebugInfoSegment>;
  protected
    function GetCount: integer;
    function GetSegment(AIndex: Cardinal): TDebugInfoSegment;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AIndex: Cardinal; AClassType: TDebugInfoSegmentClass; const AName: string): TDebugInfoSegment; overload;
    function Add(AIndex: Cardinal; const AClassName: string; const AName: string): TDebugInfoSegment; overload;
    function FindByIndex(AIndex: Cardinal): TDebugInfoSegment;
    function FindByName(const AName: string): TDebugInfoSegment;
    function FindByClassType(AClassType: TDebugInfoSegmentClass): TDebugInfoSegment;
    function FindByOffset(AOffset: TDebugInfoOffset): TDebugInfoSegment;

    property Count: integer read GetCount;
    property Segments[Index: Cardinal]: TDebugInfoSegment read GetSegment; default;

    function GetEnumerator: TEnumerator<TDebugInfoSegment>;
  end;


  TDebugInfoSourceFile = class
  private
    FFilename: string;
  public
    constructor Create(const AFilename: string);

    property Filename: string read FFilename;
  end;

  TDebugInfoSourceFiles = class
  private type
    TDebugInfoSourceFileList = TObjectDictionary<string, TDebugInfoSourceFile>;
  private
    FSourceFiles: TDebugInfoSourceFileList;
    FOwner: TDebugInfo;
  protected
    function GetCount: integer;
  public
    constructor Create(AOwner: TDebugInfo = nil);
    destructor Destroy; override;

    function Add(const AFilename: string): TDebugInfoSourceFile;
    function First: TDebugInfoSourceFile;

    property Count: integer read GetCount;

    function GetEnumerator: TEnumerator<TDebugInfoSourceFile>;
  end;


  TDebugInfoSourceLine = class
  private
    FSourceFile: TDebugInfoSourceFile;
    FModule: TDebugInfoModule;
    FLineNumber: integer;
    FOffset: TDebugInfoOffset;
  protected
    function GetAddress: TDebugInfoOffset;
  public
    property Module: TDebugInfoModule read FModule write FModule;
    property SourceFile: TDebugInfoSourceFile read FSourceFile write FSourceFile;
    property LineNumber: integer read FLineNumber write FLineNumber;
    // Offset is relative to module
    property Offset: TDebugInfoOffset read FOffset write FOffset;

    // Address = Line.Offset + Line.Module.Offset + Line.Module.Segment.Offset
    property Address: TDebugInfoOffset read GetAddress;
  end;

  TDebugInfoSourceLines = class
  private
    FModule: TDebugInfoModule;
    FSourceLines: TObjectList<TDebugInfoSourceLine>;
    FComparer: IComparer<TDebugInfoSourceLine>;
  protected
    function GetEmpty: boolean;
  public
    constructor Create(AModule: TDebugInfoModule);
    destructor Destroy; override;

    function Add(ASourceFile: TDebugInfoSourceFile; ALineNumber: integer; AOffset: TDebugInfoOffset): TDebugInfoSourceLine;

    property Empty: boolean read GetEmpty;

    function GetEnumerator: TEnumerator<TDebugInfoSourceLine>;
  end;


  TDebugInfoSymbol = class
  private
    FModule: TDebugInfoModule;
    FName: string;
    FOffset: TDebugInfoOffset;
    FSize: TDebugInfoOffset;
  protected
    function GetAddress: TDebugInfoOffset;
  public
    constructor Create(AModule: TDebugInfoModule; const AName: string; AOffset: TDebugInfoOffset);

    property Module: TDebugInfoModule read FModule write FModule;
    property Name: string read FName write FName;
    // Offset is relative to module
    property Offset: TDebugInfoOffset read FOffset;
    property Size: TDebugInfoOffset read FSize write FSize;

    // Address = Symbol.Offset + Symbol.Module.Offset + Symbol.Module.Segment.Offset
    property Address: TDebugInfoOffset read GetAddress;
  end;

  TDebugInfoSymbols = class
  private type
    TDebugInfoSymbolList = TObjectList<TDebugInfoSymbol>;
  private
    FModule: TDebugInfoModule;
    FSymbols: TDebugInfoSymbolList;
    FComparer: IComparer<TDebugInfoSymbol>;
  protected
    function GetCount: integer;
  public
    constructor Create(AModule: TDebugInfoModule);
    destructor Destroy; override;

    function Add(const AName: string; AOffset: TDebugInfoOffset): TDebugInfoSymbol;

    procedure CalculateSizes;

    property Count: integer read GetCount;

    function GetEnumerator: TEnumerator<TDebugInfoSymbol>;
  end;


  TDebugInfoModule = class
  private
    FDebugInfo: TDebugInfo;
    FName: string;
    FObjectName: string;
    FSegment: TDebugInfoSegment;
    FOffset: TDebugInfoOffset;
    FSize: TDebugInfoOffset;
    FSourceFiles: TDebugInfoSourceFiles;
    FSourceLines: TDebugInfoSourceLines;
    FSymbols: TDebugInfoSymbols;
  protected
    function GetObjectName: string;
    function GetAddress: TDebugInfoOffset;
  public
    constructor Create(ADebugInfo: TDebugInfo; const AName: string; ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset);
    destructor Destroy; override;

    property DebugInfo: TDebugInfo read FDebugInfo;

    property Name: string read FName;
    property ObjectName: string read GetObjectName write FObjectName;
    property Segment: TDebugInfoSegment read FSegment;
    // Offset is relative to segment
    property Offset: TDebugInfoOffset read FOffset;
    property Size: TDebugInfoOffset read FSize;

    // Address = Module.Offset + Module.Segment.Offset
    property Address: TDebugInfoOffset read GetAddress;

    property SourceFiles: TDebugInfoSourceFiles read FSourceFiles;
    property SourceLines: TDebugInfoSourceLines read FSourceLines;
    property Symbols: TDebugInfoSymbols read FSymbols;
  end;

  TDebugInfoModules = class
  private type
    TDebugInfoModuleList = TObjectList<TDebugInfoModule>;
  private
    FDebugInfo: TDebugInfo;
    FModules: TDebugInfoModuleList;
    FComparer: IComparer<TDebugInfoModule>;
  protected
    function GetCount: integer;
    function GetEmpty: boolean;
  public
    constructor Create(ADebugInfo: TDebugInfo);
    destructor Destroy; override;

    function Add(const AName: string; ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;
    function FindByName(const AName: string; ASegment: TDebugInfoSegment): TDebugInfoModule;
    function FindByOffset(ASegment: TDebugInfoSegment; AOffset: TDebugInfoOffset): TDebugInfoModule;
    function FindOverlap(ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;

    property Count: integer read GetCount;
    property Empty: boolean read GetEmpty;

    function GetEnumerator: TEnumerator<TDebugInfoModule>;
  end;


  TDebugInfo = class
  private
    FSegments: TDebugInfoSegments;
    FModules: TDebugInfoModules;
    FSourceFiles: TDebugInfoSourceFiles;
  public
    constructor Create;
    destructor Destroy; override;

    property Segments: TDebugInfoSegments read FSegments;
    property Modules: TDebugInfoModules read FModules;
    property SourceFiles: TDebugInfoSourceFiles read FSourceFiles;
  end;


implementation

uses
  Winapi.Windows,
  System.SysUtils;

{ TDebugInfo }

constructor TDebugInfo.Create;
begin
  inherited Create;

  FSegments := TDebugInfoSegments.Create;
  FModules := TDebugInfoModules.Create(Self);
  FSourceFiles := TDebugInfoSourceFiles.Create;
end;

destructor TDebugInfo.Destroy;
begin
  FSegments.Free;
  FModules.Free;
  FSourceFiles.Free;

  inherited;
end;

{ TDebugInfoModule }

constructor TDebugInfoModule.Create(ADebugInfo: TDebugInfo; const AName: string; ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset);
begin
  inherited Create;

  FDebugInfo := ADebugInfo;

  FName := AName;
  FSegment := ASegment;
  FOffset := AOffset;
  FSize := ASize;

  FSourceFiles := TDebugInfoSourceFiles.Create(FDebugInfo);
  FSourceLines := TDebugInfoSourceLines.Create(Self);
  FSymbols := TDebugInfoSymbols.Create(Self);
end;

destructor TDebugInfoModule.Destroy;
begin
  FSourceLines.Free;
  FSourceFiles.Free;
  FSymbols.Free;

  inherited;
end;

function TDebugInfoModule.GetAddress: TDebugInfoOffset;
begin
  Result := Segment.Offset + Offset;
end;

function TDebugInfoModule.GetObjectName: string;
begin
  Result := FObjectName;
  if (Result = '') then
    Result := FName;;
end;

{ TDebugInfoSymbols }

procedure TDebugInfoSymbols.CalculateSizes;
begin
  for var i := 0 to FSymbols.Count-2 do
    FSymbols[i].Size := FSymbols[i+1].Offset - FSymbols[i].Offset;

  // Assume last symbol extends to end of module
  if (FSymbols.Count > 0) then
    FSymbols[FSymbols.Count-1].Size := FModule.Size - FSymbols[FSymbols.Count-1].Offset;
end;

constructor TDebugInfoSymbols.Create(AModule: TDebugInfoModule);
begin
  inherited Create;

  FModule := AModule;
  FSymbols := TDebugInfoSymbolList.Create(True);
end;

destructor TDebugInfoSymbols.Destroy;
begin
  FSymbols.Free;

  inherited;
end;

function TDebugInfoSymbols.GetCount: integer;
begin
  Result := FSymbols.Count;
end;

function TDebugInfoSymbols.GetEnumerator: TEnumerator<TDebugInfoSymbol>;
begin
  Result := FSymbols.GetEnumerator;
end;

function TDebugInfoSymbols.Add(const AName: string; AOffset: TDebugInfoOffset): TDebugInfoSymbol;
begin
  Result := TDebugInfoSymbol.Create(FModule, AName, AOffset);

  if (FComparer = nil) then
    FComparer := TComparer<TDebugInfoSymbol>.Construct(
      function(const Left, Right: TDebugInfoSymbol): integer
      begin
        Result := integer(Left.Offset) - integer(Right.Offset);
      end);

  var Index: integer;
  if (FSymbols.BinarySearch(Result, Index, FComparer)) then
  begin
    // Return existing if we have an exact duplicate of Name+Offset

    if (Result.Name <> AName) then
    begin
      // Disallow duplicate offset but don't fail
      Result.Free;
      Result := nil;
    end;
  end else
    FSymbols.Insert(Index, Result);
end;

{ TDebugInfoModules }

type
  TDebugInfoModuleComparer = class(TComparer<TDebugInfoModule>)
  public
    function Compare(const Left, Right: TDebugInfoModule): Integer; override;

    class function New: IComparer<TDebugInfoModule>;
  end;

class function TDebugInfoModuleComparer.New: IComparer<TDebugInfoModule>;
begin
  Result := TDebugInfoModuleComparer.Create;
end;

function TDebugInfoModuleComparer.Compare(const Left, Right: TDebugInfoModule): Integer;
begin
  Result := integer(Left.Segment.Index) - integer(Right.Segment.Index);
  if (Result = 0) then
    Result := integer(Left.Offset) - integer(Right.Offset); // Cast to avoid integer overflow
end;


function TDebugInfoModules.Add(const AName: string; ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;
begin
  Result := TDebugInfoModule.Create(FDebugInfo, AName, ASegment, AOffset, ASize);
  try

    if (FComparer = niL) then
      FComparer := TDebugInfoModuleComparer.New;

    var Index: integer;
    if (FModules.BinarySearch(Result, Index, FComparer)) then
      raise Exception.Create('Cannot add overlapping modules');

    FModules.Insert(Index, Result);

  except
    Result.Free;
    raise;
  end;
end;

constructor TDebugInfoModules.Create(ADebugInfo: TDebugInfo);
begin
  inherited Create;

  FDebugInfo := ADebugInfo;
  FModules := TDebugInfoModuleList.Create;
end;

destructor TDebugInfoModules.Destroy;
begin
  FModules.Free;

  inherited;
end;

function TDebugInfoModules.FindByName(const AName: string; ASegment: TDebugInfoSegment): TDebugInfoModule;
begin
  for var Module in FModules do
    if (Module.Segment = ASegment) and (Module.Name = AName) then
      Exit(Module);
  Result := nil;
end;

function TDebugInfoModules.FindByOffset(ASegment: TDebugInfoSegment; AOffset: TDebugInfoOffset): TDebugInfoModule;
begin
  // Binary search
  var L := 0;
  var H := FModules.Count-1;

  while (L <= H) do
  begin
    var mid := L + (H - L) shr 1;

    var Module := FModules[mid];

    if (ASegment.Index < Module.Segment.Index) then
      H := mid - 1
    else
    if (ASegment.Index > Module.Segment.Index) then
      L := mid + 1
    else
    begin
      if (AOffset < Module.Offset) then
        H := mid - 1
      else
      if (AOffset >= Module.Offset+Module.Size) then
        L := mid + 1
      else
        Exit(Module);
    end;
  end;

  Result := nil;
end;

function TDebugInfoModules.FindOverlap(ASegment: TDebugInfoSegment; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;
begin
  // Binary search
  var L := 0;
  var H := FModules.Count-1;

  while (L <= H) do
  begin
    var mid := L + (H - L) shr 1;

    var Module := FModules[mid];

    if (ASegment.Index < Module.Segment.Index) then
      H := mid - 1
    else
    if (ASegment.Index > Module.Segment.Index) then
      L := mid + 1
    else
    begin
      if (AOffset < Module.Offset) then
      begin
        if (AOffset+ASize < Module.Offset) then
          H := mid - 1
        else
          Exit(Module);
      end else
      if (AOffset >= Module.Offset+Module.Size) then
        L := mid + 1
      else
        Exit(Module);
    end;
  end;

  Result := nil;
end;

function TDebugInfoModules.GetCount: integer;
begin
  Result := FModules.Count;
end;

function TDebugInfoModules.GetEmpty: boolean;
begin
  Result := (FModules.Count = 0);
end;

function TDebugInfoModules.GetEnumerator: TEnumerator<TDebugInfoModule>;
begin
  Result := FModules.GetEnumerator;
end;

{ TDebugInfoSymbol }

constructor TDebugInfoSymbol.Create(AModule: TDebugInfoModule; const AName: string; AOffset: TDebugInfoOffset);
begin
  inherited Create;

  FModule := AModule;
  FName := AName;
  FOffset := AOffset;
end;

function TDebugInfoSymbol.GetAddress: TDebugInfoOffset;
begin
  Result := Module.Address + Offset;
end;

{ TDebugInfoSegments }

constructor TDebugInfoSegments.Create;
begin
  inherited Create;

  FSegments := TObjectList<TDebugInfoSegment>.Create(True);
  FNames := TDictionary<string, TDebugInfoSegment>.Create;
end;

destructor TDebugInfoSegments.Destroy;
begin
  FNames.Free;
  FSegments.Free;

  inherited;
end;

function TDebugInfoSegments.Add(AIndex: Cardinal; const AClassName: string; const AName: string): TDebugInfoSegment;
begin
  var SegClassType := TDebugInfoSegment.ClassNameToClassType(AClassName);

  Result := Add(AIndex, SegClassType, AName);
end;

function TDebugInfoSegments.Add(AIndex: Cardinal; AClassType: TDebugInfoSegmentClass; const AName: string): TDebugInfoSegment;
begin
  if (AIndex = 0) then
    raise Exception.Create('Invalid Segment index');

  if (FNames.ContainsKey(AName)) then
    raise Exception.Create('Duplicate Segment name');

  // Index is 1-based
  var ListIndex := integer(AIndex-1);

  if (ListIndex < FSegments.Count) then
  begin
    if (FSegments[ListIndex] <> nil) then
      raise Exception.Create('Duplicate Segment index');
  end else
    FSegments.Count := ListIndex+1;

  Result := TDebugInfoSegment.Create(Self, AIndex, AClassType);
  Result.Name := AName;

  FSegments[ListIndex] := Result;
  FNames.Add(AName, Result);
end;

function TDebugInfoSegments.FindByName(const AName: string): TDebugInfoSegment;
begin
  if (not FNames.TryGetValue(AName, Result)) then
    Result := nil;
end;

function TDebugInfoSegments.FindByIndex(AIndex: Cardinal): TDebugInfoSegment;
begin
  // Index is 1-based
  if (AIndex <= 0) then
    raise Exception.Create('Invalid Segment index');

  Result := FSegments[AIndex - 1];
end;

function TDebugInfoSegments.GetCount: integer;
begin
  Result := FSegments.Count;
end;

function TDebugInfoSegments.GetEnumerator: TEnumerator<TDebugInfoSegment>;
begin
  Result := FSegments.GetEnumerator;
end;

function TDebugInfoSegments.GetSegment(AIndex: Cardinal): TDebugInfoSegment;
begin
  // Index is 1-based
  if (AIndex <= 0) then
    raise Exception.Create('Invalid Segment index');

  Result := FSegments[AIndex - 1];

  if (Result = nil) then
    raise Exception.CreateFmt('Segment index %d does not exist', [AIndex]);
end;

function TDebugInfoSegments.FindByClassType(AClassType: TDebugInfoSegmentClass): TDebugInfoSegment;
begin
  for Result in FSegments do
    if (Result.SegClassType = AClassType) then
      Exit;

  Result := nil;
end;

function TDebugInfoSegments.FindByOffset(AOffset: TDebugInfoOffset): TDebugInfoSegment;
begin
  for Result in FSegments do
    if (AOffset >= Result.Offset) and (AOffset < Result.Offset+Result.Size) then
      Exit;

  Result := nil;
end;

{ TDebugInfoSegment }

constructor TDebugInfoSegment.Create(AOwner: TDebugInfoSegments; AIndex: Cardinal; AClassType: TDebugInfoSegmentClass);
begin
  inherited Create;

  FOwner := AOwner;
  FIndex := AIndex;
  FSegClassType := AClassType;

  FCharacteristics := IMAGE_SCN_MEM_READ;
  if (AClassType = sctCODE) then
    FCharacteristics := FCharacteristics or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_CNT_CODE;
end;

constructor TDebugInfoSegment.Create(AOwner: TDebugInfoSegments; AIndex: Cardinal; const AClassName: string);
begin
  var SegClassType := ClassNameToClassType(AClassName);
  Create(AOwner, AIndex, SegClassType);
end;

function TDebugInfoSegment.GetSegClassName: string;
begin
  Result := sClassNames[FSegClassType];
end;

procedure TDebugInfoSegment.SetOffset(const Value: TDebugInfoOffset);
begin
  FOffset := Value;
  CheckOverlap;
end;

procedure TDebugInfoSegment.SetSize(const Value: TDebugInfoOffset);
begin
  FSize := Value;
  CheckOverlap;
end;

procedure TDebugInfoSegment.CheckOverlap;
begin
  if (Size = 0) then
    Exit;

  for var Segment in FOwner do
  begin
    // Ignore self and empty segments
    if (Segment = Self) or (Segment.Size = 0) then
      continue;

    if ((Offset >= Segment.Offset) and (Offset < Segment.Offset+Segment.Size)) or // Start is within other range
      ((Offset+Size <= Segment.Offset) and (Offset+Size > Segment.Offset+Segment.Size)) or // Start is within other range
      ((Offset <= Segment.Offset) and (Offset+Size > Segment.Offset)) then // Other is within range
      raise Exception.Create('Overlapping segments');
  end;
end;

class function TDebugInfoSegment.ClassNameToClassType(const AClassName: string): TDebugInfoSegmentClass;
begin
  for var SegClassType := Low(TDebugInfoSegmentClass) to High(TDebugInfoSegmentClass) do
    if (AClassName = sClassNames[SegClassType]) then
      Exit(SegClassType);

  raise Exception.CreateFmt('Unknown Segment Class: %s', [AClassName]);
end;

{ TDebugInfoSourceFile }

constructor TDebugInfoSourceFile.Create(const AFilename: string);
begin
  inherited Create;

  FFilename := AFilename;
end;

{ TDebugInfoSourceFiles }

function TDebugInfoSourceFiles.Add(const AFilename: string): TDebugInfoSourceFile;
begin
  if (FOwner <> nil) then
  begin
    // Delegate lookup/creation to owner...
    Result := FOwner.SourceFiles.Add(AFilename);

    // ... and add result to local list
    FSourceFiles.AddOrSetValue(AFilename, Result);
  end else
  if (not FSourceFiles.TryGetValue(AFilename, Result)) then
  begin
    Result := TDebugInfoSourceFile.Create(AFilename);
    FSourceFiles.Add(AFilename, Result);
  end;
end;

constructor TDebugInfoSourceFiles.Create(AOwner: TDebugInfo);
begin
  inherited Create;

  FOwner := AOwner;

  // If Owner=nil then we own the values
  var Ownerships: TDictionaryOwnerships;
  if (FOwner = nil) then
    Ownerships:= [doOwnsValues]
  else
    Ownerships:= [];

  FSourceFiles := TDebugInfoSourceFileList.Create(Ownerships);
end;

destructor TDebugInfoSourceFiles.Destroy;
begin
  FSourceFiles.Free;

  inherited;
end;

function TDebugInfoSourceFiles.First: TDebugInfoSourceFile;
begin
  // It's more efficient to create an enumerator than to access the Values array
  if (FSourceFiles.Count > 0) then
    for var Pair in FSourceFiles do
      Exit(Pair.Value);

  Result := nil;
end;

function TDebugInfoSourceFiles.GetCount: integer;
begin
  Result := FSourceFiles.Count;
end;

function TDebugInfoSourceFiles.GetEnumerator: TEnumerator<TDebugInfoSourceFile>;
begin
  Result := FSourceFiles.Values.GetEnumerator;
end;

{ TDebugInfoSourceLines }

function TDebugInfoSourceLines.Add(ASourceFile: TDebugInfoSourceFile; ALineNumber: integer;
  AOffset: TDebugInfoOffset): TDebugInfoSourceLine;
begin
  Result := TDebugInfoSourceLine.Create;

  Result.Module := FModule;
  Result.SourceFile := ASourceFile;
  Result.LineNumber := ALineNumber;
  Result.Offset := AOffset;

  // Order lines by SourceFile, LineNumber, Offset
  if (FComparer = nil) then
    FComparer := TComparer<TDebugInfoSourceLine>.Construct(
      function(const Left, Right: TDebugInfoSourceLine): integer
      begin
        Result := NativeInt(Left.SourceFile)-NativeInt(Right.SourceFile);

        if (Result = 0) then
          Result := Left.LineNumber - Right.LineNumber;

        if (Result = 0) then
          Result := integer(Left.Offset) - integer(Right.Offset);
      end);

  // Note that multiple offsets can map to the same Source File+Line Number.
  // This can for example be caused by include files, inlining and generics
  // expansion.

  var Index: integer;
  if (not FSourceLines.BinarySearch(Result, Index, FComparer)) then
    FSourceLines.Insert(Index, Result)
  else
    Result.Free;
end;

constructor TDebugInfoSourceLines.Create(AModule: TDebugInfoModule);
begin
  inherited Create;

  FModule := AModule;
  FSourceLines := TObjectList<TDebugInfoSourceLine>.Create(True);
end;

destructor TDebugInfoSourceLines.Destroy;
begin
  FSourceLines.Free;

  inherited;
end;

function TDebugInfoSourceLines.GetEmpty: boolean;
begin
  Result := (FSourceLines.Count = 0);
end;

function TDebugInfoSourceLines.GetEnumerator: TEnumerator<TDebugInfoSourceLine>;
begin
  Result := FSourceLines.GetEnumerator;
end;

{ TDebugInfoSourceLine }

function TDebugInfoSourceLine.GetAddress: TDebugInfoOffset;
begin
  Result := Offset + Module.Offset + Module.Segment.Offset
end;

end.

