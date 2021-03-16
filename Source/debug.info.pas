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
  Generics.Collections;

type
  TDebugInfoSegment = Word;
  TDebugInfoOffset = Cardinal;

type
  TDebugInfoSegmentClassType = (sctCODE, sctICODE, sctDATA, sctBSS, sctTLS, sctPDATA);

  TDebugInfoSegmentClass = class
  private const
    sClassNames: array[TDebugInfoSegmentClassType] of string = ('CODE', 'ICODE', 'DATA', 'BSS', 'TLS', 'PDATA');
  private
    FSegClassType: TDebugInfoSegmentClassType;
    FName: string;
    FOffset: TDebugInfoOffset;
    FSize: TDebugInfoOffset;
    FValue: integer;
  protected
    function GetSegClassName: string;
  public
    constructor Create(AValue: integer; const AClassName: string); overload;
    constructor Create(AValue: integer; AClassType: TDebugInfoSegmentClassType); overload;

    class function ClassNameToClassType(const AClassName: string): TDebugInfoSegmentClassType;

    property Value: integer read FValue;
    property SegClassType: TDebugInfoSegmentClassType read FSegClassType;
    property SegClassName: string read GetSegClassName;
    property Name: string read FName write FName;
    property Offset: TDebugInfoOffset read FOffset write FOffset;
    property Size: TDebugInfoOffset read FSize write FSize;
  end;

  TDebugInfoSegmentClasses = class
  private type
    TDebugInfoSegmentClassList = array[TDebugInfoSegmentClassType] of TDebugInfoSegmentClass;
  private
    FSegmentClasses: TDebugInfoSegmentClassList;
    FNames: TDictionary<string, TDebugInfoSegmentClass>;
  protected
    function GetSegment(AClassType: TDebugInfoSegmentClassType): TDebugInfoSegmentClass;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AValue: integer; AClassType: TDebugInfoSegmentClassType; const AName: string): TDebugInfoSegmentClass; overload;
    function Add(AValue: integer; const AClassName: string; const AName: string): TDebugInfoSegmentClass; overload;
    function FindByValue(AValue: integer): TDebugInfoSegmentClass;
    function FindByName(const AName: string): TDebugInfoSegmentClass;

    property SegmentClass[AClassType: TDebugInfoSegmentClassType]: TDebugInfoSegmentClass read GetSegment; default;
  end;

type
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
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const AFilename: string): TDebugInfoSourceFile;

    function GetEnumerator: TEnumerator<TDebugInfoSourceFile>;
  end;

type
  TDebugInfoModule = class;

  TDebugInfoSourceLine = class
  private
    FSourceFile: TDebugInfoSourceFile;
    FModule: TDebugInfoModule;
    FLineNumber: integer;
    FOffset: TDebugInfoOffset;
  public
    property Module: TDebugInfoModule read FModule write FModule;
    property SourceFile: TDebugInfoSourceFile read FSourceFile write FSourceFile;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Offset: TDebugInfoOffset read FOffset write FOffset;
  end;

  TDebugInfoSourceLines = class
  private
    FModule: TDebugInfoModule;
    FSourceLines: TObjectList<TDebugInfoSourceLine>;
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
  public
    constructor Create(AModule: TDebugInfoModule; const AName: string; AOffset: TDebugInfoOffset);

    property Module: TDebugInfoModule read FModule write FModule;
    property Name: string read FName write FName;
    property Offset: TDebugInfoOffset read FOffset;
    property Size: TDebugInfoOffset read FSize write FSize;
  end;

  TDebugInfoSymbols = class
  private type
    TDebugInfoSymbolList = TObjectList<TDebugInfoSymbol>;
  private
    FModule: TDebugInfoModule;
    FSymbols: TDebugInfoSymbolList;
    FSymbolNames: TDictionary<string, TDebugInfoSymbol>;
  public
    constructor Create(AModule: TDebugInfoModule);
    destructor Destroy; override;

    function Add(const AName: string; AOffset: TDebugInfoOffset): TDebugInfoSymbol;

    procedure CalculateSizes;

    function GetEnumerator: TEnumerator<TDebugInfoSymbol>;
  end;

  TDebugInfoModule = class
  private
    FName: string;
    FSegmentClass: TDebugInfoSegmentClass;
    FOffset: TDebugInfoOffset;
    FSize: TDebugInfoOffset;
    FSourceFiles: TDebugInfoSourceFiles;
    FSourceLines: TDebugInfoSourceLines;
    FSymbols: TDebugInfoSymbols;
  public
    constructor Create(const AName: string; ASegmentClass: TDebugInfoSegmentClass; AOffset, ASize: TDebugInfoOffset);
    destructor Destroy; override;

    property Name: string read FName;
    property SegmentClass: TDebugInfoSegmentClass read FSegmentClass;
    property Offset: TDebugInfoOffset read FOffset;
    property Size: TDebugInfoOffset read FSize;

    property SourceFiles: TDebugInfoSourceFiles read FSourceFiles;
    property SourceLines: TDebugInfoSourceLines read FSourceLines;
    property Symbols: TDebugInfoSymbols read FSymbols;
  end;

  TDebugInfoModules = class
  private type
    TDebugInfoModuleList = TObjectList<TDebugInfoModule>;
  private
    FModules: TDebugInfoModuleList;
  protected
    function GetEmpty: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const AName: string; ASegmentClass: TDebugInfoSegmentClass; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;
    function FindByName(const AName: string; ASegmentClass: TDebugInfoSegmentClass): TDebugInfoModule;
    function FindByOffset(ASegmentClass: TDebugInfoSegmentClass; AOffset: TDebugInfoOffset): TDebugInfoModule;

    property Empty: boolean read GetEmpty;

    function GetEnumerator: TEnumerator<TDebugInfoModule>;
  end;

type
  TDebugInfo = class
  private
    FSegmentClasses: TDebugInfoSegmentClasses;
    FModules: TDebugInfoModules;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentClasses: TDebugInfoSegmentClasses read FSegmentClasses;
    property Modules: TDebugInfoModules read FModules;
  end;

implementation

uses
  SysUtils,
  Generics.Defaults;

{ TDebugInfo }

constructor TDebugInfo.Create;
begin
  inherited Create;

  FSegmentClasses := TDebugInfoSegmentClasses.Create;
  FModules := TDebugInfoModules.Create;
end;

destructor TDebugInfo.Destroy;
begin
  FSegmentClasses.Free;
  FModules.Free;

  inherited;
end;

{ TDebugInfoModule }

constructor TDebugInfoModule.Create(const AName: string; ASegmentClass: TDebugInfoSegmentClass; AOffset, ASize: TDebugInfoOffset);
begin
  inherited Create;

  FName := AName;
  FSegmentClass := ASegmentClass;
  FOffset := AOffset;
  FSize := ASize;

  FSourceFiles := TDebugInfoSourceFiles.Create;
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
  FSymbolNames := TDictionary<string, TDebugInfoSymbol>.Create;
end;

destructor TDebugInfoSymbols.Destroy;
begin
  FSymbols.Free;
  FSymbolNames.Free;

  inherited;
end;

function TDebugInfoSymbols.GetEnumerator: TEnumerator<TDebugInfoSymbol>;
begin
  Result := FSymbols.GetEnumerator;
end;

function TDebugInfoSymbols.Add(const AName: string; AOffset: TDebugInfoOffset): TDebugInfoSymbol;
begin
  if (FSymbolNames.TryGetValue(AName, Result)) then
  begin
    if (AOffset <> Result.Offset) then
      raise Exception.CreateFmt('Duplicate symbol with different offsets: %s, %.8X %.8X', [AName, AOffset, Result.Offset]);

    Exit(Result);
  end;

  Result := TDebugInfoSymbol.Create(FModule, AName, AOffset);

  var Index: integer;
  if (FSymbols.BinarySearch(Result, Index, TComparer<TDebugInfoSymbol>.Construct(
    function(const Left, Right: TDebugInfoSymbol): integer
    begin
      Result := Left.Offset-Right.Offset;
    end))) then
  begin
    Result.Free;
    Result := nil;
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
  Result := Left.SegmentClass.Value - Right.SegmentClass.Value;
  if (Result = 0) then
    Result := Left.Offset - Right.Offset;
end;


function TDebugInfoModules.Add(const AName: string; ASegmentClass: TDebugInfoSegmentClass; AOffset, ASize: TDebugInfoOffset): TDebugInfoModule;
begin
  Result := TDebugInfoModule.Create(AName, ASegmentClass, AOffset, ASize);
  try

    var Index: integer;
    if (FModules.BinarySearch(Result, Index, TDebugInfoModuleComparer.New)) then
      raise Exception.Create('Cannot add overlapping modules');

    FModules.Insert(Index, Result);

  except
    Result.Free;
    raise;
  end;
end;

constructor TDebugInfoModules.Create;
begin
  inherited Create;

  FModules := TDebugInfoModuleList.Create;
end;

destructor TDebugInfoModules.Destroy;
begin
  FModules.Free;

  inherited;
end;

function TDebugInfoModules.FindByName(const AName: string; ASegmentClass: TDebugInfoSegmentClass): TDebugInfoModule;
begin
  for var Module in FModules do
    if (Module.SegmentClass = ASegmentClass) and (Module.Name = AName) then
      Exit(Module);
  Result := nil;
end;

function TDebugInfoModules.FindByOffset(ASegmentClass: TDebugInfoSegmentClass; AOffset: TDebugInfoOffset): TDebugInfoModule;
begin
  var L := 0;
  var H := FModules.Count-1;

  while (L <= H) do
  begin
    var mid := L + (H - L) shr 1;

    var Module := FModules[mid];

    if (ASegmentClass.Value < Module.SegmentClass.Value) then
      H := mid - 1
    else
    if (ASegmentClass.Value > Module.SegmentClass.Value) then
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

{ TDebugInfoSegmentClasses }

constructor TDebugInfoSegmentClasses.Create;
begin
  inherited Create;

  FNames := TDictionary<string, TDebugInfoSegmentClass>.Create;

  for var SegClassType := Low(TDebugInfoSegmentClassType) to High(TDebugInfoSegmentClassType) do
    FSegmentClasses[SegClassType] := nil;
end;

destructor TDebugInfoSegmentClasses.Destroy;
begin
  FNames.Free;

  for var SegmentClass in FSegmentClasses do
    SegmentClass.Free;

  inherited;
end;

function TDebugInfoSegmentClasses.Add(AValue: integer; const AClassName: string; const AName: string): TDebugInfoSegmentClass;
begin
  var SegClassType := TDebugInfoSegmentClass.ClassNameToClassType(AClassName);

  Result := Add(AValue, SegClassType, AName);
end;

function TDebugInfoSegmentClasses.Add(AValue: integer; AClassType: TDebugInfoSegmentClassType; const AName: string): TDebugInfoSegmentClass;
begin
  if (FSegmentClasses[AClassType] <> nil) then
    raise Exception.Create('Duplicate Segment Class');

  Result := TDebugInfoSegmentClass.Create(AValue, AClassType);
  try

    Result.Name := AName;

    FNames.Add(AName, Result);

  except
    Result.Free;
    raise;
  end;

  FSegmentClasses[AClassType] := Result;
end;

function TDebugInfoSegmentClasses.FindByName(const AName: string): TDebugInfoSegmentClass;
begin
  if (not FNames.TryGetValue(AName, Result)) then
    Result := nil;
end;

function TDebugInfoSegmentClasses.FindByValue(AValue: integer): TDebugInfoSegmentClass;
begin
  for var SegmentClass in FSegmentClasses do
    if (SegmentClass.Value = AValue) then
      Exit(SegmentClass);

  Result := nil;
end;

function TDebugInfoSegmentClasses.GetSegment(AClassType: TDebugInfoSegmentClassType): TDebugInfoSegmentClass;
begin
  Result := FSegmentClasses[AClassType];
end;

{ TDebugInfoSegmentClass }

constructor TDebugInfoSegmentClass.Create(AValue: integer; AClassType: TDebugInfoSegmentClassType);
begin
  inherited Create;

  FValue := AValue;
  FSegClassType := AClassType;
end;

constructor TDebugInfoSegmentClass.Create(AValue: integer; const AClassName: string);
begin
  var SegClassType := ClassNameToClassType(AClassName);
  Create(AValue, SegClassType);
end;

function TDebugInfoSegmentClass.GetSegClassName: string;
begin
  Result := sClassNames[FSegClassType];
end;

class function TDebugInfoSegmentClass.ClassNameToClassType(const AClassName: string): TDebugInfoSegmentClassType;
begin
  for var SegClassType := Low(TDebugInfoSegmentClassType) to High(TDebugInfoSegmentClassType) do
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
  if (not FSourceFiles.TryGetValue(AFilename, Result)) then
  begin
    Result := TDebugInfoSourceFile.Create(AFilename);
    FSourceFiles.Add(AFilename, Result);
  end;
end;

constructor TDebugInfoSourceFiles.Create;
begin
  inherited Create;

  FSourceFiles := TDebugInfoSourceFileList.Create([doOwnsValues]);
end;

destructor TDebugInfoSourceFiles.Destroy;
begin
  FSourceFiles.Free;

  inherited;
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

  // Order lines by SourceFile, Offset, LineNumber
  var Comparer: IComparer<TDebugInfoSourceLine> := TComparer<TDebugInfoSourceLine>.Construct(
    function(const Left, Right: TDebugInfoSourceLine): integer
    begin
      Result := NativeInt(Left.SourceFile)-NativeInt(Right.SourceFile);

      if (Result = 0) then
        Result := Left.Offset-Right.Offset;

      if (Result = 0) then
        Result := Left.LineNumber-Right.LineNumber;
    end);

  var Index: integer;
  if (not FSourceLines.BinarySearch(Result, Index, Comparer)) then
    FSourceLines.Insert(Index, Result)
  else
    Result.Free;

  for var i := 0 to FSourceLines.Count-2 do
    Assert(Comparer.Compare(FSourceLines[i], FSourceLines[i+1]) < 0, IntToStr(FSourceLines.Count));
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

end.

