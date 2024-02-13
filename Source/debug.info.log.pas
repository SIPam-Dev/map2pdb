unit debug.info.log;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

type
  TDebugInfoLogCategory = (lcDebug, lcInfo, lcWarning, lcError, lcFatal);
  TDebugInfoLogModule = integer; // Just a handle

  IDebugInfoLogger = interface
    ['{0EFE498C-E264-4466-AF51-D25BF7F300AE}']
    procedure Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
  end;

function DebugInfoLogger: IDebugInfoLogger;

function RegisterDebugInfoLogger(const Logger: IDebugInfoLogger): IDebugInfoLogger;

function RegisterDebugInfoLogModule(const ModuleName: string): TDebugInfoLogModule;
function GetDebugInfoLogModuleName(LogModule: TDebugInfoLogModule): string;


// Get and set the log level.
// Messages with a category less than the log level will not be logged.
function DebugInfoLogLevel: TDebugInfoLogCategory;
procedure SetDebugInfoLogLevel(Category: TDebugInfoLogCategory);


// IDebugInfoModuleLogger wraps the DebugInfoLogger/RegisterDebugInfoLogModule combo
// into a single object for convenience.
// Additionally it provides overloads for formatted output.
type
  IDebugInfoModuleLogger = interface
    ['{4472B73B-7D35-4FD1-9A38-EBAE5BDA188D}']
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Fmt: string; const Args: array of const); overload;

    procedure Info(const Msg: string); overload;
    procedure Info(const Fmt: string; const Args: array of const); overload;

    procedure Warning(const Msg: string); overload;
    procedure Warning(const Fmt: string; const Args: array of const); overload;

    procedure Error(const Msg: string; Fatal: boolean = True); overload;
    procedure Error(const Fmt: string; const Args: array of const; Fatal: boolean = True); overload;
  end;

function RegisterDebugInfoModuleLogger(const ModuleName: string): IDebugInfoModuleLogger;


implementation

uses
  System.Generics.Collections,
  System.SysUtils,
  Debug.Info; // EDebugInfo

var
  FDebugInfoLogLevel: TDebugInfoLogCategory = lcError;

function DebugInfoLogLevel: TDebugInfoLogCategory;
begin
  Result := FDebugInfoLogLevel;
end;

procedure SetDebugInfoLogLevel(Category: TDebugInfoLogCategory);
begin
  FDebugInfoLogLevel := Category;
end;

type
  TDebugInfoModuleLogger = class abstract(TInterfacedObject, IDebugInfoModuleLogger)
  private
    FLogModule: TDebugInfoLogModule;
  protected
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Fmt: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Fmt: string; const Args: array of const); overload;
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Fmt: string; const Args: array of const); overload;
    procedure Error(const Msg: string; Fatal: boolean = True); overload;
    procedure Error(const Fmt: string; const Args: array of const; Fatal: boolean = True); overload;
  public
    constructor Create(ALogModule: TDebugInfoLogModule);
  end;

constructor TDebugInfoModuleLogger.Create(ALogModule: TDebugInfoLogModule);
begin
  inherited Create;
  FLogModule := ALogModule;
end;

procedure TDebugInfoModuleLogger.Debug(const Fmt: string; const Args: array of const);
begin
  if (FDebugInfoLogLevel <= lcDebug) then
    Debug(Format(Fmt, Args));
end;

procedure TDebugInfoModuleLogger.Debug(const Msg: string);
begin
  if (FDebugInfoLogLevel <= lcDebug) then
    DebugInfoLogger.Log(lcDebug, FLogModule, Msg);
end;

procedure TDebugInfoModuleLogger.Error(const Fmt: string; const Args: array of const; Fatal: boolean);
begin
  if (FDebugInfoLogLevel <= lcError) or (Fatal) then
    Error(Format(Fmt, Args), Fatal);
end;

procedure TDebugInfoModuleLogger.Error(const Msg: string; Fatal: boolean);
const
  ErrorCategory: array[boolean] of TDebugInfoLogCategory = (lcError, lcFatal);
begin
  if (FDebugInfoLogLevel <= lcError) or (Fatal) then
    DebugInfoLogger.Log(ErrorCategory[Fatal], FLogModule, Msg);
end;

procedure TDebugInfoModuleLogger.Info(const Fmt: string; const Args: array of const);
begin
  if (FDebugInfoLogLevel <= lcInfo) then
    Info(Format(Fmt, Args));
end;

procedure TDebugInfoModuleLogger.Info(const Msg: string);
begin
  if (FDebugInfoLogLevel <= lcInfo) then
    DebugInfoLogger.Log(lcInfo, FLogModule, Msg);
end;

procedure TDebugInfoModuleLogger.Warning(const Fmt: string; const Args: array of const);
begin
  if (FDebugInfoLogLevel <= lcWarning) then
    Warning(Format(Fmt, Args));
end;

procedure TDebugInfoModuleLogger.Warning(const Msg: string);
begin
  if (FDebugInfoLogLevel <= lcWarning) then
    DebugInfoLogger.Log(lcWarning, FLogModule, Msg);
end;

function RegisterDebugInfoModuleLogger(const ModuleName: string): IDebugInfoModuleLogger;
begin
  var LogModule := RegisterDebugInfoLogModule(ModuleName);
  Result := TDebugInfoModuleLogger.Create(LogModule);
end;

type
  TDebugInfoNullLogger = class(TInterfacedObject, IDebugInfoLogger)
  protected
    // IDebugInfoLogger
    procedure Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
  end;

procedure TDebugInfoNullLogger.Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
begin
end;

var
  FDebugInfoLogger: IDebugInfoLogger;
  FDebugInfoNullLogger: IDebugInfoLogger;

function DebugInfoLogger: IDebugInfoLogger;
begin
  if (FDebugInfoLogger = nil) then
  begin
    if (FDebugInfoNullLogger = nil) then
      FDebugInfoNullLogger := TDebugInfoNullLogger.Create;
    Result := FDebugInfoNullLogger;
  end else
    Result := FDebugInfoLogger;
end;

function RegisterDebugInfoLogger(const Logger: IDebugInfoLogger): IDebugInfoLogger;
begin
  Result := FDebugInfoLogger;
  FDebugInfoLogger := Logger;
end;

var
  FLogModules: TList<string>;

function RegisterDebugInfoLogModule(const ModuleName: string): TDebugInfoLogModule;
begin
  if (FLogModules = nil) then
    FLogModules := TList<string>.Create;

  Result := FLogModules.IndexOf(ModuleName);

  if (Result = -1) then
    Result := FLogModules.Add(ModuleName);
end;

function GetDebugInfoLogModuleName(LogModule: TDebugInfoLogModule): string;
begin
  if (FLogModules = nil) then
    raise EDebugInfo.CreateFmt('Invalid log module handle: %d', [LogModule]);

  Result := FLogModules[LogModule];
end;


initialization
finalization
  FDebugInfoLogger := nil;
  FDebugInfoNullLogger := nil;
  FreeAndNil(FLogModules);
end.

