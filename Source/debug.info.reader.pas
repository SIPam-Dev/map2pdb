unit debug.info.reader;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Classes,
  debug.info;

type
  // Abstract reader base class
  TDebugInfoReader = class abstract
  private
    FLogging: boolean;
  protected
    procedure Log(const Msg: string);
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Fmt: string; const Args: array of const); overload;
    procedure Warning(LineNumber: integer; const Msg: string); overload;
    procedure Warning(LineNumber: integer; const Fmt: string; const Args: array of const); overload;
  public
    constructor Create; virtual;

    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); virtual; abstract;
    procedure LoadFromFile(const Filename: string; DebugInfo: TDebugInfo); virtual;

    property Logging: boolean read FLogging write FLogging;
  end;

  TDebugInfoReaderClass = class of TDebugInfoReader;


implementation

uses
  System.SysUtils;


constructor TDebugInfoReader.Create;
begin
  inherited Create;
  // Does nothing. We just need a virtual constructor for polymorphism.
end;

procedure TDebugInfoReader.Log(const Msg: string);
begin
  if (FLogging) then
    WriteLn(Msg);
end;

procedure TDebugInfoReader.Warning(const Msg: string);
begin
  WriteLn(Msg);
end;

procedure TDebugInfoReader.Warning(const Fmt: string; const Args: array of const);
begin
  Warning(Format(Fmt, Args));
end;

procedure TDebugInfoReader.Warning(LineNumber: integer; const Msg: string);
begin
  Warning('[%5d] %s', [LineNumber, Msg]);
end;

procedure TDebugInfoReader.Warning(LineNumber: integer; const Fmt: string; const Args: array of const);
begin
  Warning(LineNumber, Format(Fmt, Args));
end;

procedure TDebugInfoReader.LoadFromFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  var Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try

    LoadFromStream(Stream, DebugInfo);

  finally
    Stream.Free;
  end;
end;

end.

