unit debug.info.writer;

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
  // Abstract writer base class
  TDebugInfoWriter = class abstract
  private
    FLogging: boolean;
  protected
    procedure Log(const Msg: string); virtual;
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
  public
    constructor Create; virtual;

    procedure SaveToStream(Stream: TStream; DebugInfo: TDebugInfo); virtual; abstract;
    procedure SaveToFile(const Filename: string; DebugInfo: TDebugInfo);

    property Logging: boolean read FLogging write FLogging;
  end;

  TDebugInfoWriterClass = class of TDebugInfoWriter;

implementation

{ TDebugInfoWriter }

constructor TDebugInfoWriter.Create;
begin
  inherited Create;
  // Does nothing. We just need a virtual constructor for polymorphism.
end;

procedure TDebugInfoWriter.Log(const Msg: string);
begin
  if (FLogging) then
    WriteLn(Msg);
end;

procedure TDebugInfoWriter.Warning(const Msg: string);
begin
  WriteLn('Warning: ' + Msg);
end;

procedure TDebugInfoWriter.Error(const Msg: string);
begin
  WriteLn('Error:   ' + Msg);
  Halt(1);
end;

procedure TDebugInfoWriter.SaveToFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  var Stream := TFileStream.Create(Filename, fmCreate);
  try

    SaveToStream(Stream, DebugInfo);

  finally
    Stream.Free;
  end;
end;

end.

