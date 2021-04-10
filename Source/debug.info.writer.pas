unit debug.info.writer;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  System.Classes,
  debug.info,
  debug.info.log;

type
  // Abstract writer base class
  TDebugInfoWriter = class abstract
  private
    FModuleLogger: IDebugInfoModuleLogger;
  protected
    property Logger: IDebugInfoModuleLogger read FModuleLogger;
  public
    constructor Create; virtual;

    procedure SaveToStream(Stream: TStream; DebugInfo: TDebugInfo); virtual; abstract;
    procedure SaveToFile(const Filename: string; DebugInfo: TDebugInfo);
  end;

  TDebugInfoWriterClass = class of TDebugInfoWriter;


implementation

constructor TDebugInfoWriter.Create;
begin
  inherited Create;
  FModuleLogger := RegisterDebugInfoModuleLogger('writer');
end;

procedure TDebugInfoWriter.SaveToFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  try

    var Stream := TMemoryStream.Create;
    try

      SaveToStream(Stream, DebugInfo);
      Stream.SaveToFile(Filename);

    finally
      Stream.Free;
    end;

  except
    on E: EFCreateError do
      Logger.Error(E.Message);
  end;
end;

end.

