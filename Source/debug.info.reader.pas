unit debug.info.reader;

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
  // Abstract reader base class
  TDebugInfoReader = class abstract
  private
    FModuleLogger: IDebugInfoModuleLogger;
  protected
    property Logger: IDebugInfoModuleLogger read FModuleLogger;
  public
    constructor Create; virtual;

    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); virtual; abstract;
    procedure LoadFromFile(const Filename: string; DebugInfo: TDebugInfo); virtual;
  end;

  TDebugInfoReaderClass = class of TDebugInfoReader;


implementation

uses
  System.SysUtils;


constructor TDebugInfoReader.Create;
begin
  inherited Create;
  FModuleLogger := RegisterDebugInfoModuleLogger('reader');
end;

procedure TDebugInfoReader.LoadFromFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  try

    var Stream := TBufferedFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try

      LoadFromStream(Stream, DebugInfo);

    finally
      Stream.Free;
    end;

  except
    on E: EFOpenError do
      Logger.Error(E.Message);
  end;
end;

end.

