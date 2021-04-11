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

// Define SAVE_MEMSTREAM to use a TMemoryStream when constructing the PDB.
// Otherwise a TBufferedFileStream is used.
{$define SAVE_MEMSTREAM}

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

{$ifdef SAVE_MEMSTREAM}
    var Stream := TMemoryStream.Create;
{$else SAVE_MEMSTREAM}
    var Stream := TBufferedFileStream.Create(Filename, fmCreate, $8000);
{$endif SAVE_MEMSTREAM}
    try

      SaveToStream(Stream, DebugInfo);

{$ifdef SAVE_MEMSTREAM}
      Stream.SaveToFile(Filename);
{$endif SAVE_MEMSTREAM}

    finally
      Stream.Free;
    end;

  except
    on E: EFCreateError do
      Logger.Error(E.Message);
  end;
end;

end.

