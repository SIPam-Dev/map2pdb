program map2yaml;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  debug.info.reader.map in 'debug.info.reader.map.pas',
  debug.info.writer.yaml in 'debug.info.writer.yaml.pas',
  debug.info in 'debug.info.pas';

begin
  try

    Writeln('map2yaml - Copyright (c) 2021 Anders Melander');
    if (ParamCount < 1) then
    begin
      Writeln('Parses the map file produced by Delphi and writes a YAML file for use with llvm-pdbutil.');
      Writeln;
      Writeln('Usage: map2yaml <map-filename> [<yaml-filename>]');
      Exit;
    end;

    var SourceFilename := ParamStr(1);
    var TargetFilename := ParamStr(2);
    if (TargetFilename = '') then
      TargetFilename := TPath.ChangeExtension(SourceFilename, '.yaml');

    var DebugInfo := TDebugInfo.Create;
    try

      var Reader := TDebugInfoMapReader.Create;
      try

        Reader.LoadFromFile(SourceFilename, DebugInfo);

      finally
        Reader.Free;
      end;

      var Writer := TDebugInfoYamlWriter.Create;
      try

        Writer.SaveToFile(TargetFilename, DebugInfo);

      finally
        Writer.Free;
      end;

    finally
      DebugInfo.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

