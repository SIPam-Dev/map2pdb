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
  debug.info in 'debug.info.pas',
  debug.info.pdb in 'debug.info.pdb.pas';

// Find parameter by index (zero based), ignores switches.
function FindParam(Index: integer; var Value: string; const Default: string = ''): boolean;
begin
  for var i := 1 to ParamCount do
  begin
    Value := ParamStr(i);

    if (CharInSet(Value[1], SwitchChars)) then
      continue;

    if (Index = 0) then
      Exit(True);

    Dec(Index);
  end;
  Value := Default;
  Result := False;
end;

procedure DisplayBanner;
begin
  Writeln('map2yaml - Copyright (c) 2021 Anders Melander');
  Writeln('Version 1.0');
  Writeln;
end;

procedure DisplayHelp;
begin
  Writeln('Parses the map file produced by Delphi and writes a YAML file for use with llvm-pdbutil.');
  Writeln;
  Writeln('Usage: map2yaml [options] <map-filename> [<yaml-filename>]');
  Writeln;
  Writeln('Options:');
  Writeln('  -v     Verbose output');
  Writeln;
end;

begin
  try

    DisplayBanner;

    var SourceFilename: string;
    if (not FindParam(0, SourceFilename)) or (FindCmdLineSwitch('h')) or (FindCmdLineSwitch('?')) then
    begin
      DisplayHelp;
      exit;
    end;

    var TargetFilename: string;
    if (not FindParam(1, TargetFilename)) then
      TargetFilename := TPath.ChangeExtension(SourceFilename, '.yaml');

    var DebugInfo := TDebugInfo.Create;
    try

      var Logging := FindCmdLineSwitch('v');

      var Reader := TDebugInfoMapReader.Create;
      try

        Reader.Logging := Logging;

        Reader.LoadFromFile(SourceFilename, DebugInfo);

      finally
        Reader.Free;
      end;

      var Writer := TDebugInfoYamlWriter.Create;
      try

        Writer.Logging := Logging;

        Writer.SaveToFile(TargetFilename, DebugInfo);

      finally
        Writer.Free;
      end;

    finally
      DebugInfo.Free;
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

