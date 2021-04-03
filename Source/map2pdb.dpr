program map2pdb;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}

{$R *.res}

uses
{$ifdef MADEXCEPT}
  madExcept,
{$endif MADEXCEPT}
  System.SysUtils,
  System.IOUtils,
  debug.info.reader.map in 'debug.info.reader.map.pas',
  debug.info.writer.yaml in 'debug.info.writer.yaml.pas',
  debug.info in 'debug.info.pas',
  debug.info.pdb in 'debug.info.pdb.pas',
  debug.info.writer.pdb in 'debug.info.writer.pdb.pas',
  debug.info.writer in 'debug.info.writer.pas',
  debug.info.codeview in 'debug.info.codeview.pas',
  debug.info.reader.test in 'debug.info.reader.test.pas',
  debug.info.reader in 'debug.info.reader.pas',
  debug.info.pdb.bind in 'debug.info.pdb.bind.pas',
  debug.info.msf in 'debug.info.msf.pas';

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
  Writeln('map2pdb - Copyright (c) 2021 Anders Melander');
  Writeln('Version 2.0');
  Writeln;
end;

procedure DisplayHelp;
begin
  Writeln('Parses the map file produced by Delphi and writes a PDB file.');
  Writeln;
  Writeln('Usage: map2pdb [options] <map-filename>');
  Writeln;
  Writeln('Options:');
  Writeln('  -v                         Verbose output');
  Writeln('  -pdb[:<output-filename>]   Writes a PDB (default)');
  Writeln('  -yaml[:<output-filename>]  Writes an YAML file that can be used with llvm-pdbutil');
  Writeln('  -bind[:<exe-filename>]     Patches a Delphi compiled exe file to include a reference to the pdb file');
  Writeln('  -test                      Works on test data. Ignores the input file');
  Writeln;
end;

type
  TTargetType = (ttPDB, ttYAML);
const
  sFileTypes: array[TTargetType] of string = ('.pdb', '.yaml');
  WriterClasses: array[TTargetType] of TDebugInfoWriterClass = (TDebugInfoPdbWriter, TDebugInfoYamlWriter);
begin
  try

    DisplayBanner;

    var SourceFilename: string;
    var TargetFilename: string := '';
    var PEFilename: string := '';

    FindCmdLineSwitch('bind', PEFilename, True, [clstValueAppended]);

    if (not FindParam(0, SourceFilename)) or (FindCmdLineSwitch('h')) or (FindCmdLineSwitch('?')) then
    begin
      DisplayHelp;
      exit;
    end;

    var Test := FindCmdLineSwitch('t') or FindCmdLineSwitch('test');
    var Logging := FindCmdLineSwitch('v') or FindCmdLineSwitch('verbose');

    var TargetType: TTargetType := ttPDB;
    if (FindCmdLineSwitch('yaml', TargetFilename, True, [clstValueAppended])) or (FindCmdLineSwitch('yaml')) then
    begin

      TargetType := ttYAML;

    end else
    begin

      FindCmdLineSwitch('pdb', TargetFilename, True, [clstValueAppended]);

      // If we're both building a PDB and binding it in one go then we can use
      // a new GUID for both.
      if (PEFilename <> '') or (FindCmdLineSwitch('bind')) then
      begin
        PdbBuildSignature := TGUID.NewGuid;

        if (Logging) then
          Writeln(Format('Constructed a new PDB GUID: %s', [PdbBuildSignature.ToString]));
      end;
    end;

    if (TargetFilename = '') then
    begin
      TargetFilename := TPath.ChangeExtension(SourceFilename, sFileTypes[TargetType]);

      if (Logging) then
        Writeln(Format('Output filename not specified. Defaulting to %s', [TPath.GetFileName(TargetFilename)]));
    end;

    var DebugInfo := TDebugInfo.Create;
    try

      var ReaderClass: TDebugInfoReaderClass := TDebugInfoMapReader;

      if (Test) then
        ReaderClass := TDebugInfoSyntheticReader;

      var Reader := ReaderClass.Create;
      try

        Reader.Logging := Logging;

        Reader.LoadFromFile(SourceFilename, DebugInfo);

      finally
        Reader.Free;
      end;


      var Writer := WriterClasses[TargetType].Create;
      try

        Writer.Logging := Logging;

        Writer.SaveToFile(TargetFilename, DebugInfo);

      finally
        Writer.Free;
      end;

    finally
      DebugInfo.Free;
    end;


    if (PEFilename <> '') or (FindCmdLineSwitch('bind')) then
    begin
      if (TargetType <> ttPDB) then
        raise Exception.Create('-bind requires PDB output');

      if (PEFilename = '') then
      begin
        PEFilename := TPath.ChangeExtension(SourceFilename, '.exe');

        if (Logging) then
          Writeln(Format('PE filename not specified. Defaulting to %s', [TPath.GetFileName(PEFilename)]));
      end;

      PatchPE(PEFilename, TargetFilename, Logging);
    end;


  except
{$ifdef MADEXCEPT}
    madExcept.HandleException;

    if (DebugHook <> 0) then
    begin
      Writeln('Press enter to continue');
      Readln;
    end;
    Halt(1);
{$else MADEXCEPT}
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);

      if (DebugHook <> 0) then
      begin
        Writeln('Press enter to continue');
        Readln;
      end;

      Halt(1);
    end;
{$endif MADEXCEPT}
  end;
end.

