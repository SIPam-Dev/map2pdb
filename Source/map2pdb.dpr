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
{$WEAKLINKRTTI ON}

{$R *.res}

uses
  {$ifdef MADEXCEPT}
  madExcept,
  {$endif MADEXCEPT}
  System.SysUtils,
  System.IOUtils,
  System.Diagnostics,
  System.Generics.Collections,
  System.Masks,
  System.StrUtils,
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
  debug.info.msf in 'debug.info.msf.pas',
  debug.info.log in 'debug.info.log.pas',
  debug.info.progress in 'debug.info.progress.pas';

var
  Logger: IDebugInfoModuleLogger;

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
  Writeln('Version 2.5');
  Writeln;
end;

procedure DisplayHelp;
begin
  //                1         2         3         4         5         6         7         8
  //       12345678901234567890123456789012345678901234567890123456789012345678901234567890
  //
  Writeln('Parses the map file produced by Delphi and writes a PDB file.');
  Writeln;
  Writeln('Usage: map2pdb [options] <map-filename>');
  Writeln;
  Writeln('Options:');
  Writeln('  -v                         Verbose output');
  Writeln('  -pdb[:<output-filename>]   Writes a PDB file (default)');
  Writeln('  -yaml[:<output-filename>]  Writes an YAML file that can be used with');
  Writeln('                             llvm-pdbutil');
  Writeln('  -bind[:<exe-filename>]     Patches a Delphi compiled exe/dll file to include');
  Writeln('                             a reference to the pdb file');
  Writeln('  -include:<filenames>       Include the specified list of modules in the pdb');
  Writeln('                             (semicolor separated list, wildcards supported)');
  Writeln('  -exclude:<filenames>       Exclude the specified list of modules from the pdb');
  Writeln('                             (semicolor separated list, wildcards supported)');
  Writeln('  -test                      Works on test data. Ignores the input file');
  Writeln;
  Writeln('Examples:');
  Writeln;
  Writeln('* Read from foobar.map, create foobar.pdb, patch foobar.exe to reference');
  Writeln('  foobar.pdb and ignore all units starting with "system" or "dx":');
  Writeln;
  Writeln('    map2pdb -exclude:system*;dx* -bind foobar.map');
  Writeln;
  Writeln('* Read from mypackage.map, create mypackage.pdb, patch mypackage.bpl to');
  Writeln('  reference mypackage.pdb:');
  Writeln;
  Writeln('    map2pdb -bind:mypackage.bpl mypackage.map');
  Writeln;
end;

procedure DisplayElapsedTime(ms: Int64);
const
  MillisecondsPerSecond = 1000;
  MillisecondsPerMinute = 60 * Int64(MillisecondsPerSecond);
  MillisecondsPerHour = 60 * Int64(MillisecondsPerMinute);
begin
  Logger.Info('Elapsed time: %.2d:%.2d:%.2d.%d', [
    (ms div MillisecondsPerHour) mod 24,
    (ms div MillisecondsPerMinute) mod 60,
    (ms div MillisecondsPerSecond) mod 60,
    (ms mod MillisecondsPerSecond)]);
end;

procedure FilterModules(DebugInfo: TDebugInfo; const ModuleFilter: string; Include: boolean);
begin
  try
    var MaskValues := ModuleFilter.Split([';']);

    var CountIncluded := 0;
    var CountExcluded := 0;

    var Masks := TObjectList<TMask>.Create;
    var Segments := TList<Word>.Create;
    try
      Masks.Capacity := Length(MaskValues);
      for var MaskValue in MaskValues do
      begin
        var Segment: integer;
        if (MaskValue.Length = 4) and (TryStrToInt('$'+MaskValue, Segment)) and (Segment <= $FFFF) then
          Segments.Add(Segment)
        else
          Masks.Add(TMask.Create(MaskValue));
      end;

      for var i := DebugInfo.Modules.Count-1 downto 0 do
      begin
        var Module := DebugInfo.Modules[i];
        var KeepIt: boolean := not Include;

        for var Mask in Masks do
          if (Mask.Matches(Module.Name)) then
          begin
            KeepIt := Include;
            break;
          end;

        if (KeepIt = not Include) then
          for var Segment in Segments do
            if (Module.Segment.Index = Segment) then
            begin
              KeepIt := Include;
              break;
            end;

        if (not KeepIt) then
        begin
          if (Include) then
          begin
            Inc(CountIncluded);
            Logger.Debug('Include filter eliminated module: %s', [Module.Name])
          end else
          begin
            Inc(CountExcluded);
            Logger.Debug('Exclude filter eliminated module: %s', [Module.Name]);
          end;

          DebugInfo.Modules.Remove(Module);
        end;
      end;
    finally
      Segments.Free;
      Masks.Free;
    end;

    if (CountIncluded > 0) then
      Logger.Info('Filter included %.0n module(s)', [CountIncluded * 1.0]);

    if (CountExcluded > 0) then
      Logger.Info('Filter excluded %.0n module(s)', [CountExcluded * 1.0]);

  except
    on E: EMaskException do
      Logger.Error('Invalid filter. %s', [E.Message]);
  end;
end;

type
  TDebugInfoConsoleLogger = class(TInterfacedObject, IDebugInfoLogger)
  protected
    // IDebugInfoLogger
    procedure Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
  public
    class function New: IDebugInfoLogger;
  end;

procedure TDebugInfoConsoleLogger.Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
const
  sLogCategory: array[TDebugInfoLogCategory] of string = ('Debug: ', '', 'Warning: ', 'Error: ', 'Fatal: ');
begin
  if (Category >= DebugInfoLogLevel) then
    Writeln(Format('%s%s', [sLogCategory[Category], Msg]));

  if (Category = lcFatal) then
    Abort;
end;

class function TDebugInfoConsoleLogger.New: IDebugInfoLogger;
begin
  Result := TDebugInfoConsoleLogger.Create;
end;

type
  TTargetType = (ttPDB, ttYAML);
const
  sFileTypes: array[TTargetType] of string = ('.pdb', '.yaml');
  WriterClasses: array[TTargetType] of TDebugInfoWriterClass = (TDebugInfoPdbWriter, TDebugInfoYamlWriter);
begin
  var sw := TStopwatch.StartNew;
  try

    RegisterDebugInfoLogger(TDebugInfoConsoleLogger.New);
    Logger := RegisterDebugInfoModuleLogger('main');

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

    if FindCmdLineSwitch('debug') then
      SetDebugInfoLogLevel(lcDebug)
    else
    if FindCmdLineSwitch('v') or FindCmdLineSwitch('verbose') then
      SetDebugInfoLogLevel(lcInfo);

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

        Logger.Info('Constructed a new PDB GUID: %s', [PdbBuildSignature.ToString]);
      end;
    end;

    if (TargetFilename = '') then
    begin
      TargetFilename := TPath.ChangeExtension(SourceFilename, sFileTypes[TargetType]);

      Logger.Info('Output filename not specified. Defaulting to %s', [TPath.GetFileName(TargetFilename)]);
    end;


    var DebugInfo := TDebugInfo.Create;
    try

      (*
      ** Read source file
      *)
      var ReaderClass: TDebugInfoReaderClass := TDebugInfoMapReader;

      if (Test) then
        ReaderClass := TDebugInfoSyntheticReader;

      var Reader := ReaderClass.Create;
      try

        Reader.LoadFromFile(SourceFilename, DebugInfo);

      finally
        Reader.Free;
      end;


      (*
      ** Apply filters
      *)

      // Eliminate modules that doesn't satisfy include filter
      var Filter := ''; // Include everything by default
      if (FindCmdLineSwitch('include', Filter, True, [clstValueAppended])) and (Filter <> '') then
        FilterModules(DebugInfo, Filter, True);

      // Eliminate modules that satisfies exclude filter
      Filter := ''; // Exclude nothing by default
      if (FindCmdLineSwitch('exclude', Filter, True, [clstValueAppended])) and (Filter <> '') then
        FilterModules(DebugInfo, Filter, False);


      (*
      ** Write target file
      *)
      var Writer := WriterClasses[TargetType].Create;
      try

        Writer.SaveToFile(TargetFilename, DebugInfo);

      finally
        Writer.Free;
      end;

    finally
      DebugInfo.Free;
    end;


    (*
    ** Bind PE file to PDB
    *)
    if (PEFilename <> '') or (FindCmdLineSwitch('bind')) then
    begin
      if (TargetType <> ttPDB) then
        raise Exception.Create('-bind requires PDB output');

      if (PEFilename = '') then
      begin
        PEFilename := TPath.ChangeExtension(SourceFilename, '.exe');

        Logger.Info('PE filename not specified. Defaulting to %s', [TPath.GetFileName(PEFilename)]);
      end;

      PatchPE(PEFilename, TargetFilename);
    end;

  except
{$ifdef MADEXCEPT}
    madExcept.HandleException;

    if (DebugHook <> 0) then
    begin
      Writeln('Press enter to continue');
      Readln;
    end;
    ExitCode := 1;
{$else MADEXCEPT}
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);

      if (DebugHook <> 0) then
      begin
        Writeln('Press enter to continue');
        Readln;
      end;

      ExitCode := 1;
    end;
{$endif MADEXCEPT}
  end;

  DisplayElapsedTime(sw.ElapsedMilliseconds);

  if (ExitCode = 0) and (FindCmdLineSwitch('pause')) then
  begin
    Writeln('Done - Press enter to continue');
    Readln;
  end;

end.
