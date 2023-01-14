unit debug.info.utilities;

(*
 * Copyright (c) 2021 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  debug.info,
  debug.info.log;


// -----------------------------------------------------------------------------
//
//      FilterModules
//
// -----------------------------------------------------------------------------
// Remove modules from debug info based on name and segment.
// -----------------------------------------------------------------------------
procedure FilterModules(DebugInfo: TDebugInfo; const ModuleFilter: string; Include: boolean; Logger: IDebugInfoModuleLogger);


// -----------------------------------------------------------------------------
//
//      PostImportValidation
//
// -----------------------------------------------------------------------------
// Perform variaous validation on debug info.
// -----------------------------------------------------------------------------
procedure PostImportValidation(DebugInfo: TDebugInfo; Logger: IDebugInfoModuleLogger);


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.Generics.Collections,
  System.Masks,
  System.SysUtils,
  System.StrUtils;

// -----------------------------------------------------------------------------
//
//      FilterModules
//
// -----------------------------------------------------------------------------
procedure FilterModules(DebugInfo: TDebugInfo; const ModuleFilter: string; Include: boolean; Logger: IDebugInfoModuleLogger);
begin
  try
    var MaskValues := ModuleFilter.Split([';']);

    var SymbolsIncludeEliminateCount := 0;
    var SymbolsExcludeEliminateCount := 0;
    var ModulesIncludeEliminateCount := 0;
    var ModulesExcludeEliminateCount := 0;

    var Masks := TObjectList<TMask>.Create;
    var Segments := TList<Word>.Create;
    try
      Masks.Capacity := Length(MaskValues);
      for var MaskValue in MaskValues do
      begin
        var Segment: integer;
        if (MaskValue.Length = 4) and (TryStrToInt(MaskValue, Segment)) and (Segment <= $FFFF) then
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
            Inc(ModulesIncludeEliminateCount);
            Inc(SymbolsIncludeEliminateCount, Module.Symbols.Count);
            Logger.Debug('Include filter eliminated module: %s', [Module.Name])
          end else
          begin
            Inc(ModulesExcludeEliminateCount);
            Inc(SymbolsExcludeEliminateCount, Module.Symbols.Count);
            Logger.Debug('Exclude filter eliminated module: %s', [Module.Name]);
          end;

          DebugInfo.Modules.Remove(Module);
        end;
      end;
    finally
      Segments.Free;
      Masks.Free;
    end;

    if (ModulesIncludeEliminateCount > 0) then
      Logger.Info('Include filter eliminated %.0n module(s), %.0n symbol(s)', [ModulesIncludeEliminateCount * 1.0, SymbolsIncludeEliminateCount * 1.0]);

    if (ModulesExcludeEliminateCount > 0) then
      Logger.Info('Exclude filter eliminated %.0n module(s), %.0n symbols(s)', [ModulesExcludeEliminateCount * 1.0, SymbolsExcludeEliminateCount * 1.0]);

  except
    on E: EMaskException do
      Logger.Error('Invalid filter. %s', [E.Message]);
  end;
end;

// -----------------------------------------------------------------------------
//
//      PostImportValidation
//
// -----------------------------------------------------------------------------
procedure PostImportValidation(DebugInfo: TDebugInfo; Logger: IDebugInfoModuleLogger);
begin
  // Modules with lines but no source or vice versa
  for var Module in DebugInfo.Modules do
  begin
    if (Module.SourceFiles.Count = 0) then
    begin
      if (Module.SourceLines.Count = 0) then
        Logger.Debug('Module has no source files: %s', [Module.Name])
      else
        Logger.Debug('Module has source lines but no source file: %s', [Module.Name]);
    end else
    if (Module.SourceLines.Count = 0) then
      Logger.Debug('Module has source files but no source lines: %s', [Module.Name]);
  end;
end;

// -----------------------------------------------------------------------------

end.

