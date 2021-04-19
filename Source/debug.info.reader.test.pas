unit debug.info.reader.test;

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
  debug.info.reader;

type
  TDebugInfoSyntheticReader = class(TDebugInfoReader)
  private
    procedure SynthesizeDebugInfo(DebugInfo: TDebugInfo);
  protected
  public
    procedure LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo); override;
    procedure LoadFromFile(const Filename: string; DebugInfo: TDebugInfo); override;
  end;



implementation

procedure TDebugInfoSyntheticReader.LoadFromFile(const Filename: string; DebugInfo: TDebugInfo);
begin
  SynthesizeDebugInfo(DebugInfo);
end;

procedure TDebugInfoSyntheticReader.LoadFromStream(Stream: TStream; DebugInfo: TDebugInfo);
begin
  SynthesizeDebugInfo(DebugInfo);
end;

procedure TDebugInfoSyntheticReader.SynthesizeDebugInfo(DebugInfo: TDebugInfo);
begin
//  var Segment := DebugInfo.SegmentClasses.Add(2, sctDATA, 'FOOBAR');
//  Segment.Offset := $00171717;
//  Segment.Size := $00272727;

  var Segment := DebugInfo.Segments.Add(1, '.text', sctCODE);
  Segment.Offset := $00474747;
  Segment.Size := 123456;


  var Module := DebugInfo.Modules.Add('modmodmodmodmod', Segment, $121212, $202020);
  Module.ObjectName := 'objobjobjob.obj';

  var SourceFile := Module.SourceFiles.Add('foofoofoofo.pas');
  Module.SourceLines.Add(SourceFile, $0077, $0000);
  Module.SourceLines.Add(SourceFile, $0099, $0011);

  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');
  Module.SourceLines.Add(SourceFile, $0066, $0022);

  SourceFile := Module.SourceFiles.Add('barbarbarba.pas');
  Module.SourceLines.Add(SourceFile, $0044, $0033);
  Module.SourceLines.Add(SourceFile, $0055, $0044);



  Module := DebugInfo.Modules.Add('zzzzzzzzzzzzzzz', Segment, $101010, $202020);

  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');
  Module.SourceLines.Add(SourceFile, $1111, $0101);


  Module.Symbols.Add('aaaaaaaa', $0f0f0f);
//  Module.Symbols.Add('bbbbbbbb', $200);

//  Module.ObjectName := 'objobjobjob.obj';
//  SourceFile := Module.SourceFiles.Add('yyyyyyyyyyy.pas');

end;


end.

