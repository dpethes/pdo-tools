{
 Pdo tools - PDO format extraction and conversion tools
 Copyright (C) 2015 David Pethes

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit opf_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, glist,
  pdo_common, part_outliner, tabs;

type
  TPageSetup = record
      clipped_width,
      clipped_height: single;
      width,
      height: single;
      margin_top: single;
      margin_side: single;
  end;

  TOpf2DVertex = record
      x, y,
      u, v: double;
  end;
  POpf2DVertex = ^TOpf2DVertex;

  TOpfFace = record
      material_index: integer;
      vertices: array of TOpf2DVertex;
  end;
  TOpfFaceList = specialize TList<TOpfFace>;

  { TOpfPart2d }

  TOpfPart2d = class
  public
      name: string;
      bounding_box: TPdoRect;
      bounding_box_vert: TPdoRect;  //bounding box defined by vertices
      faces: TOpfFaceList;
      outline_paths: TPathList;
      tabs: TFlapList;
      fold_lines: array[ltCut..ltValley] of TEdgeList;
      dpi: integer;
      png_stream: TMemoryStream;
      png_w, png_h: integer;
      page_w, page_h: integer;

      outline_edges_debug: TEdgeList;

      constructor Create;
      destructor Destroy; override;
  end;

  TOpfPart2dList = specialize TFPGList<TOpfPart2d>;

implementation

{ TOpfPart2d }

constructor TOpfPart2d.Create;
begin
  faces := TOpfFaceList.Create;
  png_stream := nil;
  outline_edges_debug := nil;
  outline_paths := nil;
  tabs := nil;
  fold_lines[ltCut] := nil;
  fold_lines[ltMountain] := nil;
  fold_lines[ltValley] := nil;
end;

destructor TOpfPart2d.Destroy;
begin
  inherited Destroy;
  faces.Free;
  if outline_edges_debug <> nil then outline_edges_debug.Free;
  if outline_paths <> nil then begin
      FreePathList(outline_paths);
      outline_paths.Free;
  end;
  if tabs <> nil then tabs.Free;
  if fold_lines[ltCut] <> nil then fold_lines[ltCut].Free;
  if fold_lines[ltMountain] <> nil then fold_lines[ltMountain].Free;
  if fold_lines[ltValley] <> nil then fold_lines[ltValley].Free;
  if png_stream <> nil then png_stream.Free;
end;

end.

