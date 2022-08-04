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
unit opf2vector;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, math, contnrs, glist,
  pdo_common, pdo_util, opf_common, part_outliner, tabs,
  vector_writer, pdf_writer, svg_writer, png_writer;

type
  TImgList = specialize TList<TPdoImage>;
  TTextList = specialize TList<TPdoTextBlock>;
  TVecLayerMap = specialize TFPGMap<String, TVecLayerId>;

  TVectorExportFormat = (TvfPdf, TvfSvg);

  TVectorExportOptions = record
      tabs,
      foldlines,
      extended_foldlines,
      outlines,
      textures,
      faces,
      debug_edges: boolean;  //switch layers on/off
      outlines_with_tabs: boolean;  //make tabs part of the outline (useful for machine cutting)
      multi_file: boolean;   //output to multiple files
      codepage: string;      //override charset (if not empty)
  end;

  { TOpf2dVectorExport }

  TOpf2dVectorExport = class
    private
      _pdo: TPdoStructure;
      _parts: TOpfPart2dList;
      _page: TPageSetup;
      _strmap: TFPStringHashTable;
      _layermap: TVecLayerMap;
      _opts: TVectorExportOptions;

      procedure DrawBasicParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
      procedure DrawTabs(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
      procedure DrawImages(var pdfw: TVectorWriter; const images: TImgList);
      procedure DrawPageParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
      procedure DrawTexts(var pdfw: TVectorWriter; const texts: TTextList);
      procedure DrawTexturedParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList; const pvertex_buffer: psingle);
      procedure InitLayers(var pdfw: TVectorWriter);
    public
      constructor Create(const pdo: TPdoStructure; const parts: TOpfPart2dList; const page: TPageSetup);
      procedure Prepare(const options: TVectorExportOptions);
      procedure ExportToFile(const file_name: string; const format: TVectorExportFormat);
  end;

procedure DefaultVectorExportOptions(out opt: TVectorExportOptions);

implementation

procedure DefaultVectorExportOptions(out opt: TVectorExportOptions);
begin
  opt.outlines := true;
  opt.outlines_with_tabs := false;
  opt.textures := true;
  opt.foldlines := true;
  opt.extended_foldlines := true;
  opt.tabs := true;
  opt.debug_edges := false;
  opt.faces := false;
  opt.multi_file := false;
  opt.codepage := '';
end;

function OpfFacesToOutlinerFaces(const faces: TOpfFaceList): TOutlinerFaceList;
var
  face: TOpfFace;
  vertex_count: integer;
  outface: TOutlinerFace;
  i, f: Integer;
begin
  result := TOutlinerFaceList.Create;
  for f := 0 to faces.Count - 1 do begin
      face := faces[f];
      vertex_count := Length(face.vertices);
      SetLength(outface.vertices, vertex_count);
      for i := 0 to vertex_count - 1 do begin
          outface.vertices[i].x := face.vertices[i].x;
          outface.vertices[i].y := face.vertices[i].y;
      end;
      result.Add(outface);
  end;
end;

{ CreateOutline
  Create part's outline (clipping path) by detecting its edges
  Also clip tabs, so they don't cross edges.
}
procedure CreateOutline(part2d: TOpfPart2d; include_tabs: boolean);
var
  outliner_faces: TOutlinerFaceList;
  edges: TEdgeList;
  tab: TFlap;
  i, k: Integer;
  tabface: TOutlinerFace;
begin
  outliner_faces := OpfFacesToOutlinerFaces(part2d.faces);
  edges := FindOutlineEdges(outliner_faces);
  outliner_faces.Free;
  part2d.outline_paths := MergeEdgesToPaths(edges);
  ClipTabs(part2d.tabs, edges);

  //edges are unused behind this point, store them just for debug layer
  part2d.outline_edges_debug := edges;

  //pass 2: extend outline with clipped tabs
  //add tabs as faces to other faces to make them part of the outline
  if include_tabs then begin
      outliner_faces := OpfFacesToOutlinerFaces(part2d.faces);
      for i := 0 to part2d.tabs.Count - 1 do begin
          tab := part2d.tabs[i];
          SetLength(tabface.vertices, tab.vertex_count);
          for k := 0 to tab.vertex_count - 1 do
              tabface.vertices[k] := tab.vertices[k];
          outliner_faces.Add(tabface);
      end;

      edges := FindOutlineEdges(outliner_faces);
      outliner_faces.Free;
      part2d.outline_paths := MergeEdgesToPaths(edges);
  end;
end;

{ TOpf2dVectorExport }

constructor TOpf2dVectorExport.Create(const pdo: TPdoStructure; const parts: TOpfPart2dList;
  const page: TPageSetup);
begin
  _pdo := pdo;
  _parts := parts;
  _page := page;
end;

{ Prepare stuff that can be reused by multiple exports
}
procedure TOpf2dVectorExport.Prepare(const options: TVectorExportOptions);
var
  part: TOpfPart2d;
begin
  _opts := options;
  for part in _parts do begin
      CreateOutline(part, options.outlines_with_tabs);
  end;
end;

{ PartsWherePageXY
  Get parts that are on the page given by page coordinates.
}
function PartsWherePageXY(const parts: TOpfPart2dList; const x, y: integer): TOpfPart2dList;
var
  i: integer;
  in_page, has_outline: boolean;
begin
  result := TOpfPart2dList.Create;
  for i := 0 to parts.Count - 1 do begin
      in_page := (parts[i].page_w = x) and (parts[i].page_h = y);
      has_outline := parts[i].outline_paths.Count > 0;
      if in_page and has_outline then
          result.Add(parts[i]);
  end;
end;


{ ImagesWherePageXY
  Get images for page by page coordinates.
  Check if image spans to given page - image can have small overhangs to other pages,
  but they should be within small margin
}
function ImagesWherePageXY(const images: array of TPdoImage; const page: TPageSetup; const x, y: integer): TImgList;

  function is_in_page(const x, span, page_dim: single; const expected: integer): boolean;
  const
    PAGE_MARGIN = 10.0;
  var
    a, b: single;
  begin
    a := floor( (x + PAGE_MARGIN) / page_dim );
    b := floor( (x + span - PAGE_MARGIN) / page_dim );
    result := (a = expected) or (b = expected)
  end;

var
  img: TPdoImage;
  in_page_w: Boolean;
  in_page_h: Boolean;
begin
  result := TImgList.Create;
  for img in images do begin
      in_page_w := is_in_page(img.bounding_box.left, img.bounding_box.width, page.clipped_width, x);
      in_page_h := is_in_page(img.bounding_box.top, img.bounding_box.height, page.clipped_height, y);
      if in_page_w and in_page_h then
          result.Add(img);
  end;
end;


{ TextsWherePageXY
  Get parts that are on the page given by page coordinates.
}
function TextsWherePageXY(textblocks: array of TPdoTextBlock; const page: TPageSetup; const x, y: integer): TTextList;
  function is_in_page(const x, page_dim: single; const expected: integer): boolean;
  begin
    result := floor(x / page_dim) = expected
  end;
var
  txt: TPdoTextBlock;
  in_page_w: Boolean;
  in_page_h: Boolean;
begin
  result := TTextList.Create;
  for txt in textblocks do begin
      in_page_w := is_in_page(txt.bounding_box.left, page.clipped_width, x);
      in_page_h := is_in_page(txt.bounding_box.top, page.clipped_height, y);
      if in_page_w and in_page_h then
          result.Add(txt);
  end;
end;


{ InitLayers
  Define layers in bottom-to-top order
}
procedure TOpf2dVectorExport.InitLayers(var pdfw: TVectorWriter);
begin
  _layermap := TVecLayerMap.Create;
  _layermap.Add('tabs', pdfw.GenerateLayer('tabs'));
  _layermap.Add('foldlines-extended', pdfw.GenerateLayer('foldlines-extended'));
  _layermap.Add('faces', pdfw.GenerateLayer('faces'));
  _layermap.Add('textures', pdfw.GenerateLayer('textures'));
  _layermap.Add('outlines', pdfw.GenerateLayer('outlines'));
  _layermap.Add('foldlines', pdfw.GenerateLayer('foldlines'));
  _layermap.Add('texts', pdfw.GenerateLayer('texts'));
  if _opts.debug_edges then
      _layermap.Add('debug', pdfw.GenerateLayer('debug'));
end;


{ ExportToFile
  Export 2D layout to file in PDF or SVG vector format.
}
procedure TOpf2dVectorExport.ExportToFile(const file_name: string; const format: TVectorExportFormat);
var
  pdfw: TVectorWriter;
  part: TOpfPart2d;
  parts: TOpfPart2dList;
  pages: record
      horizontal, vertical: integer;
  end;
  px, py: integer;
  images: TImgList;
  texts: TTextList;
  page_is_empty: boolean;
  page_number: integer;
  extension: array[TVectorExportFormat] of string[4] = ('.pdf', '.svg');
  codepage: String;

  procedure InitWriter;
  begin
    if format = TvfSvg then
        pdfw := TSvgWriter.Create()
    else
        pdfw := TPdfWriter.Create();

    InitLayers(pdfw);
    pdfw.InitDocProperties(_page.width, _page.height, _pdo.header.codepage = CodepageShiftJIS);
  end;

  procedure CloseWriter;
  var
    output_filename: string;
  begin
    output_filename := file_name;
    if _opts.multi_file then
        output_filename += '_page' + IntToStr(page_number);

    pdfw.SaveToFile(output_filename + extension[format]);
    pdfw.Free;
  end;

begin
  pages.horizontal := 0;
  pages.vertical := 0;
  for part in _parts do begin
      if part.page_w > pages.horizontal then
          pages.horizontal := part.page_w;
      if part.page_h > pages.vertical then
          pages.vertical := part.page_h;
  end;

  codepage := _pdo.header.codepage;
  if _opts.codepage <> '' then
      codepage := _opts.codepage;
  _strmap := PdoStringsToUtf8(_pdo, codepage);
  if not _opts.multi_file then
      InitWriter;

  page_number := 0;
  for py := 0 to pages.vertical do begin
      for px := 0 to pages.horizontal do begin

          if _opts.multi_file then
              InitWriter;

          parts  := PartsWherePageXY (_parts, px, py);
          images := ImagesWherePageXY(_pdo.images, _page, px, py);
          texts  := TextsWherePageXY (_pdo.textblocks, _page, px, py);

          page_is_empty := (parts.Count + images.Count + texts.Count) = 0;
          if not page_is_empty then begin
              pdfw.AddPage;
              pdfw.SetLineWidth(0.1);

              if (format = TvfPdf) or _opts.multi_file then
                  pdfw.SetCoordOffset(px * _page.clipped_width  - _page.margin_side,
                                      py * _page.clipped_height - _page.margin_top );

              DrawImages(pdfw, images);
              DrawTexts(pdfw, texts);
              DrawPageParts(pdfw, parts);
          end;

          parts.Free;
          images.Free;
          texts.Free;
          page_number += 1;

          if _opts.multi_file and (not page_is_empty) then
              CloseWriter;
      end;
  end;

  if not _opts.multi_file then
      CloseWriter;
  _strmap.Free;
  _layermap.Free;
end;


procedure TOpf2dVectorExport.DrawPageParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
var
  pvertex_buffer: psingle;

  procedure ExtendLine(const Ax, Ay: single; var Bx, By: single);
  const
    LENGTH_EXTEND = 4;
  var
     dx, dy, length: single;
  begin
    dx := bx - ax;
    dy := by - ay;
    length := sqrt(dx * dx + dy * dy);
    bx := Ax + dx / length * (length + LENGTH_EXTEND);
    by := Ay + dy / length * (length + LENGTH_EXTEND);
  end;

  procedure DrawEdgeList(const edge_list: TEdgeList; const bbox: TPdoRect; const extended: boolean = false);
  var
    i: integer;
    Ax, Ay, Bx, By: single;
  begin
    for i := 0 to edge_list.Count - 1 do begin
        Ax := edge_list[i][0].x + bbox.left;
        Ay := edge_list[i][0].y + bbox.top;
        Bx := edge_list[i][1].x + bbox.left;
        By := edge_list[i][1].y + bbox.top;

        if extended then begin
            ExtendLine(Ax, Ay, Bx, By);
            ExtendLine(Bx, By, Ax, Ay);
        end;

        pvertex_buffer[0] := Ax;
        pvertex_buffer[1] := Ay;
        pvertex_buffer[2] := Bx;
        pvertex_buffer[3] := By;
        pdfw.DrawLine(pvertex_buffer);
    end;
  end;

  procedure DrawDebugLayer(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
  var
    part: TOpfPart2d;
    bbox: TPdoRect;
    i: integer;
  begin
    pdfw.SetLayer(_layermap['debug']);
    pdfw.SetSolidLine();
    i := 0;
    for part in parts do begin
        bbox := part.bounding_box;
        DrawEdgeList(part.outline_edges_debug, bbox);
        //pdfw.Print(IntToStr(i), bbox.left, bbox.top, 6);
        pdfw.Print(part.name, bbox.left, bbox.top, 6);
        i += 1;
    end;
  end;

  procedure DrawFoldLines(const extended: boolean = false);
  var
    part: TOpfPart2d;
    bbox: TPdoRect;
  begin
    for part in parts do begin
        bbox := part.bounding_box;
        pdfw.SetDashedLine();
        DrawEdgeList(part.fold_lines[ltValley], bbox, extended);
        pdfw.SetSolidLine();
        DrawEdgeList(part.fold_lines[ltMountain], bbox, extended);
    end;
  end;

begin
  pdfw.SetLayer(_layermap['outlines']);
  pvertex_buffer := getmem(1 shl 20);

  //tabs
  if _opts.tabs then begin
      pdfw.SetLayer(_layermap['tabs']);
      DrawTabs(pdfw, parts);
  end;

  //extended fold lines
  if _opts.extended_foldlines then begin
      pdfw.SetLayer(_layermap['foldlines-extended']);
      DrawFoldLines(true);
  end;

  //single-colored faces
  if _opts.faces then
      DrawBasicParts(pdfw, parts);

  //textures + clipping + outline
  DrawTexturedParts(pdfw, parts, pvertex_buffer);

  //regular (pdo-style) fold lines
  if _opts.foldlines then begin
      pdfw.SetLayer(_layermap['foldlines']);
      DrawFoldLines;
  end;

  //debug info on top
  if _opts.debug_edges then
      DrawDebugLayer(pdfw, parts);

  freemem(pvertex_buffer);
end;

{ PdoToOpf2dTransform
  Export part's outline and separately set the outline as the clipping path for part's bitmap.
}
procedure TOpf2dVectorExport.DrawTexturedParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList;
  const pvertex_buffer: psingle);
var
  vertex_list: TVertexList;
  bbox: TPdoRect;
  vertex: TEdgeVertex;
  i: integer;
  part: TOpfPart2d;
  path_idx: integer;
  polys: array of TVectorPoly;
  vertex_buffer_current: psingle;
begin
  for part in parts do begin
      SetLength(polys, part.outline_paths.Count);
      vertex_buffer_current := pvertex_buffer;

      for path_idx := 0 to part.outline_paths.Count - 1 do begin
          vertex_list := part.outline_paths[path_idx];
          bbox := part.bounding_box;
          polys[path_idx].coords := vertex_buffer_current;
          polys[path_idx].count := vertex_list.Count;

          for i := 0 to vertex_list.Count - 1 do begin
              vertex := vertex_list[i];
              vertex_buffer_current[i * 2    ] := bbox.left + vertex.x;
              vertex_buffer_current[i * 2 + 1] := bbox.top  + vertex.y;
          end;

          vertex_buffer_current += vertex_list.Count * 2 * 4;
      end;

      pdfw.PushState;
      pdfw.SetClippingPolys(polys);

      if _opts.textures then begin
          pdfw.SetLayer(_layermap['textures']);
          //use bounding box of the vertices - rasterized image contains only faces
          bbox.left += part.bounding_box_vert.left;
          bbox.top  += part.bounding_box_vert.top;
          bbox.width -= part.bounding_box_vert.left;
          bbox.height -= part.bounding_box_vert.top;
          pdfw.DrawPngImage(part.png_stream.Memory, part.png_stream.Size,
                            bbox.left, bbox.top + bbox.height,
                            bbox.width, bbox.height);
      end;

      if _opts.outlines then begin
          pdfw.SetLayer(_layermap['outlines']);
          for path_idx := 0 to part.outline_paths.Count - 1 do
              pdfw.DrawPoly(polys[path_idx].coords, polys[path_idx].count);
      end;

      pdfw.PopState;
  end;
end;


{ DrawBasicParts
  Draw triangles or quads straight as they are defined in pdo
}
procedure TOpf2dVectorExport.DrawBasicParts(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
var
  part: TOpfPart2d;
  bbox: TPdoRect;
  f: TOpfFace;
  v: TOpf2DVertex;
  k, i: Integer;
  vbuffer: array of Single;
begin
  pdfw.SetLayer(_layermap['faces']);
  pdfw.SetLineStyle(lsFace);
  SetLength(vbuffer, 2 * 4);
  for part in parts do begin
      bbox := part.bounding_box;
      for k := 0 to part.faces.Count - 1 do begin
          f := part.faces[k];
          if Length(vbuffer) < Length(f.vertices) * 2 then
              SetLength(vbuffer, Length(f.vertices) * 2);
          for i := 0 to Length(f.vertices) - 1 do begin
              v := f.vertices[i];
              vbuffer[2 * i]     := v.x + bbox.left;
              vbuffer[2 * i + 1] := v.y + bbox.top;
          end;
          pdfw.DrawPoly(@vbuffer[0], Length(f.vertices));
      end;
  end;
  pdfw.SetSolidLine;
  vbuffer := nil;
end;


procedure TOpf2dVectorExport.DrawImages(var pdfw: TVectorWriter; const images: TImgList);
var
  img: TPdoImage;
  data: pbyte;
  png_stream: TMemoryStream;
  bbox: TPdoRect;
  i: Integer;
begin
  pdfw.SetLayer(_layermap['textures']);
  for i := 0 to images.Count - 1 do begin
      img := images[i];
      data := _pdo.tex_storage.GetPixels(img.texture.texture_id);
      bbox := img.bounding_box;
      if pdfw is TSvgWriter then begin
          //svg writer takes only PNG data
          png_stream := ImageToPngStream(data, img.texture.width, img.texture.height);
          pdfw.DrawPngImage(png_stream.Memory, png_stream.Size,
                      bbox.left, bbox.top + bbox.height,
                      bbox.width, bbox.height);
          png_stream.Free;
      end else begin
          pdfw.DrawImage(data, img.texture.width, img.texture.height,
                      bbox.left, bbox.top + bbox.height,
                      bbox.width, bbox.height);
      end;
  end;
end;


{ DrawTabs
  Draws triangles or quads used as tabs.
  The vertices are shuffled around to have the part-tab edge drawn by path closing segment.
}
procedure TOpf2dVectorExport.DrawTabs(var pdfw: TVectorWriter; const parts: TOpfPart2dList);
const
  VertexShuffle: array[0..1, 0..3] of byte = (
    (0, 2, 1, 0),
    (0, 3, 2, 1)
  );
var
  part: TOpfPart2d;
  bbox: TPdoRect;
  nvertices: integer;
  k: integer;
  vertex_buffer: array[0..3] of record
    x, y: single;
  end;
  flap: TFlap;

  procedure Load(const count: integer; const shuffle: array of byte);
  var
    vertex: TEdgeVertex;
    i: integer;
  begin
    for i := 0 to count - 1 do begin
        vertex := flap.vertices[shuffle[i]];
        vertex_buffer[i].x := bbox.left + vertex.x;
        vertex_buffer[i].y := bbox.top + vertex.y;
    end;
  end;

begin
  for part in parts do begin
      bbox := part.bounding_box;
      for k := 0 to part.tabs.Count - 1 do begin
          flap := part.tabs[k];
          nvertices := flap.vertex_count;
          Load(nvertices, VertexShuffle[nvertices - 3]);
          pdfw.DrawPoly(@vertex_buffer, nvertices);
      end;
  end;
end;


procedure TOpf2dVectorExport.DrawTexts(var pdfw: TVectorWriter; const texts: TTextList);
var
  text: TPdoTextBlock;
  str: string;
  i, k, lines, font_size: integer;
  x, y, line_height: single;

begin
  pdfw.SetLayer(_layermap['texts']);
  for k := 0 to texts.Count - 1 do begin
      text := texts[k];
      lines := Length(text.lines);
      line_height := text.bounding_box.height / lines;
      font_size := text.font_size div 4 + 1;
      x := text.bounding_box.left;
      y := text.bounding_box.top;
      for i := 0 to lines - 1 do begin
          y += line_height;
          str := _strmap[text.lines[i]];
          pdfw.Print(str, x, y, font_size);
      end;
  end;
end;


end.

