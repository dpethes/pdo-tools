unit pdo2opf;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, MTProcs, math,
  pdo_common, pdo_util, progress_report, part_outliner, tabs,
  opf_common, face2d_rasterizer, png_writer, utils;

const
  USE_MT = true;
  MIN_DPI = 100;  //low-res textures benefit from some interpolation, 100 looks nice

type
  { PdoToOpf2dTransform }

  PdoToOpf2dTransform = class
  private
      _pdo: TPdoStructure;
      _parts: TOpfPart2dList;
      _log: TPartProgressLog;
      _tex_ctx: TTextureTableCtx;
      _dpi: integer;
      _dpi_min: integer;
      _page: TPageSetup;

      function GetMaxFaceDpi(const face: TPdoFace): integer;
      function IsJunkPart(const bb: TPdoRect): boolean;
      procedure SetupPageSize;
      procedure SetUsedFacesAndBB(const part: TPdoPart; const part_idx: integer; faces: array of TPdoFace; part2d: TOpfPart2d);
      procedure SinglePartRasterizeToPngStream(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  public
      constructor Create(const pdo: TPdoStructure; const log: TPartProgressLog);
      destructor Destroy; override;

      function GetParts: TOpfPart2dList;
      function GetPageSetup: TPageSetup;
      procedure PartsTo2D;
      procedure RasterizeToPngStream(const dpi: integer; dpi_min: integer = MIN_DPI);
      procedure DumpPng(const path: string);
  end;


//**************************************************************************************************
implementation

function PdoToOpfFace(const f: TPdoFace): TOpfFace;
    function ToOpfVert(const v: TPdoFace2DVertex): TOpf2DVertex;
    begin
      result.x := v.x;
      result.y := v.y;
      result.u := v.u;
      result.v := v.v;
    end;
var
  r: TOpfFace;
  i: Integer;
begin
  r.material_index := f.material_index;
  SetLength(r.vertices, Length(f.vertices));
  for i := 0 to Length(f.vertices) - 1 do
       r.vertices[i] := ToOpfVert(f.vertices[i]);
  result := r;
end;


{ PdoToOpf2dTransform }

constructor PdoToOpf2dTransform.Create(const pdo: TPdoStructure; const log: TPartProgressLog);
begin
  _pdo := pdo;
  _log := log;
  _parts := TOpfPart2dList.Create;
end;

destructor PdoToOpf2dTransform.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to _parts.Count - 1 do
      _parts[i].Free;
  _parts.Free;
end;


function PdoToOpf2dTransform.GetMaxFaceDpi(const face: TPdoFace): integer;
var
  i: Integer;
  max_span: single;
  tex_w, tex_h: integer;
  material_idx: integer;

  function GetSpan(const a, b: TPdoFace2DVertex): single;
  var
    dx, dy: single;
    du, dv: single;
    len: single;
  begin
    dx := abs(a.x - b.x);
    dy := abs(a.y - b.y);
    len := sqrt(dx*dx + dy*dy);
    du := abs(a.u - b.u) * tex_w;
    dv := abs(a.v - b.v) * tex_h;
    if len = 0 then
        result := 0
    else
        result := max(du/len, dv/len);
  end;

begin
  result := 0;
  material_idx := face.material_index;
  if (material_idx = -1) or not _pdo.materials[material_idx].has_texture then
      exit;

  tex_w := _pdo.materials[material_idx].texture.width;
  tex_h := _pdo.materials[material_idx].texture.height;
  max_span := GetSpan(face.vertices[0], face.vertices[Length(face.vertices) - 1]);
  for i := 0 to Length(face.vertices) - 2 do begin
      max_span := max(max_span, GetSpan(face.vertices[i], face.vertices[i + 1]));
  end;

  //round to multiplies of 10?
  result := ceil(max_span * 25.4);
end;

{ SetUsedFacesAndBB
  Find faces that belong to a part and calculate bounding box' width & height.
  Pdo part's bounding box contains flaps, so the BB must be calculated from vertex positions.
  Also try to pick optimal DPI based on texture span for each face. If faces don't use textures
  and use multiple materials, the DPI is boosted to have less visible aliasing between faces using
  different materials.
}
procedure PdoToOpf2dTransform.SetUsedFacesAndBB(const part: TPdoPart; const part_idx: integer;
  faces: array of TPdoFace; part2d: TOpfPart2d);
var
  vertex: TPdoFace2DVertex;
  face: TPdoFace;
  width, height: double;
  minx, miny: double;
  face_dpi_max: integer;
  first_material: integer;
  multiple_materials: boolean;
  opf_face: TOpfFace;
begin
  width := 0;
  height := 0;
  //the left/top offset is due to flaps; max flap height shouldn't be above 1000, so use this as default
  minx := 1000;
  miny := 1000;
  part2d.dpi := 0;
  first_material := faces[0].material_index;
  multiple_materials := false;

  for face in faces do begin
      if face.part_index <> part_idx then
          continue;

      opf_face := PdoToOpfFace(face);
      part2d.faces.Add(opf_face);

      for vertex in face.vertices do begin
          if vertex.x > width  then width  := vertex.x;
          if vertex.y > height then height := vertex.y;
          if vertex.x < minx  then minx := vertex.x;
          if vertex.y < miny  then miny := vertex.y;
      end;
      face_dpi_max := GetMaxFaceDpi(face);
      part2d.dpi := Max(part2d.dpi, face_dpi_max);

      if first_material <> face.material_index then
          multiple_materials := true;
  end;

  if multiple_materials and (part2d.dpi = 0) then
      part2d.dpi := MIN_DPI * 3;

  part2d.bounding_box.left := part.bounding_box.left;
  part2d.bounding_box.top  := part.bounding_box.top;
  part2d.bounding_box.width  := width;
  part2d.bounding_box.height := height;

  part2d.bounding_box_vert.left := minx;
  part2d.bounding_box_vert.top  := miny;
  part2d.bounding_box_vert.width  := width  - minx;
  part2d.bounding_box_vert.height := height - miny;
end;


procedure CreateTabsFoldLines(const obj: TPdoObject; const part: TPdoPart; const part2d: TOpfPart2d);
var
  i: integer;
  line: TPdoLine;
  line_extra: TPdoLineExtra;
  vertex0, vertex1: TPdoFace2DVertex;
  flap: TFlap;
  face: TPdoFace;
  edge: TEdge;
begin
  part2d.tabs := TFlapList.Create;
  part2d.fold_lines[ltCut] := TEdgeList.Create;
  part2d.fold_lines[ltMountain] := TEdgeList.Create;
  part2d.fold_lines[ltValley] := TEdgeList.Create;

  for i := 0 to Length(part.lines_extra) - 1 do begin
      line := part.lines[i];
      if line.hidden then
          continue;

      line_extra := part.lines_extra[i];

      face := obj.faces[line.face_index];
      vertex0 := face.vertices[line_extra.vert2didx1];
      vertex1 := face.vertices[line_extra.vert2didx2];

      if line_extra.isflap then begin
          flap := CreateTabCoords(line_extra, vertex0, vertex1);
          part2d.tabs.Add(flap);
          //flap edge can be mountain or valley. the flap is however drawn as quad, and I want to avoid double line drawing
          continue;
      end;

      edge[0].x := vertex0.x;
      edge[0].y := vertex0.y;
      edge[1].x := vertex1.x;
      edge[1].y := vertex1.y;
      if line_extra.Ltype = 0 then
          part2d.fold_lines[ltCut].add(edge)
      else if line_extra.Ltype = 1 then
          part2d.fold_lines[ltMountain].add(edge)
      else if line_extra.Ltype = 2 then
          part2d.fold_lines[ltValley].add(edge);
  end;
end;


function PdoToOpf2dTransform.GetParts: TOpfPart2dList;
begin
  result := _parts;
end;

function PdoToOpf2dTransform.GetPageSetup: TPageSetup;
begin
  result := _page;
end;

function PdoToOpf2dTransform.IsJunkPart(const bb: TPdoRect): boolean;
begin
  result := false;
  if bb.top + bb.height < 0 - _page.margin_top then
      result := true;
  if bb.left + bb.width < 0 - _page.margin_side then
      result := true;
end;

procedure PdoToOpf2dTransform.SetupPageSize;
begin
  if _pdo.settings.page.type_ <> PdoPageTypeOther then begin
      GetPageSizeForPageType(_pdo.settings.page.type_, _page.width, _page.height);
  end else begin
      _page.width  := _pdo.settings.page.custom_width;
      _page.height := _pdo.settings.page.custom_height;
  end;
  _page.margin_side := _pdo.settings.page.margin_side;
  _page.margin_top  := _pdo.settings.page.margin_top;
  if _pdo.settings.page.orientation = 1 then begin
      Swap2f(_page.width, _page.height);
      Swap2f(_page.margin_side, _page.margin_top);
  end;

  _page.clipped_width  := _page.width  - 2 * _page.margin_side;
  _page.clipped_height := _page.height - 2 * _page.margin_top;
end;


procedure PdoToOpf2dTransform.PartsTo2D;
var
  part: TPdoPart;
  obj: TPdoObject;
  part2d: TOpfPart2d;
  bb_top, bb_left: single;
  part_idx: integer;

begin
  _log.BeginWriting(Length(_pdo.parts));

  SetupPageSize;
  GetLineTypes(_pdo);
  SetLines(_pdo);

  _parts.Capacity := Length(_pdo.parts);
  part_idx := 0;
  for part in _pdo.parts do begin
      obj := _pdo.objects[part.object_index];

      part2d := TOpfPart2d.Create();
      part2d.name := part.name;
      SetUsedFacesAndBB(part, part_idx, obj.faces, part2d);
      part_idx += 1;

      //throw away parts that are positioned out of pages
      if IsJunkPart(part2d.bounding_box) then begin
          part2d.Free;
          continue;
      end;

      //for debugging: skip all parts that don't match the name
      //part2d.name += '__part_idx:' + IntToStr(part_idx);
      //if (part2d.name <> 'object__part_idx:1') then continue;

      CreateTabsFoldLines(obj, part, part2d);

      //get real bounding box top/left position based on vertices.
      //Stored BB can be crappy - extended even beyond vertices and tabs
      bb_left := part2d.bounding_box.left + part2d.bounding_box_vert.left;
      bb_top  := part2d.bounding_box.top  + part2d.bounding_box_vert.top;
      part2d.page_w := floor( bb_left / _page.clipped_width );
      part2d.page_h := floor( bb_top / _page.clipped_height );

      _parts.Add(part2d);
      _log.PartWritten;
  end;
  _log.EndWriting;
end;

procedure PdoToOpf2dTransform.SinglePartRasterizeToPngStream(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  rasterizer: TFace2dRasterizer;
  part: TOpfPart2d;
  face: TOpfFace;
  pixbuf: TRasterizerTexture;
  i: Integer;
  dpi: Integer;
begin
  part := TOpfPart2dList(Data)[Index];

  dpi := max(_dpi_min, min(part.dpi, _dpi));
  rasterizer := TFace2dRasterizer.Create(_tex_ctx);
  rasterizer.BeginPart(part.bounding_box_vert, dpi);

  for i := 0 to part.faces.Count - 1 do begin
      face := part.faces[i];
      rasterizer.RenderFace(face);
  end;
  pixbuf := rasterizer.GetPixelBuffer;
  part.png_stream := ImageToPngStream(pixbuf.pixels, pixbuf.width, pixbuf.height);
  part.png_w := pixbuf.width;
  part.png_h := pixbuf.height;
  freemem(pixbuf.pixels);

  rasterizer.EndPart;
  rasterizer.Free;
  _log.PartWritten;
end;

procedure PdoToOpf2dTransform.RasterizeToPngStream(const dpi: integer; dpi_min: integer);
var
  i: integer;
  mpix: int64;
begin
  _log.BeginWriting(_parts.Count);
  _tex_ctx := TFace2dRasterizer.BuildTextureTable(_pdo);
  _dpi := dpi;
  _dpi_min := dpi_min;

  if USE_MT then begin
      ProcThreadPool.DoParallel(@SinglePartRasterizeToPngStream, 0, _parts.Count - 1, _parts);
  end else begin
      for i := 0 to _parts.Count - 1 do
          SinglePartRasterizeToPngStream(i, _parts, nil);
  end;

  TFace2dRasterizer.DestroyTextureTable(_tex_ctx);
  _log.EndWriting;

  mpix := 0;
  for i := 0 to _parts.Count - 1 do begin
      mpix += _parts[i].png_w * _parts[i].png_h;
  end;
  //writeln('MPix rendered: ', mpix / (1 shl 20):7:2);
end;

procedure PdoToOpf2dTransform.DumpPng(const path: string);
var
  part: TOpfPart2d;
  i: integer;
  name: string;
begin
  i := 0;
  for part in _parts do begin
      i += 1;
      name := path + 'part' + IntToStr(i);
      //name := name + '_dpi' + IntToStr(part.dpi);  //for DPI debugging
      part.png_stream.SaveToFile(name + '.png');
  end;
end;


end.

