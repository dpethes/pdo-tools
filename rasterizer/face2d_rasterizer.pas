unit face2d_rasterizer;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  rasterizer2d, pdo_common, utils, opf_common;

type
  TRasterizerTexture = record
      width, height: integer;
      size: integer;
      pixels: pbyte;
      tex: TTexture2D;
  end;

  TTextureTableCtx = record
      textures: array of TRasterizerTexture;  //table matIndex -> texture data
      colors: array of TPixelRGBA;            //table matIndex -> material color
  end;

  { TFace2dRasterizer }

  TFace2dRasterizer = class
    private
      rasterizer: TPoly2dRasterizer;
      x_mult, y_mult: single;   //multipliers to transform vertex to unit size
      x_sub, y_sub: single;     //constant x/y subtracts
      width, height: integer;
      part_counter: integer;
      texctx: TTextureTableCtx;

      procedure ExpandEdgeColor(const pixels: pbyte);
    public
      DisableTexturing: boolean;

      constructor Create(var texture_table_context: TTextureTableCtx);
      destructor Destroy; override;
      procedure RenderFace(const face: TOpfFace);
      procedure BeginPart(const boundingBox: TPdoRect; const dpi: integer);
      function GetPixelBuffer: TRasterizerTexture;
      procedure EndPart;
      class function BuildTextureTable(const pdo: TPdoStructure): TTextureTableCtx;
      class procedure DestroyTextureTable(var table: TTextureTableCtx);
  end;


implementation

const
  BackColor = 255;
  ALPHA_OPAQUE = 0;
  MAX_IMAGE_WIDTH = 2048;


{ Pack 32bit RGBA source to 24bit RGB destination.
}
procedure PackRgbaToRgb(src, dst: pbyte; const size: integer);
var
  i: Integer;
begin
  for i := 0 to size - 1 do begin
      plongword(dst)^ := plongword(src)^;
      dst += 3;
      src += 4;
  end;
end;

{ TFace2dRasterizer }

class function TFace2dRasterizer.BuildTextureTable(const pdo: TPdoStructure): TTextureTableCtx;
var
  i: integer;
  tex: TPdoTexture;
  num_materials: integer;
  pixcolor: TPixelRGBA;
  tex_data: pbyte;
begin
  num_materials := length(pdo.materials);
  SetLength(result.textures, num_materials);
  SetLength(result.colors, num_materials);

  for i := 0 to num_materials - 1 do begin
      if pdo.materials[i].has_texture then begin
          tex := pdo.materials[i].texture;
          tex_data := pdo.tex_storage.GetPixels(tex.texture_id);
          result.textures[i].width  := tex.width;
          result.textures[i].height := tex.height;
          result.textures[i].pixels := tex_data;
          result.textures[i].tex := TTexture2D.Create(tex_data, tex.width, tex.height);
      end
      else begin
          result.textures[i].width := 0;
          result.textures[i].height := 0;
          result.textures[i].pixels := nil;
      end;
      pixcolor.r := round(pdo.materials[i].color2d_rgba[0] * 255);
      pixcolor.g := round(pdo.materials[i].color2d_rgba[1] * 255);
      pixcolor.b := round(pdo.materials[i].color2d_rgba[2] * 255);
      pixcolor.a := ALPHA_OPAQUE;
      result.colors[i] := pixcolor;
  end;
end;


class procedure TFace2dRasterizer.DestroyTextureTable(var table: TTextureTableCtx);
var
  i: integer;
begin
  for i := 0 to Length(table.textures) - 1 do
      if table.textures[i].pixels <> nil then begin
          table.textures[i].tex.Free;
      end;
  table.textures := nil;
  table.colors := nil;
end;


{ Duplicate the fully opaque edge pixels in horizontal and vertical direction.
  Rasterizer draws only pixels, whose centers are inside the triangle's edges.
  Pixels whose center is not inside, their corners are, are not drawn. Therefore a
  small amount of uncolored area is visible between vector edges of the triangle
  and the rendered texture. Edge pixel duplication is a way to remedy this effect.
}
procedure TFace2dRasterizer.ExpandEdgeColor(const pixels: pbyte);
var
  stride: integer;

  procedure ExpandPixelsHoriz(const h, w: integer);
  var
    x, y: integer;
    p, a, b: PByte;
    pa: plongword;
    alpha: array[0..MAX_IMAGE_WIDTH-1] of byte;
  begin
    FillByte(alpha, MAX_IMAGE_WIDTH, 0);
    for y := 0 to h - 1 do begin
        p := pixels + y * stride;
        pa := plongword(p);
        for x := 0 to w - 1 do
            alpha[x] := pa[x] >> 24;

        for x := 0 to w - 2 do begin
            if alpha[x] <> alpha[x+1] then begin
                a := p + x * COLOR_SAMPLE_SIZE;
                b := a + COLOR_SAMPLE_SIZE;
                if alpha[x] <> ALPHA_OPAQUE then begin
                    plongword(a)^ := plongword(b)^;
                end else begin
                    plongword(b)^ := plongword(a)^;
                end;
            end;
        end;
    end;
  end;

  procedure ExpandPixelsVert(const h, w: integer);
  var
    x, y: integer;
    p, a, b: PByte;
    pa: plongword;
    alpha: array[0..MAX_IMAGE_WIDTH-1] of byte;
  begin
    FillByte(alpha, MAX_IMAGE_WIDTH, 0);
    for x := 0 to w - 1 do begin
        p := pixels + x * COLOR_SAMPLE_SIZE;
        pa := plongword(p);
        for y := 0 to h - 1 do
            alpha[y] := pa[y*width] >> 24;

        for y := 0 to h - 2 do begin
            if alpha[y] <> alpha[y+1] then begin
                a := p + y * stride;
                b := a + stride;
                if alpha[y] <> ALPHA_OPAQUE then begin
                    plongword(a)^ := plongword(b)^;
                end else begin
                    plongword(b)^ := plongword(a)^;
                end;
            end;
        end;
    end;
  end;

begin
  stride := width * COLOR_SAMPLE_SIZE;
  ExpandPixelsHoriz(height, width);
  ExpandPixelsVert(height, width);
end;

constructor TFace2dRasterizer.Create(var texture_table_context: TTextureTableCtx);
begin
  part_counter := 0;
  DisableTexturing := false;
  texctx := texture_table_context;
end;

destructor TFace2dRasterizer.Destroy;
begin
  inherited Destroy;
end;

{ RenderFace
  Assemble triangles from face's vertices and send them to rasterizer, using proper per-face material/texture
}
procedure TFace2dRasterizer.RenderFace(const face: TOpfFace);
const
  ColorWhite: TPixelRGBA = (r:255; g:255; b:255; a:0);
var
  vertices: POpf2DVertex;
  num_vertices: integer;
  vertex_buffer: array[0..3] of TVertexf;
  i: integer;
  material_idx: integer;
  tex: TRasterizerTexture;
  color: TPixelRGBA;
  triptr: ^TTriangle;

  procedure LoadToVbuffer(const dst, src: integer);
  begin
      vertex_buffer[dst].x := clip3(0, (vertices[src].x - x_sub) * x_mult - 0.5, width);
      vertex_buffer[dst].y := clip3(0, (vertices[src].y - y_sub) * y_mult - 0.5, height);
      vertex_buffer[dst].u := vertices[src].u;
      vertex_buffer[dst].v := vertices[src].v;
  end;

begin
  if DisableTexturing then
      exit;

  tex.width := 0;
  color := ColorWhite;
  material_idx := face.material_index;
  if material_idx <> -1 then begin
      tex := texctx.textures[material_idx];
      color := texctx.colors[material_idx];
  end;
  if tex.width > 0 then
      rasterizer.BindTexture(tex.tex, true)
  else begin
      rasterizer.ReleaseTexture;
      rasterizer.SetSolidColor(color);
  end;

  triptr := @vertex_buffer;
  vertices := @face.vertices[0];
  num_vertices := Length(face.vertices);

  if num_vertices = 3 then begin
      for i := 0 to num_vertices - 1 do
          LoadToVbuffer(i, i);
      rasterizer.DrawTriangle(triptr^);
  end else begin
      //slow path: split N-gons to triangles
      for i := 1 to num_vertices - 2 do begin
          LoadToVbuffer(0, 0);
          LoadToVbuffer(1, i);
          LoadToVbuffer(2, i + 1);
          rasterizer.DrawTriangle(triptr^);
      end;
  end;
end;

procedure TFace2dRasterizer.BeginPart(const boundingBox: TPdoRect; const dpi: integer);
var
  dpmm: single;  //dots per mm
begin
  //rasterizer's buffer is reduced to 2048x2048, so do the same
  dpmm := dpi / 25.4;
  width  := clip3(1, trunc(boundingBox.width  * dpmm), MAX_IMAGE_WIDTH);
  height := clip3(1, trunc(boundingBox.height * dpmm), MAX_IMAGE_WIDTH);

  x_mult := width / boundingBox.width;
  y_mult := height / boundingBox.height;
  x_sub := boundingBox.left;
  y_sub := boundingBox.top;

  rasterizer := TPoly2dRasterizer.Create();
  rasterizer.InitWindow(width, height);
  rasterizer.Clear(BackColor);
  part_counter += 1;
end;


function TFace2dRasterizer.GetPixelBuffer: TRasterizerTexture;
var
  data, p: pbyte;
begin
  p := getmem(width * height * 3 + 4);  //4B padding for PackRgbaToRgb
  data := rasterizer.GetColorBuffer();
  ExpandEdgeColor(data);
  PackRgbaToRgb(data, p, width * height);

  result.pixels := p;
  result.size := width * height * 3;
  result.width := width;
  result.height := height;
end;

procedure TFace2dRasterizer.EndPart;
//var
//  dst: pbyte;
begin
  //dst := rasterizer.GetColorBuffer();
  //PnmSave ('r:\part' + IntToStr(part_counter) + '.pnm', dst, width, height);
  //writeln('rasterized tris: ', rasterizer.RenderedTriangles);
  rasterizer.Free;
end;

end.

