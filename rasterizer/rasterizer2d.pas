{
Basic triangle rasterizer with texture mapping

references:
http://forum.devmaster.net/t/advanced-rasterization/6145
  - basic setup / rasterization, subpixel precision, fill convention
http://fgiesen.wordpress.com/2013/02/17/optimizing-sw-occlusion-culling-index/
  - maths, optimalizations
http://www1.eonfusion.com/manual/index.php/Formulae_for_interpolation
  - vertex attribute interpolation
}
unit rasterizer2d;
{$mode objfpc}{$H+}

interface

const
  COLOR_SAMPLE_SIZE = 4;
  TEX_SAMPLE_SIZE = 3;

type
  TVertexf = record
      x, y: single;
      u, v: single;
  end;

  TTriangle = array[0..2] of TVertexf;
  PTriangle = ^TTriangle;

  TMapUV2f = record
      u, v: single;
  end;
  TTriangleUV = array[0..2] of TMapUV2f;

  TPixelRGBA = record
    case boolean of
      true: (r, g, b, a: byte);
      false: (rgba: longword);
  end;
  PPixelRGBA = ^TPixelRGBA;

  { TTexture2D
    Texture with 24bit RGB data
  }
  TTexture2D = class
    private
      m_data: pbyte;
      m_width, m_height: integer;
      m_stride: integer;
    public
      constructor Create(const data: pbyte; const width, height: integer);
  end;


  { TLinearAttributeSetup
    Setup interpolation constants for linearly varying vaues
  }
  TBilerpConsts = record a,b,c: single end;
  TLinearAttributeSetup = class
    private
      m_triangle: TTriangle;
      m_determinant: single;
    public
      function SetTriangle(const triangle: TTriangle): single;
      function GetInterpolationConsts(const w1, w2, w3: single): TBilerpConsts;
  end;

  TFragSamplingType = (fsConst, fsNearest, fsLinear);

  { TTexSampler }
  TTexSampler = class
    private
      texture: pbyte;
      width, height: integer;
      stride: integer;
      mask_w, mask_h: integer;
    public
      constructor Create;
      procedure SetTexture(const texture_: TTexture2D);
      function GetNearest(const u, v: single): TPixelRGBA;
      function GetLinear(const u, v: single): TPixelRGBA;
  end;

  { TPoly2dRasterizer }
  TPoly2dRasterizer = class
    private
      dst: pbyte;
      tev: TTexSampler;
      attribute_setup: TLinearAttributeSetup;

      dst_width, dst_height: integer;
      dst_stride: integer;
      m_sampleType: TFragSamplingType;
      solid_color: TPixelRGBA;
      pixels_per_triangle: integer;
      m_scale_unit_coords: boolean;

      procedure RasterizeTriangle(triangle: TTriangle);
      procedure ProcessFragments(const pdst: pbyte; const y, start_x: integer;
        const frag_count: integer; const attr_v, attr_u: TBilerpConsts);

    public
      property TrianglePixelCount: integer read pixels_per_triangle;

      constructor Create();
      destructor Destroy; override;
      procedure InitWindow(const width, height: integer);
      function GetColorBuffer(): pbyte;
      procedure Clear(const color: integer);
      procedure BindTexture(const texture: TTexture2D; const bilinear_sampling: boolean);
      procedure ReleaseTexture;
      procedure SetSolidColor(const color: TPixelRGBA);
      procedure EnableVertexCoordScaling(const enable: boolean);
      procedure DrawTriangle(triangle: TTriangle);
      procedure DrawTriangleBuffer(const triangle_buffer: PTriangle; const count: integer);
  end;

//**************************************************************************************************
implementation

uses
  math;

function min3(const a, b, c: integer): integer; inline;
begin
  result := min(a, min(b, c));
end;

function max3(const a, b, c: integer): integer; inline;
begin
  result := max(a, max(b, c));
end;

procedure swap_vertex(var a, b: TVertexf); inline;
var
  t: TVertexf;
begin
  t := a; a := b; b := t;
end;

{
  Make sure the triangle has counter-clockwise winding

  For a triangle A B C, you can find the winding by computing the cross product (B - A) x (C - A).
  For 2d tri's, with z=0, it will only have a z component.
  To give all the same winding, swap vertices C and B if this z component is negative.
}
function EnsureCcWinding(var triangle: TTriangle): boolean;
begin
  result := false;
  if (triangle[1].x - triangle[0].x) * (triangle[2].y - triangle[0].y)
      > (triangle[2].x - triangle[0].x) * (triangle[1].y - triangle[0].y)
  then begin
      swap_vertex(triangle[1], triangle[2]);
      result := true;
  end;
end;

{ TLinearAttributeSetup }

function TLinearAttributeSetup.SetTriangle(const triangle: TTriangle): single;
var
  x1, x2, x3: single;
  y1, y2, y3: single;
begin
  m_triangle := triangle;
  x1 := triangle[0].x;
  y1 := triangle[0].y;
  x2 := triangle[1].x;
  y2 := triangle[1].y;
  x3 := triangle[2].x;
  y3 := triangle[2].y;
  m_determinant := x1*y2-x2*y1+x2*y3-x3*y2+x3*y1-x1*y3;
  result := m_determinant;
end;

function TLinearAttributeSetup.GetInterpolationConsts(const w1, w2, w3: single): TBilerpConsts;
var
  x1, x2, x3: single;
  y1, y2, y3: single;
begin
  Assert(m_determinant <> 0);  //degenerate triangle, nothing to solve

  x1 := m_triangle[0].x;
  y1 := m_triangle[0].y;
  x2 := m_triangle[1].x;
  y2 := m_triangle[1].y;
  x3 := m_triangle[2].x;
  y3 := m_triangle[2].y;

  result.a := ((y2-y3)*w1 + (y3-y1)*w2 + (y1-y2)*w3) / m_determinant;
  result.b := ((x3-x2)*w1 + (x1-x3)*w2 + (x2-x1)*w3) / m_determinant;
  result.c := ((x2*y3-x3*y2)*w1 + (x3*y1-x1*y3)*w2 + (x1*y2-x2*y1)*w3) / m_determinant;
end;


{ TTexture2D }

constructor TTexture2D.Create(const data: pbyte; const width, height: integer);
begin
  m_data := data;
  m_width := width;
  m_height := height;
  m_stride := width * TEX_SAMPLE_SIZE;
end;


{ emulate GL_TEXTURE_WRAP_S/T: GL_REPEAT
}
function TexCoordWrapRepeat(const v: single): single; inline;
begin
  result := v;
  if v < 0 then
      result := 1 - trunc(v) + v
  else if v > 1 then
      result := v - trunc(v)
end;


{ TTexSampler }

constructor TTexSampler.Create;
begin
  texture := nil;
end;

procedure TTexSampler.SetTexture(const texture_: TTexture2D);
begin
  texture := texture_.m_data;
  width  := texture_.m_width;
  height := texture_.m_height;
  stride := texture_.m_stride;
  mask_w := width  - 1;
  mask_h := height - 1;
end;

function TTexSampler.GetNearest(const u, v: single): TPixelRGBA;
var
  tu, tv: single;
  x, y: integer;
begin
  tu := TexCoordWrapRepeat(u) * width ;
  tv := TexCoordWrapRepeat(v) * height;
  x := trunc( tu ) and mask_w;
  y := trunc( tv ) and mask_h;
  result := PPixelRGBA(texture + y * stride + x * TEX_SAMPLE_SIZE)^;
end; 


function TTexSampler.GetLinear(const u, v: single): TPixelRGBA;
var
  y0, x0, y1, x1: integer;
  tu0, tv0: single;
  fracx, fracy: integer;
  w1, w2, w3, w4: longword; // filtering weights
  p1, p2, p3, p4: TPixelRGBA; // pixel samples
  r, g, b: longword;
begin
  tu0 := TexCoordWrapRepeat(u) * width ;
  tv0 := TexCoordWrapRepeat(v) * height;
  x0 := trunc( tu0 );
  y0 := trunc( tv0 );
  fracx := trunc((tu0 - x0) * 256);
  fracy := trunc((tv0 - y0) * 256);

  x0 := x0 and mask_w;
  y0 := y0 and mask_h;
  x1 := (x0 + 1) and mask_w;
  y1 := (y0 + 1) and mask_h;

  fracx := trunc((tu0 - x0) * 256);
  fracy := trunc((tv0 - y0) * 256);

  p1 := PPixelRGBA(texture + y0 * stride + x0 * TEX_SAMPLE_SIZE)^;
  p2 := PPixelRGBA(texture + y0 * stride + x1 * TEX_SAMPLE_SIZE)^;
  p3 := PPixelRGBA(texture + y1 * stride + x0 * TEX_SAMPLE_SIZE)^;
  p4 := PPixelRGBA(texture + y1 * stride + x1 * TEX_SAMPLE_SIZE)^;

  w1 := (256-fracx) * (256-fracy);
  w2 := fracx       * (256-fracy);
  w3 := (256-fracx) * fracy;
  w4 := fracx       * fracy;

  result.rgba := 0;
  r := p1.r * w1 + p2.r * w2 + p3.r * w3 + p4.r * w4;
  g := p1.g * w1 + p2.g * w2 + p3.g * w3 + p4.g * w4;
  b := p1.b * w1 + p2.b * w2 + p3.b * w3 + p4.b * w4;
  result.r := r >> 16;
  result.g := g >> 16;
  result.b := b >> 16;
end;


{ TPoly2dRasterizer }

constructor TPoly2dRasterizer.Create;
begin
  dst := nil;
  tev := TTexSampler.Create;
  m_sampleType := fsConst;
  attribute_setup := TLinearAttributeSetup.Create;
end;

destructor TPoly2dRasterizer.Destroy;
begin
  inherited Destroy;
  attribute_setup.Free;
  tev.Free;
  if dst <> nil then
      freemem(dst);
end;

{ Initialize color&depth buffer, limit resolution to 2048x2048
}
procedure TPoly2dRasterizer.InitWindow(const width, height: integer);
begin
  dst_width  := min(width, 2048);
  dst_height := min(height, 2048);
  dst_stride := dst_width * COLOR_SAMPLE_SIZE;
  dst := getmem(dst_height * dst_stride);
end;

procedure TPoly2dRasterizer.Clear(const color: integer);
begin
  FillByte(dst^, dst_stride * dst_height, color and $ff);
end;


procedure TPoly2dRasterizer.BindTexture(const texture: TTexture2D; const bilinear_sampling: boolean);
begin
  tev.SetTexture(texture);
  if bilinear_sampling then
      m_sampleType := fsLinear
  else
      m_sampleType := fsNearest;
end;

procedure TPoly2dRasterizer.ReleaseTexture;
begin
  m_sampleType := fsConst;
end;

procedure TPoly2dRasterizer.SetSolidColor(const color: TPixelRGBA);
begin
  solid_color := color;
  m_sampleType := fsConst;
end;

procedure TPoly2dRasterizer.EnableVertexCoordScaling(const enable: boolean);
begin
  m_scale_unit_coords := enable;
end;


function TPoly2dRasterizer.GetColorBuffer: pbyte;
begin
  result := dst;
end;


procedure TPoly2dRasterizer.DrawTriangle(triangle: TTriangle);
var
  offset_u, offset_v: single;
  i: Integer;
begin
  //triangle must have counter-clockwise winding
  EnsureCcWinding(triangle);

  //scale vertices to pixel grid
  if m_scale_unit_coords then begin
      triangle[0].x *= dst_width;
      triangle[0].y *= dst_height;
      triangle[1].x *= dst_width;
      triangle[1].y *= dst_height;
      triangle[2].x *= dst_width;
      triangle[2].y *= dst_height;
  end;

  //offset texel centers
  if m_sampleType in [fsNearest, fsLinear] then begin
      offset_u := 1/(2*tev.width);
      offset_v := 1/(2*tev.height);
      for i := 0 to 2 do begin
          triangle[i].u -= offset_u;
          triangle[i].v -= offset_v;
      end;
  end;

  RasterizeTriangle(triangle);
end;

procedure TPoly2dRasterizer.DrawTriangleBuffer(const triangle_buffer: PTriangle; const count: integer);
var
  i: integer;
  pix_per_batch: integer;
begin
  pix_per_batch := 0;
  for i := 0 to count - 1 do begin
      DrawTriangle(triangle_buffer[i]);
      pix_per_batch += pixels_per_triangle;
  end;
  pixels_per_triangle := pix_per_batch;
end;


procedure TPoly2dRasterizer.ProcessFragments(const pdst: pbyte; const y, start_x: integer;
  const frag_count: integer; const attr_v, attr_u: TBilerpConsts);
var
  sampleType: TFragSamplingType;
  sample: TPixelRGBA;
  x: integer;
  buffer: plongword;  //32bit color buffer
  v, u: single;
begin
  buffer := plongword(pdst + start_x * COLOR_SAMPLE_SIZE);
  u := attr_u.a * start_x + attr_u.b * y + attr_u.c;
  v := attr_v.a * start_x + attr_v.b * y + attr_v.c;
  sampleType := m_sampleType;
  sample.rgba := 0;

  for x := 0 to frag_count - 1 do begin
      case sampleType of
        fsConst: sample := solid_color;
        fsNearest: sample := tev.GetNearest(u, v);
        fsLinear: sample := tev.GetLinear(u, v);
      end;
      u += attr_u.a;
      v += attr_v.a;
      buffer^ := sample.rgba;
      buffer += 1;
  end;
end;


procedure TPoly2dRasterizer.RasterizeTriangle(triangle: TTriangle);
var
  DX12, DX23, DX31, DY12, DY23, DY31: integer;
  cy1, cy2, cy3: integer;
  cx1, cx2, cx3: integer;
  minx, maxx,
  miny, maxy: integer;
  x, y: integer;
  row_last_x: integer;
  row_frag_count: integer;
  pdst: pbyte;
  determinant: single;
  attr_u, attr_v: TBilerpConsts;

  procedure RasterizationSetup;
  var
    x1, x2, x3: integer;
    y1, y2, y3: integer;
    c1, c2, c3: integer;
  begin
    // 28.4 fixed-point coordinates
    x1 := round(triangle[0].x * 16);
    y1 := round(triangle[0].y * 16);
    x2 := round(triangle[1].x * 16);
    y2 := round(triangle[1].y * 16);
    x3 := round(triangle[2].x * 16);
    y3 := round(triangle[2].y * 16);

    // Deltas
    DX12 := X1 - X2;
    DX23 := X2 - X3;
    DX31 := X3 - X1;
    DY12 := Y1 - Y2;
    DY23 := Y2 - Y3;
    DY31 := Y3 - Y1;

    // Bounding rectangle
    minx := (min3(x1, x2, x3) + 15) shr 4;
    maxx := (max3(x1, x2, x3) + 15) shr 4;
    miny := (min3(y1, y2, y3) + 15) shr 4;
    maxy := (max3(y1, y2, y3) + 15) shr 4;
    minx := max(0, minx);
    maxx := min(dst_width, maxx);
    miny := max(0, miny);
    maxy := min(dst_height, maxy);

    // Half-edge constants
    C1 := SarLongint( DY12 * X1 - DX12 * Y1, 4 );
    C2 := SarLongint( DY23 * X2 - DX23 * Y2, 4 );
    C3 := SarLongint( DY31 * X3 - DX31 * Y3, 4 );

    // Correct for fill convention
    if (DY12 < 0) or ((DY12 = 0) and (DX12 > 0)) then C1 += 1;
    if (DY23 < 0) or ((DY23 = 0) and (DX23 > 0)) then C2 += 1;
    if (DY31 < 0) or ((DY31 = 0) and (DX31 > 0)) then C3 += 1;

    CY1 := C1 + DX12 * miny - DY12 * minx;
    CY2 := C2 + DX23 * miny - DY23 * minx;
    CY3 := C3 + DX31 * miny - DY31 * minx;
  end;

begin
  pixels_per_triangle := 0;

  //setup pixel rasterization
  RasterizationSetup;

  //setup interpolation
  determinant := attribute_setup.SetTriangle(triangle);
  if determinant = 0 then
      exit;
  attr_u := attribute_setup.GetInterpolationConsts(triangle[0].u, triangle[1].u, triangle[2].u);
  attr_v := attribute_setup.GetInterpolationConsts(triangle[0].v, triangle[1].v, triangle[2].v);

  // Scan through bounding rectangle
  pdst := dst + miny * dst_stride;
  for y := miny to maxy - 1 do
  begin
      CX1 := CY1;
      CX2 := CY2;
      CX3 := CY3;
      row_frag_count := 0;
      row_last_x := 0;
      for x := minx to maxx - 1 do begin
          // When all half-space functions positive, pixel is in triangle
          // test only sign bits
          if (CX1 or CX2 or CX3) > 0 then begin
              row_frag_count += 1;
              row_last_x := x;
          end;
          CX1 -= DY12;
          CX2 -= DY23;
          CX3 -= DY31;
      end;
      CY1 += DX12;
      CY2 += DX23;
      CY3 += DX31;

      pixels_per_triangle += row_frag_count;
      ProcessFragments(pdst, y, row_last_x + 1 - row_frag_count, row_frag_count, attr_v, attr_u);

      pdst += dst_stride;
  end;
end;


end.

