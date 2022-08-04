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
unit pdf_writer;
{$mode objfpc}{$H+}
{define HAS_FONTS}

interface

uses
  Classes, SysUtils,
  vector_writer,
  hpdf, hpdf_types, hpdf_consts;

type
  { TPdfWriter }
  TPdfWriter = class (TVectorWriter)
  private
    _pdf: HPDF_Doc;
    _page: HPDF_Page;  //current page
    _font: HPDF_Font;  //current font
    _coord_offset_x,
    _coord_offset_y: single;
    _vbuf: psingle;
    _page_width,
    _page_height: single;
    procedure AdjustCoords(const buffer: PSingle; const items: integer);
    procedure BufferCoords(const buffer: PSingle; const size: integer);
  public
    constructor Create();
    destructor Destroy; override;
    procedure InitDocProperties(const width_mm, height_mm: single; const jp_font: boolean = false); override;
    procedure AddPage(); override;
    procedure SaveToFile(const fname: string); override;
    procedure SetLineWidth(const width: single); override;
    procedure DrawLine(const x1, y1, x2, y2: single); override;
    procedure DrawLine(const buffer: PSingle); override;
    procedure SetDashedLine; override;
    procedure SetSolidLine; override;
    procedure SetLineStyle(const line_style: TVecLineStyle); override;
    //draw polygon
    //buffer: xy pairs
    //size: number of xy pairs
    procedure DrawPoly(const buffer: PSingle; const size: integer); override;
    procedure PushState; override;
    procedure PopState; override;
    procedure SetClippingPoly(const buffer: PSingle; const size: integer); override;
    procedure SetClippingPolys(const polygons: array of TVectorPoly); override;
    procedure DrawPngImage(const png: pbyte; const size: integer; const x, y, width, height: single); override;
    procedure DrawImage(const prgba: pbyte; const img_w, img_h: integer; const x, y, width, height: single); override;
    procedure SetCoordOffset(const x, y: single); override;
    procedure Print(const text: string; const x, y: single; const font_size: integer = 1); override;
    function GenerateLayer(const name: string): TVecLayerId; override;
    procedure SetLayer(const id: TVecLayerId); override;
  end;

//**************************************************************************************************
implementation

procedure ErrorHandler (error_no: HPDF_STATUS; detail_no: HPDF_STATUS; user_data: Pointer); stdcall;
var
  message: string;
begin
  message := format('libHaru error: error_no=%x ', [error_no, detail_no]);
  if error_no = $1051 then message += 'invalid gmode';
  if error_no = $1017 then message += 'cannot open file for writing';
  if error_no = $1028 then message += 'Combination between font and encoder is wrong';
  if error_no = $102B then message += 'wrong encoding';
  if error_no = $102F then message += 'Font with the specified name is not found';
  raise Exception.Create(message);
end;

{ TPdfWriter }

constructor TPdfWriter.Create;
begin
  _pdf := HPDF_New(@ErrorHandler, nil);
  if _pdf = nil then
      raise Exception.Create('error: cannot create PdfDoc object');
  _coord_offset_x := 0;
  _coord_offset_y := 0;
  _vbuf := getmem(1 shl 20);
  _page := nil;
end;

destructor TPdfWriter.Destroy;
begin
  inherited Destroy;
  HPDF_Free(_pdf);
  freemem(_vbuf);
end;

procedure TPdfWriter.InitDocProperties(const width_mm, height_mm: single; const jp_font: boolean);
const
  SizeToPixels = 2.8346;

  //setup external fonts: UTF8 + TTF, plus any font that has japanese symbols if needed
  procedure LoadExternalFonts;
  var
    font_name: string;
  begin
    HPDF_UseUTFEncodings(_pdf);
    HPDF_SetCurrentEncoder(_pdf, 'UTF-8');
    font_name := HPDF_LoadTTFontFromFile(_pdf, 'c:\windows\fonts\arial.ttf', HPDF_TRUE);
    if jp_font then
        font_name := HPDF_LoadTTFontFromFile2(_pdf, 'c:\windows\fonts\msgothic.ttc', 0, HPDF_TRUE);
    _font := HPDF_GetFont (_pdf, pchar(font_name), 'UTF-8');
  end;

begin
  HPDF_SetCompressionMode (_pdf, HPDF_COMP_ALL);
  //font: standard pdf font + fancier external fonts if available
  _font := HPDF_GetFont (_pdf, 'Helvetica', nil);
{$ifdef HAS_FONTS}
   LoadExternalFonts;
{$endif}
  _page_width  :=  width_mm * SizeToPixels;
  _page_height := height_mm * SizeToPixels;
end;

procedure TPdfWriter.AddPage;
const
  MmPerInch = 25.4;
begin
  _page := HPDF_AddPage(_pdf);
  HPDF_Page_SetLineJoin(_page, HPDF_BEVEL_JOIN);
  //set inch to mm transformation matrix
  HPDF_Page_Concat (_page,
                   72.0 / MmPerInch, 0,
                   0, 72.0 / MmPerInch,
                   0, 0);
  HPDF_Page_SetWidth (_page, _page_width);
  HPDF_Page_SetHeight(_page, _page_height);
  HPDF_Page_SetFontAndSize (_page, _font, 5);
end;

procedure TPdfWriter.SaveToFile(const fname: string);
begin
  if _page = nil then
      HPDF_AddPage(_pdf);
  HPDF_SaveToFile(_pdf, Pchar(fname));
end;

procedure TPdfWriter.SetLineWidth(const width: single);
begin
  HPDF_Page_SetLineWidth(_page, width);
end;

procedure TPdfWriter.SetCoordOffset(const x, y: single);
begin
  _coord_offset_x := x;
  _coord_offset_y := y;
end;

procedure TPdfWriter.AdjustCoords(const buffer: PSingle; const items: integer);
var
  i: Integer;
  y, hadj: single;
begin
  hadj := HPDF_Page_GetHeight(_page) / 2.8346;
  for i := 0 to items - 1 do begin
      buffer[i*2] -= _coord_offset_x;
      y := hadj - (buffer[i*2+1] - _coord_offset_y);
      buffer[i*2+1] := y;
  end;
end;

procedure TPdfWriter.BufferCoords(const buffer: PSingle; const size: integer);
begin
  move(buffer^, _vbuf^, size * 2 * 4);
  AdjustCoords(_vbuf, size);
end;

procedure TPdfWriter.DrawLine(const x1, y1, x2, y2: single);
var
  buf: array[0..3] of single;
begin
  buf[0] := x1;
  buf[1] := y1;
  buf[2] := x2;
  buf[3] := y2;
  DrawLine(@buf);
end;

procedure TPdfWriter.DrawLine(const buffer: PSingle);
begin
  BufferCoords(buffer, 2);
  HPDF_Page_MoveTo(_page, _vbuf[0], _vbuf[1]);
  HPDF_Page_LineTo(_page, _vbuf[2], _vbuf[3]);
  HPDF_Page_Stroke(_page);
end;

procedure TPdfWriter.SetDashedLine;
const
  dash_ptn: array[0..0] of word = (1);
begin
  HPDF_Page_SetDash(_page, @dash_ptn, 1, 0);
end;

procedure TPdfWriter.SetSolidLine;
begin
  HPDF_Page_SetDash(_page, nil, 0, 0);
end;

procedure TPdfWriter.SetLineStyle(const line_style: TVecLineStyle);
begin
  //nothing, used only in SVG so far
end;

procedure TPdfWriter.DrawPoly(const buffer: PSingle; const size: integer);
var
  i: Integer;
begin
  BufferCoords(buffer, size);
  HPDF_Page_MoveTo(_page, _vbuf[0], _vbuf[1]);
  for i := 1 to size - 1 do begin
      HPDF_Page_LineTo(_page, _vbuf[i*2], _vbuf[i*2+1]);
  end;
  HPDF_Page_ClosePathStroke(_page);
end;

procedure TPdfWriter.PushState;
begin
  HPDF_Page_GSave(_page);
end;

procedure TPdfWriter.PopState;
begin
  HPDF_Page_GRestore(_page);
end;

procedure TPdfWriter.SetClippingPoly(const buffer: PSingle; const size: integer);
var
  i: Integer;
begin
  BufferCoords(buffer, size);
  HPDF_Page_MoveTo(_page, _vbuf[0], _vbuf[1]);
  for i := 1 to size - 1 do begin
      HPDF_Page_LineTo(_page, _vbuf[i*2], _vbuf[i*2+1]);
  end;
  HPDF_Page_Clip(_page);
  HPDF_Page_EndPath(_page);
end;

procedure TPdfWriter.SetClippingPolys(const polygons: array of TVectorPoly);
var
  poly: TVectorPoly;
  i: Integer;
begin
  for poly in polygons do begin
      BufferCoords(poly.coords, poly.count);
      HPDF_Page_MoveTo(_page, _vbuf[0], _vbuf[1]);
      for i := 1 to poly.count - 1 do begin
          HPDF_Page_LineTo(_page, _vbuf[i*2], _vbuf[i*2+1]);
      end;
  end;
  HPDF_Page_Clip(_page);
  HPDF_Page_EndPath(_page);
end;

procedure TPdfWriter.DrawPngImage
  (const png: pbyte; const size: integer; const x, y, width, height: single);
var
  image: HPDF_Image;
  buf: array[0..1] of single;
begin
  buf[0] := x;
  buf[1] := y;
  BufferCoords(@buf, 1);
  image := HPDF_LoadPngImageFromMem(_pdf, png, size);
  HPDF_Page_DrawImage(_page, image, _vbuf[0], _vbuf[1], width, height);
end;

procedure TPdfWriter.DrawImage(const prgba: pbyte; const img_w, img_h: integer;
  const x, y, width, height: single);
var
  image: HPDF_Image;
  buf: array[0..1] of single;
begin
  buf[0] := x;
  buf[1] := y;
  BufferCoords(@buf, 1);
  image := HPDF_LoadRawImageFromMem(_pdf, prgba, img_w, img_h, HPDF_CS_DEVICE_RGB, 8);
  HPDF_Page_DrawImage(_page, image, _vbuf[0], _vbuf[1], width, height);
end;

procedure TPdfWriter.Print(const text: string; const x, y: single; const font_size: integer);
var
  buf: array[0..1] of single;
begin
  buf[0] := x;
  buf[1] := y;
  BufferCoords(@buf, 1);
  HPDF_Page_BeginText(_page);
  HPDF_Page_TextOut(_page, _vbuf[0], _vbuf[1], pchar(text));
  HPDF_Page_EndText(_page);
end;


function TPdfWriter.GenerateLayer(const name: string): TVecLayerId;
begin
  result := 0;
end;

procedure TPdfWriter.SetLayer(const id: TVecLayerId);
begin
  //pdf is not layered
end;

end.

