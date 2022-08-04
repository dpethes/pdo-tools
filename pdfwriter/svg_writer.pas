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
unit svg_writer;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64,
  vector_writer;

type
  { TSvgStream }
  TSvgStream = class(TMemoryStream)
    public
      procedure WriteStr(const s: string);
      procedure WriteStrLine(const s: string);
  end;

  { TSvgWriter }
  TSvgWriter = class (TVectorWriter)
  private
    m_layer: TStrings;               //current drawing layer
    m_defs: TStrings;                //definitions (for clipping paths etc.)
    m_line_style: TVecLineStyle;     //current line style, used for <path>
    m_clip_path_id: integer;         //id for def and path pairing
    m_font_family: string;
    m_doclayers: array of TStrings;  //all drawing layers
    m_enable_clipping: boolean;
    m_currentDecimalSeparator: Char; //todo use TFormatSettings?
    m_doclayer_names: array of string;  //drawing layer names
    m_page_width,
    m_page_height: integer;
    m_coord_offset_x,
    m_coord_offset_y: Single;

    function WritePoints(const points: PSingle; const count: integer; closepath: boolean = true): string;
    procedure AddClipPath(const path: string);
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

const
  FontFamilyForShiftJIS = 'MS Gothic';

  //lsLine, lsOutline, lsTab, lsValleyFold, lsMountainFold, lsFace, lsText
  LineStyleDefs: array[TVecLineStyle] of string[60] = (
    'stroke:#000;fill:none;stroke-width:0.1',       //fill #008080
    'stroke:#000;fill:none;stroke-width:0.1',
    'stroke:#000;fill:#fff;stroke-width:0.1',
    'stroke:#808080;stroke-width:0.1;stroke-dasharray:1,1',
    'stroke:#808080;stroke-width:0.1',
    'stroke:#000;fill:#088;stroke-width:0.1',
    'stroke:none;fill:#000'
  );

{ TSvgStream }

procedure TSvgStream.WriteStr(const s: string);
begin
  WriteBuffer(s[1], Length(s));
end;

procedure TSvgStream.WriteStrLine(const s: string);
begin
  WriteStr(s);
  WriteStr(LineEnding);
end;

{ TSvgWriter }

function TSvgWriter.WritePoints(const points: PSingle; const count: integer; closepath: boolean = true): string;
var
  i: integer;
  x, y: double;
begin
  result := 'M ';
  for i := 0 to count - 1 do begin
      x := points[i * 2 + 0] - m_coord_offset_x;
      y := points[i * 2 + 1] - m_coord_offset_y;
      result += format('%.3f, %.3f ', [x, y]);
  end;
  if closepath then
      result += 'Z';
end;

procedure TSvgWriter.AddClipPath(const path: string);
var
  clip_id: string;
begin
  clip_id := 'clip' + IntToStr(m_clip_path_id);
  m_defs.Add('<clipPath clipPathUnits="userSpaceOnUse" id="' + clip_id + '">');
  m_defs.Add(path);
  m_defs.Add('</clipPath>');
end;

constructor TSvgWriter.Create;
begin
  m_defs := TStringList.Create;
  m_currentDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  m_line_style := lsLine;
  m_clip_path_id := 0;
  m_enable_clipping := false;
  m_font_family := 'Arial';
  m_coord_offset_x := 0;
  m_coord_offset_y := 0;
end;

destructor TSvgWriter.Destroy;
var
  i: integer;
begin
  DefaultFormatSettings.DecimalSeparator := m_currentDecimalSeparator;
  for i := 0 to Length(m_doclayers) - 1 do
      m_doclayers[i].Free;
  m_doclayers := nil;
  m_doclayer_names := nil;
  m_defs.Free;
  inherited Destroy;
end;

procedure TSvgWriter.InitDocProperties(const width_mm, height_mm: single; const jp_font: boolean);
begin
  if jp_font then
     m_font_family := FontFamilyForShiftJIS;
  m_page_width  := round(width_mm );
  m_page_height := round(height_mm);
end;

procedure TSvgWriter.AddPage;
begin
  //unused, we output only one-page svgs and no coord manipulation is being made
end;

procedure TSvgWriter.SaveToFile(const fname: string);
var
  f: TSvgStream;

  procedure WriteLayer(const layer: Tstrings; const name: string);
  var
    str: string;
  begin
    f.WriteStrLine('<g id="' + name + '" inkscape:groupmode="layer" inkscape:label="' + name + '">');
    for str in layer do
        f.WriteStrLine(str);
    f.WriteStrLine('</g>');
  end;

  procedure WriteDefs(const defs: TStrings);
  var
    str: string;
  begin
    f.WriteStrLine('<defs>');
    for str in defs do
        f.WriteStrLine(str);
    f.WriteStrLine('</defs>');
  end;

var
  page_size_definition: string;
  i: integer;
begin
  f := TSvgStream.Create;
  f.WriteStrLine('<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
  f.WriteStrLine('<!-- Created with PdoTools --> ');
  f.WriteStrLine('<svg xmlns:svg="http://www.w3.org/2000/svg" '
                   + 'xmlns="http://www.w3.org/2000/svg" '
                   + 'xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape" '  //for layers support
                   + 'xmlns:xlink="http://www.w3.org/1999/xlink" ' //xlink for linking images
                   + 'version="1.1" ');

  page_size_definition := Format('width="%dmm" height="%dmm" viewBox="0 0 %d %d"',
                          [m_page_width, m_page_height, m_page_width, m_page_height]);
  f.WriteStrLine(page_size_definition + '>');
  WriteDefs(m_defs);

  //todo write all layers
  for i := 0 to length(m_doclayers) - 1 do
      WriteLayer(m_doclayers[i], m_doclayer_names[i]);

  f.WriteStrLine('</svg>');
  f.SaveToFile(fname);
  f.Free;
end;

procedure TSvgWriter.SetLineWidth(const width: single);
begin

end;

procedure TSvgWriter.DrawLine(const x1, y1, x2, y2: single);
var
  buf: array[0..3] of single;
begin
  buf[0] := x1; buf[1] := y1;
  buf[2] := x2; buf[3] := y2;
  DrawLine(@buf);
end;

procedure TSvgWriter.DrawLine(const buffer: PSingle);
var
  points: string;
  path: string;
begin
  points := WritePoints(buffer, 2, false);
  path := Format(' <path d="%s" style="%s"/>', [points, LineStyleDefs[m_line_style]]);
  m_layer.Add(path);
end;

procedure TSvgWriter.SetDashedLine;
begin
  m_line_style := lsValleyFold;
end;

procedure TSvgWriter.SetSolidLine;
begin
  m_line_style := lsLine;  //mountainfold?
end;

procedure TSvgWriter.SetLineStyle(const line_style: TVecLineStyle);
begin
  m_line_style := line_style;
end;

procedure TSvgWriter.DrawPoly(const buffer: PSingle; const size: integer);
var
  points: string;
  path: string;
begin
  points := WritePoints(buffer, size);
  path := Format(' <path d="%s" style="%s"/>', [points, LineStyleDefs[m_line_style]]);
  m_layer.Add(path);
end;

procedure TSvgWriter.PushState;
begin
  m_enable_clipping := true;
end;

procedure TSvgWriter.PopState;
begin
  m_enable_clipping := false;
  m_clip_path_id += 1;
end;

procedure TSvgWriter.SetClippingPoly(const buffer: PSingle; const size: integer);
var
  points: string;
  path: string;
begin
  points := WritePoints(buffer, size);
  path := Format(' <path d="%s" style="%s"/>', [points, LineStyleDefs[m_line_style]]);
  AddClipPath(path);
end;

procedure TSvgWriter.SetClippingPolys(const polygons: array of TVectorPoly);
var
  poly: TVectorPoly;
  points: string;
  path: string;
begin
  points := '';
  for poly in polygons do
      points += WritePoints(poly.coords, poly.count);
  path := Format(' <path d="%s" style="%s"/>', [points, LineStyleDefs[m_line_style]]);
  AddClipPath(path);
end;

procedure TSvgWriter.DrawPngImage(const png: pbyte; const size: integer;
  const x, y, width, height: single);
var
  s: string;
  encoder: TBase64EncodingStream;
  stream: TStringStream;
  clip_id: string;
  px, py: Single;
begin
  //store as base64 string
  stream := TStringStream.Create('');
  encoder := TBase64EncodingStream.Create(stream);
  encoder.Write(png^, size);
  encoder.Flush;
  encoder.Free;

  px := x - m_coord_offset_x;
  py := y - height - m_coord_offset_y;
  s := format('<image x="%.3f" y="%.3f" width="%.3f" height="%.3f" preserveAspectRatio="none"',
             [px, py, width, height]);
  //s += ' id="texture_' + name + '"';
  if m_enable_clipping then begin
      clip_id := 'clip' + IntToStr(m_clip_path_id);
      s += ' clip-path="url(#' + clip_id + ')"';
  end;
  s += ' xlink:href="data:image/png;base64,' + stream.DataString + '"/>';
  m_layer.Add(s);
  stream.Free;
end;

procedure TSvgWriter.DrawImage(const prgba: pbyte; const img_w, img_h: integer;
  const x, y, width, height: single);
begin
  //support raw images, or use pngcompression callback?
end;

procedure TSvgWriter.SetCoordOffset(const x, y: single);
begin
  m_coord_offset_x := x;
  m_coord_offset_y := y;
end;

procedure TSvgWriter.Print(const text: string; const x, y: single; const font_size: integer);
var
  s, escaped_text: string;
  px, py: Single;
begin
  px := x - m_coord_offset_x;
  py := y - m_coord_offset_y;

  escaped_text := StringReplace(text, '<', '&lt;', [rfReplaceAll]);
  escaped_text := StringReplace(escaped_text, '>', '&gt;', [rfReplaceAll]);

  s := '<text';
  s += format(' x="%.3f" y="%.3f"', [px, py]);
  s += format(' style="font-size:%dpx;font-family:%s;%s">',
             [font_size, m_font_family, LineStyleDefs[TVecLineStyle.lsText]]);
  s += escaped_text + '</text>';
  m_layer.Add(s);
end;

function TSvgWriter.GenerateLayer(const name: string): TVecLayerId;
var
  max_id: integer;
begin
  max_id := Length(m_doclayers);
  SetLength(m_doclayers, max_id + 1);
  SetLength(m_doclayer_names, max_id + 1);

  m_doclayers[max_id] := TStringList.Create;
  m_doclayer_names[max_id] := name;
  result := max_id;
end;

procedure TSvgWriter.SetLayer(const id: TVecLayerId);
begin
  m_layer := m_doclayers[id];
end;

end.

