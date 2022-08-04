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
unit vector_writer;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TVecLayerId = integer;

  TVectorPoly = record
      coords: psingle;
      count: integer;  //number of x,y pairs in coords buffer
  end;

  TVecLineStyle = (lsLine, lsOutline, lsTab, lsValleyFold, lsMountainFold, lsFace, lsText);

  { TVectorWriter
    Abstract vector data writer
  }
  TVectorWriter = class
  public
    procedure InitDocProperties(const width_mm, height_mm: single; const jp_font: boolean = false); virtual; abstract;
    procedure AddPage(); virtual; abstract;
    procedure SaveToFile(const fname: string); virtual; abstract;
    procedure SetLineWidth(const width: single); virtual; abstract;
    procedure DrawLine(const x1, y1, x2, y2: single); virtual; abstract;
    procedure DrawLine(const buffer: PSingle); virtual; abstract;
    procedure SetDashedLine; virtual; abstract;
    procedure SetSolidLine; virtual; abstract;
    procedure SetLineStyle(const line_style: TVecLineStyle); virtual; abstract;
    procedure DrawPoly(const buffer: PSingle; const size: integer); virtual; abstract;
    procedure PushState; virtual; abstract;
    procedure PopState; virtual; abstract;
    procedure SetClippingPoly(const buffer: PSingle; const size: integer); virtual; abstract;
    procedure SetClippingPolys(const polygons: array of TVectorPoly); virtual; abstract;
    procedure DrawPngImage(const png: pbyte; const size: integer; const x, y, width, height: single); virtual; abstract;
    procedure DrawImage(const prgba: pbyte; const img_w, img_h: integer; const x, y, width, height: single); virtual; abstract;
    procedure SetCoordOffset(const x, y: single); virtual; abstract;
    procedure Print(const text: string; const x, y: single; const font_size: integer = 1); virtual; abstract;
    function GenerateLayer(const name: string): TVecLayerId; virtual; abstract;
    procedure SetLayer(const id: TVecLayerId); virtual; abstract;
  end;

implementation

end.

