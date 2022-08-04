unit gzip_format;
{$mode objfpc}{$H+}

interface

uses
  sysutils;

procedure gzf_write_header(var f: file; const name: string);
procedure gzf_write_end(var f: file; const original_size, crc32: longword);

{
  Reads the base gzip header without encoded file name and checks if the ID is correct
}
function gzf_read_header(var f: file): boolean;

{
  Reads the stored filename
}
function gzf_read_filename(var f: file): string;

function gzf_read_crc(var f: file): longword;


implementation

const
  GZIP_ID: array[0..1] of byte = ($1f, $8b);

procedure gzf_write_header(var f: file; const name: string);
const
  CM: byte = 8;
  FNAME: byte = %1000;
  MTIME: longword = 0; //todo replace with real time
  XFL: byte = 0;
  OS:  byte = 0;
begin
  blockwrite(f, GZIP_ID, 2); //GZIP_ID
  blockwrite(f, CM, 1);      //cm - cmethod
  blockwrite(f, FNAME, 1);   //flag FNAME
  blockwrite(f, MTIME, 4);   //MTIME (Modification TIME)
  blockwrite(f, XFL, 1);     //XFL (eXtra FLags)
  blockwrite(f, OS, 1);      //OS (Operating System)
  //name zero terminated
  blockwrite(f, pchar(name)^, length(name));
  blockwrite(f, OS, 1);
end;


procedure gzf_write_end(var f: file; const original_size, crc32: longword);
begin
  blockwrite(f, crc32, 4);
  blockwrite(f, original_size, 4);
end;


function gzf_read_header(var f: file): boolean;
var
  id: array[0..1] of byte;
  buf: longword;
begin
  result := false;

  blockread(f, id[0], 1);
  blockread(f, id[1], 1);
  if (id[0] <> GZIP_ID[0]) or (id[1] <> GZIP_ID[1]) then
      exit;

  blockread(f, buf, 1);  //cm - cmethod
  blockread(f, buf, 1);  //flag FNAME
  blockread(f, buf, 4);  //MTIME (Modification TIME)
  blockread(f, buf, 1);  //XFL (eXtra FLags)
  blockread(f, buf, 1);  //OS (Operating System)

  result := true;
end;


function gzf_read_filename(var f: file): string;
var
  i: byte = 0;
begin
  result := '';
  blockread(f, i, 1);
  repeat
      result += char(i);
      blockread(f, i, 1);
  until i = 0;
end;

function gzf_read_crc(var f: file): longword;
begin
  result := 0;
  blockread(f, result, 4);
end;


end.

(*******************************************************************************
gzip_format.pas
Copyright (c) 2009-2018 David Pethes

This file is part of Dc2.

Dc2 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Dc2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Dc2.  If not, see <http://www.gnu.org/licenses/>.

*******************************************************************************)
