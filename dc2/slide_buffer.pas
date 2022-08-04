(*******************************************************************************
slide_buffer.pas
Copyright (c) 2010-2015 David Pethes

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
unit slide_buffer;
{$mode objfpc}{$H+}

interface

uses
  common;

type
  { TSlidingBuffer }
  TSlidingBuffer = object
  private
      _buffer: pbyte;
      _previous_bytes_count: integer;
  public
      constructor Init();
      destructor Done;
      function GetWindow: pbyte; inline;
      procedure InsertData(const data: pbyte; const size: integer);
  end;


(*******************************************************************************
*******************************************************************************)
implementation

{ TSlidingBuffer }

constructor TSlidingBuffer.Init();
begin
  _buffer := getmem(2 * MAX_BLOCK_SIZE);
  _buffer += MAX_BLOCK_SIZE;
  _previous_bytes_count := 0;
end;

destructor TSlidingBuffer.Done;
begin
  freemem(_buffer - MAX_BLOCK_SIZE);
end;

function TSlidingBuffer.GetWindow: pbyte;
begin
  result := _buffer;
end;

procedure TSlidingBuffer.InsertData(const data: pbyte; const size: integer);
begin
  Assert(size <= MAX_BLOCK_SIZE, 'cannot insert more data than allocated range');

  if _previous_bytes_count > 0 then
      move((_buffer + _previous_bytes_count - MAX_BLOCK_SIZE)^,
           (_buffer - MAX_BLOCK_SIZE)^,
           MAX_BLOCK_SIZE);

  move(data^, _buffer^, size);
  _previous_bytes_count := size;
end;

end.

