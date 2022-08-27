unit bitstream;
{$mode objfpc}
{$define bs_inline}

interface

type

TBitstreamBufferState = record
    current_bits: longword;
    mask: longword;
end;

{ TBitstreamWriter }

TBitstreamWriter = class
  private
    cur: plongword;
    mask: longword;
    buffer: plongword;
    closed: boolean;

  public
    constructor Create(const memory_buffer: pbyte);
    destructor Destroy; override;
    procedure Reset;

    procedure Close;
    function IsByteAligned: boolean;
    procedure ByteAlign;
    function GetBitSize: longword;
    function GetByteSize: longword;
    function GetUnbufferedByteSize: longword;
    function GetDataStart: pbyte;

    procedure Write(const bit: integer);
    procedure WriteSafe(const bits, length: longword);
    procedure Write(const bits, length: longword);  //write multiple bits, lsb first

    function GetState: TBitstreamBufferState;
    procedure SetState(const state: TBitstreamBufferState);

    procedure ResetBufferPosition;
end;


{ TBitstreamReader }

TBitstreamReader = class
  private
    bits: uint32;
    used: longword;
    cur: pbyte;
    buffer: pbyte;

  public
    constructor Create(const memory_buffer: pbyte);
    function  GetPosition(): longword;
    function  GetBitCount(): longword;
    function  GetUncachedPosition(): longword;
    function  IsByteAligned(): boolean;
    procedure Start();
    function  Read(): longword;
    function  Read(count: longword): longword;

    //U functions need explicit Refill calls
    procedure Refill;
    function  ReadU: longword;                   {$ifdef bs_inline}inline;{$endif}
    function  ReadU(count: longword): longword;  {$ifdef bs_inline}inline;{$endif}
    function  ShowU(const count: longword): longword; {$ifdef bs_inline}inline;{$endif}
    function  Show9U: longword;                  {$ifdef bs_inline}inline;{$endif}
    procedure SkipU(const count: longword);      {$ifdef bs_inline}inline;{$endif}

    function  ReadInverse(bit_count: longword): longword;

    function  GetState: TBitstreamBufferState;
    procedure SetState(const state: TBitstreamBufferState);
    procedure ResetBufferPosition;

    function GetInternalState: TBitstreamBufferState;
    procedure SetInternalState(const state: TBitstreamBufferState);
end;

function SwapBits (const bits, bit_count: longword): longword;


(*******************************************************************************
*******************************************************************************)
implementation

{ SwapBits
  Swap bit ordering in source pattern. Swaps up to 16 bits.
}
function SwapBits (const bits, bit_count: longword): longword;
var
  x: longword;
begin
  x := bits;
  x := ((x and $aaaa) >> 1) or ((x and $5555) << 1);
  x := ((x and $cccc) >> 2) or ((x and $3333) << 2);
  x := ((x and $f0f0) >> 4) or ((x and $0f0f) << 4);
  x := ((x and $ff00) >> 8) or ((x and $00ff) << 8);
  result := x >> (16 - bit_count);
end;

{ TBitstreamWriter }

constructor TBitstreamWriter.Create(const memory_buffer: pbyte);
begin
  buffer := plongword (memory_buffer);
  Reset;
end;

procedure TBitstreamWriter.Reset;
begin
  cur  := buffer;
  cur^ := 0;
  mask := 0;
end;

destructor TBitstreamWriter.Destroy;
begin
  if not closed then
      Close;

  inherited Destroy;
end;

function TBitstreamWriter.GetBitSize: longword;
begin
  result := 32 * (cur - buffer) + mask;
end;

function TBitstreamWriter.GetByteSize: longword;
begin
  result := (cur - buffer) * 4;
  result += (mask + 7) div 8;  //+ buffer
end;

function TBitstreamWriter.GetUnbufferedByteSize: longword;
begin
  result := (cur - buffer) * 4;
end;

function TBitstreamWriter.GetDataStart: pbyte;
begin
  result := pbyte(buffer);
end;

procedure TBitstreamWriter.Close;
begin
end;

function TBitstreamWriter.IsByteAligned: boolean;
begin
  result := mask mod 8 = 0;
end;

procedure TBitstreamWriter.ByteAlign;
begin
  while not IsByteAligned do
      Write(0);
end;

procedure TBitstreamWriter.Write(const bit: integer);
begin
  cur^ := cur^ or longword((bit and 1) shl mask);
  mask += 1;

  if mask = 32 then begin
      cur += 1;
      cur^ := 0;
      mask := 0;
  end;
end;

procedure TBitstreamWriter.WriteSafe(const bits, length: longword);
var
  bits_: longword;
begin
  Assert(length <= 32, 'bit_count over 32');

  bits_ := bits and ($ffffffff shr (32 - length));  //clear unused bits
  cur^ := cur^ or (bits_ shl mask);
  mask += length;
  if mask >= 32 then begin
      mask -= 32;  //number of bits that didn't fit into buffer
      cur += 1;
      cur^ := 0;

      if mask > 0 then
          cur^ := bits_ shr (length - mask);
  end;
end;

//assumes that unused bits not covered by length are not set
procedure TBitstreamWriter.Write(const bits, length: longword);
begin
  Assert(length <= 32, 'bit_count over 32');

  cur^ := cur^ or (bits shl mask);
  mask += length;
  if mask >= 32 then begin
      mask -= 32;
      cur += 1;
      cur^ := 0;

      if mask > 0 then
          cur^ := bits shr (length - mask);
  end;
end;

function TBitstreamWriter.GetState: TBitstreamBufferState;
begin
  Result.mask := mask;
  Result.current_bits := cur^;
end;

procedure TBitstreamWriter.SetState(const state: TBitstreamBufferState);
begin
  mask := state.mask;
  cur^ := state.current_bits;
end;

procedure TBitstreamWriter.ResetBufferPosition;
var
  cache: TBitstreamBufferState;
begin
  cache := GetState;
  cur := buffer;
  SetState(cache);
end;


{ TBitstreamReader }

constructor TBitstreamReader.Create(const memory_buffer: pbyte);
begin
  buffer := memory_buffer;
  cur  := buffer;
  used := 0;
  bits := plongword(cur)^;
end;

function TBitstreamReader.GetPosition: longword;
begin
  result := cur - buffer;  //used bytes
  result += (used + 7) shr 3;  //+ buffer
end;

function TBitstreamReader.GetBitCount: longword;
begin
  result := 8 * (cur - buffer) + used;
end;

function TBitstreamReader.GetUncachedPosition: longword;
begin
  result := cur - buffer;
end;

function TBitstreamReader.IsByteAligned: boolean;
begin
  result := true;
  if used mod 8 > 0 then result := false;
end;

procedure TBitstreamReader.Start;
begin
  bits := plongword(cur)^;
end;

function TBitstreamReader.Read: longword;
begin
  result := (bits shr used) and 1;
  used += 1;
  if used = 32 then begin
      cur += 4;
      bits := plongword(cur)^;
      used := 0;
  end;
end;

function TBitstreamReader.Read(count: longword): longword;
var
  bits_left: integer;
begin
  result := bits shr used;
  if count < (32 - used) then begin
      result := result and ($ffffffff shr (32 - count));
      used += count;
  end else begin
      bits_left := count - (32 - used);
      cur += 4;
      bits := plongword(cur)^;
      if bits_left > 0 then
          result := result or (bits and ($ffffffff shr (32 - bits_left))) shl (32 - used);
      used := bits_left;
  end;
end;

procedure TBitstreamReader.Refill;
var
  fill_bits: integer;
begin
  if used < 8 then
      exit;
  fill_bits := used and (not %111);
  used -= fill_bits;
  bits := (bits >> fill_bits) or (plongword(cur + 4)^ << (32 - fill_bits));
  cur += fill_bits >> 3;
end;

function TBitstreamReader.ReadU: longword;
begin
  result := (bits shr used) and 1;
  used += 1;
end;

function TBitstreamReader.ReadU(count: longword): longword;
begin
  result := bits shr used;
  result := result and ($ffffffff shr (32 - count));
  used += count;
end;

function TBitstreamReader.ShowU(const count: longword): longword;
begin
  result := bits shr used;
  result := result and ($ffffffff shr (32 - count));
end;

function TBitstreamReader.Show9U: longword;
begin
  result := bits shr used;
  result := result and ($ffffffff shr 23)
end;

procedure TBitstreamReader.SkipU(const count: longword);
begin
  used += count;
end;

function TBitstreamReader.ReadInverse(bit_count: longword): longword;
var
  i: integer;
begin
  result := 0;
  for i := bit_count - 1 downto 0 do
      result := result or Read() shl i;
end;

function TBitstreamReader.GetState: TBitstreamBufferState;
begin
  result.current_bits := bits;
end;

procedure TBitstreamReader.SetState(const state: TBitstreamBufferState);
begin
  bits := state.current_bits;
end;

procedure TBitstreamReader.ResetBufferPosition;
begin
  cur := buffer;
end;

function TBitstreamReader.GetInternalState: TBitstreamBufferState;
begin
  result.current_bits := cur - buffer;
  result.mask := used;
end;

procedure TBitstreamReader.SetInternalState(const state: TBitstreamBufferState);
begin
  cur := buffer + state.current_bits;
  used := state.mask;
end;


end.

(*******************************************************************************
bitstream.pas
Copyright (c) 2007-2018 David Pethes

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
