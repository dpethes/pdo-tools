unit vlc;
{$mode objfpc}

interface

uses
  bitstream, huffcoding;

const
  END_OF_BLOCK = 1000;  //must not collide with any valid Deflate values
  END_OF_BLOCK_CODE = 256;
  MAX_VLC_LENGTH = 6;  //maximum code+length vlc in bytes: 15+5 bits match with 15+13 bits offset

type

  { TVlcWriter }
  TVlcWriter = object
  private
    bs: TBitstreamWriter;
    len_tree, dist_tree: PVlcCode;
  public
    procedure SetTrees(const bitstream: TBitstreamWriter; const length_tree, distance_tree: PVlcCode);
    procedure WriteMatch (const len, dist: longword);
    procedure WriteLiteral(const c: integer); inline;
    procedure WriteBlockEnd ();
  end;

  TSymbolBits = record
      symbol: word;
      nbits: byte;
  end;

const
  TAB0_BITS = 9;  //LUT bits, must be less or equal to maximum bit length of huff codes allowed by Deflate

type
  TDecodeLookupTables = record
      codes_t0: array[0..511] of TSymbolBits;  //9 bits = 512 values
      canon_table: TDecodeTable;
  end;

  { TVlcReader }

  TVlcReader = class
  private
    bs: TBitstreamReader;
    literal_dectable, distance_dectable: TDecodeLookupTables;
  public
    procedure SetTables(const bitreader: TBitstreamReader; const literal_table, distance_table: TDecodeLookupTables);
    function ReadCode: integer; inline;
    procedure ReadDist(code: integer; out length, distance: word);
  end;


function Length2code (const len:  longword): longword;
function Distance2code(const dist: integer): longword;

function vlc_ReadCode(const bs: TBitstreamReader; const dectable: TDecodeLookupTables): integer;
function InitDecodeLut(const code_lengths: pbyte; const count: integer): TDecodeLookupTables;

(*******************************************************************************
*******************************************************************************)
implementation

{ Length2code
  Map match length value to length code for huff encoding.
}
function Length2code (const len: longword): longword; inline;
const
  table: array[byte] of byte = (
  1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 13, 13, 14, 14,
  14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18,
  18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20,
  20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23,
  23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
  25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26,
  26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
  26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
  28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29
  );
begin
  Assert(len >= 3);
  result := 256 + table[len-3];  //0..255 = literals, 256 = block end
end;


{ Code2length
  Map decoded length code to length value.
}
function Code2length(const code: longword): longword; inline;
const
  table: array[257..285] of byte = (  //final value minus 3
  0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56, 64,
  80, 96, 112, 128, 160, 192, 224, 255
  );
begin
  result := table[code] + 3;
end;


{ Distance2code
  Map distance value to distance code for huff encoding.
}
function Distance2code(const dist: integer): longword; inline;
const
  table_384: array [0..383] of byte = (
  0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9,
  9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12,
  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
  12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16
  );
  table_32768: array[0..127] of byte = (
  0,  17,
  18, 19, 20, 20, 21, 21, 22, 22, 22, 22, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24,
  24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
  26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
  28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29,
  29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
  29, 29, 29, 29, 29, 29
  );
begin
  if dist <= 384 then
      result := table_384[dist - 1]
  else begin
      result := table_32768[(dist - 1) shr 8];
  end;
end;


{ Code2distance
  Map decoded distance code to distance value.
}
function Code2distance(const code: longword): longword; inline;
const
  table: array[0..29] of word = (
  1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769,
  1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
  );
begin
  result := table[ code ];
end;

{ TVlcWriter }

procedure TVlcWriter.SetTrees(const bitstream: TBitstreamWriter;
  const length_tree, distance_tree: PVlcCode);
begin
  bs := bitstream;
  len_tree := length_tree;
  dist_tree := distance_tree;
end;

procedure TVlcWriter.WriteMatch(const len, dist: longword);
var
  code, bits: integer;
begin
  //length
  code := Length2code(len);
  bs.Write(len_tree[code].bits, len_tree[code].code_len);
  if (code >= 265) and (code < 285) then begin  //extra bits
      bits := 5 - (284 - code) div 4;
      bs.WriteSafe(len - 3, bits);
  end;

  //offset / distance
  code := Distance2code(dist);
  bs.Write(dist_tree[code].bits, dist_tree[code].code_len);
  bits := (code >> 1) - 1;
  if code >= 4 then begin
      bs.WriteSafe(dist - 1, bits);
  end;
end;

procedure TVlcWriter.WriteLiteral(const c: integer); inline;
begin
  bs.Write(len_tree[c].bits, len_tree[c].code_len);
end;

procedure TVlcWriter.WriteBlockEnd();
begin
  WriteLiteral(END_OF_BLOCK_CODE)
end;

{ vlc_ReadCode
  Read one canonical huffman code using the given decoding table. Maximum symbol length cannot
  exceed 15 bits (maximum allowed by Deflate), otherwise reading fails and bad things happen.
}
function vlc_ReadCodeSlow(const bs: TBitstreamReader; const table: TDecodeTable): integer;
var
  i, codes,
  diff, value: longword;
  value_low: longword;      //lowest value for code of given length
  codes_skipped: longword;  //how many codes we already skipped
  number_of_codes: pword;   //# codes of given length
begin
  i := 0;
  value := 0;
  codes_skipped := 0;
  value_low := 0;
  codes := 0;
  number_of_codes := @table.codes_of_legth[0];
  repeat
      codes_skipped += codes;
      value_low += codes;
      value_low := value_low shl 1;

      i += 1;
      Assert(i < 16, 'could not read vlc code');  //checking costs ~2%
      codes := number_of_codes[i];

      value := (value shl 1) or bs.readu();
      diff := value - value_low;
  until codes > diff;

  result := table.code_value[ codes_skipped + diff ];
end;


{ vlc_ReadCode
  Read one variable-length code using the given lookup table. If the code couldn't be read, try
  to read with the canon huff decoding table.
}
function vlc_ReadCode(const bs: TBitstreamReader; const dectable: TDecodeLookupTables): integer;
var
  bits: PtrInt;
begin
  bits := bs.Show9u();  //same as bs.Show(TAB0_BITS) but inlined and slightly faster
  result := dectable.codes_t0[bits].symbol;
  bits := dectable.codes_t0[bits].nbits;
  bs.Skipu(bits);
  if (bits = 0) then begin
      result := vlc_ReadCodeSlow(bs, dectable.canon_table);
      bs.Refill;
  end;
end;


{ InitDecodeLut
  Assign canonical huff code bits to each code by its length and build a look-up table for fast
  decoding. Uses separate code bits runs for each code length. Makes 2 passes over input data,
  one pass could be removed if code length stats were provided beforehand, but it doesn't gain much.
}
function InitDecodeLut(const code_lengths: pbyte; const count: integer): TDecodeLookupTables;
var
  i, len, code_bits: integer;
  value, k, b: integer;
  sb: TSymbolBits;
  num_lengths: array[0..15] of integer;  //# of codes of given length
  length_bits: array[0..15] of integer;  //canonical bits for codes of given length
begin
  FillByte(num_lengths, sizeof(num_lengths), 0);
  FillByte(length_bits, sizeof(length_bits), 0);
  for i := 0 to count - 1 do begin
      num_lengths[code_lengths[i]] += 1;
  end;
  b := 0;
  for i := 1 to 15 do begin
      length_bits[i] := b;
      b += num_lengths[i];
      b := b << 1;
  end;

  FillByte(result.codes_t0, sizeof(result.codes_t0), 0);
  for i := 0 to count - 1 do begin
      len := code_lengths[i];
      if not (len in [1..TAB0_BITS]) then
          continue;

      code_bits := length_bits[len];
      length_bits[len] += 1;
      sb.symbol := i;
      sb.nbits := len;

      //insert each code length + junk code_bits combination
      code_bits := SwapBits(code_bits, len);
      for k := 0 to 1 << (TAB0_BITS - len) - 1 do begin
          value := (k << len) or code_bits;
          result.codes_t0[value] := sb;
      end;
  end;
end;

{ TVlcReader }

procedure TVlcReader.SetTables(const bitreader: TBitstreamReader;
  const literal_table, distance_table: TDecodeLookupTables);
begin
  bs := bitreader;
  literal_dectable := literal_table;
  distance_dectable := distance_table;
end;

{ ReadCode + ReadDist
  There are at most 7 bits in buffer after refill, which gives 32-7=25 bits for unchecked reads.
  Here we do 9 bits lookup + 5 bits extra + 9 bits lookup = 23 bits at most;
  codes that cannot be decoded from lookup table cause buffer refills in vlc_ReadCode.
}
function TVlcReader.ReadCode: integer;
begin
  bs.Refill;
  result := vlc_ReadCode(bs, literal_dectable);
end;

{ ReadDist
  Code must be a valid length code read by ReadCode
}
procedure TVlcReader.ReadDist(code: integer; out length, distance: word);
const
  LENGTH_EXTRA_BITS: array[257..285] of byte = (
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
  );
var
  extra_bits: longword;
begin
  length := Code2length(code);
  extra_bits := LENGTH_EXTRA_BITS[code];
  if extra_bits > 0 then begin
      length += bs.Readu(extra_bits);
  end;

  code := vlc_ReadCode(bs, distance_dectable);
  distance := Code2distance(code);
  if code >= 4 then begin
      distance += bs.Read(code >> 1 - 1);  //bitstream refill check needed, can read up to 13 bits
  end;
end;


end.

(*******************************************************************************
vlc.pas
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
