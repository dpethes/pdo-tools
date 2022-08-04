unit blocks;
{$mode objfpc}{$H+}

interface

uses
  sysutils,
  common, bitstream, vlc, huffcoding;

const
  MIN_MATCH_LENGTH = 3;
  END_OF_BLOCK = vlc.END_OF_BLOCK;

type
  TLiteralMatch = record
      match_length: word; //match length
      offset: word;       //match offset
      literal: byte;      //byte from input stream
  end;
  PLiteralMatch = ^TLiteralMatch;

  { TBlockWriter }
  TBlockWriter = class
  private
    bitWriter: TBitstreamWriter;      //bitstream writer
    literal_match_stats: pinteger;
    distance_stats: pinteger;
    literal_codes: PVlcCode;
    distance_codes: PVlcCode;
    huff_memory: pointer;

    _block_type: TBlockTypeEnum;
    _last: boolean;                   //last block in stream
    bs_cache: TBitstreamBufferState;  //state of the buffer at beginning of the block

    procedure BeginBlock;
    procedure BuildHuffCodes;
    procedure BuildFixedHuffCodes;
    procedure WriteCodingTrees;
    procedure WriteBlockEncoded(const search_results: PLiteralMatch; const size: integer);
    procedure WriteBlockRaw(const rawdata: pbyte; const rawsize: integer);

  public
    constructor Create(const output_buffer: pbyte);
    destructor Destroy; override;

    procedure InitNewBlock(const block_type: TBlockTypeEnum);
    procedure SetLast;
    procedure UpdateStatsMatch(const len, dist: longword); inline;
    procedure UpdateStatsLiteral(const literal: byte); inline;

    procedure WriteBlock(const rawdata: pbyte; const rawsize: integer;
      const search_results: PLiteralMatch; const size: integer; const keep_buffer: boolean = false);
    procedure Done;

    function GetStreamSize: integer;
  end;


  TBlockContext = record
      btype: TBlockTypeEnum;
      size:  integer;
      unfinished: boolean;
      last:  boolean;       //last block flag
  end;

  { TBlockReader }
  TBlockReader = class
  private
    _block_type: TBlockTypeEnum;
    _vlc: TVlcReader;
    procedure ReadHeaderCodes(const bs: TBitstreamReader);
    procedure InitFixedCodes(const bs: TBitstreamReader);
  public
    constructor Create;
    destructor Destroy; override;
    function ReadBlockHeader(const bs: TBitstreamReader): TBlockContext;
    function GetVlcReader: TVlcReader; inline;
  end;

implementation

const
  //code ordering for header code length alphabet
  //see RFC1951 section 3.2.7. Compression with dynamic Huffman codes (BTYPE=10)
  HeaderCodeLengthOrder: array[0..18] of byte = (
      16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  );

  LITERAL_MATCH_ELEMENTS = END_OF_STREAM + 1;
  DISTANCE_ELEMENTS = 30;
  MAX_CODE_LENGTHS = 286 + 32; //# of Literal/Length codes + # of Distance codes

type
  TRleData = record
      size: integer;
      nonzero: integer;
      rl_pairs: array[0..MAX_CODE_LENGTHS-1] of record
          code_len, repeats: byte;
      end;
      code_lengths: array[0..MAX_CODE_LENGTHS-1] of byte;
  end;

{
  Build length-limited huffman code tree.
  Length is limited by reducing the code occurence's statistics.
  Less accuracy means that the differences between code lengths are reduced, too.
  This is somewhat suboptimal.

  Distance trees are special, because there are cases where they contain only one used symbol.
}
procedure build_limited_tree(var tree: THuffTree; limit, size: word; const for_distance: boolean = false);
var
  i: integer;
  tree_ok: boolean;
  freq_limit: integer;
begin
  freq_limit := 256*2;
  repeat
      if for_distance then
          huff_build_distance_tree(tree, freq_limit)
      else
          huff_build_tree(tree, freq_limit);
      tree_ok := true;
      for i := 0 to size - 1 do
          if tree.codes[i].code_len > limit then begin
              tree_ok := false;
              freq_limit := freq_limit shr 1;
              break;
          end;
  until tree_ok;
  //reverse bits for faster bitwriting
  for i := 0 to size - 1 do begin
      tree.codes[i].bits := SwapBits(tree.codes[i].bits, tree.codes[i].code_len);
  end;
end;

{
  WriteCodeLengths

  Write lengths of tree's codes using, encoded by header_codes
}
procedure WriteCodeLengths
  (const bs: TBitstreamWriter; const header_codes, tree_codes: PVlcCode; const size: integer);
var
  i, k: integer;
  bits, length: longword;
begin
  for i := 0 to size - 1 do begin
      k := tree_codes[i].code_len;
      bits   := header_codes[k].bits;
      length := header_codes[k].code_len;
      bs.Write(bits, length);
  end;
end;


procedure WriteCodeLengthsRle
  (const bs: TBitstreamWriter; const header_codes: PVlcCode; const rle_res: TRleData);
var
  i, k: integer;
  bits, length: longword;
  size: integer;
begin
  size := rle_res.size;
  i := 0;
  while i < size do begin
      k := rle_res.rl_pairs[i].code_len;
      bits   := header_codes[k].bits;
      length := header_codes[k].code_len;
      bs.Write(bits, length);

      if k = 16 then begin //Copy the previous code length, the next 2 bits indicate repeat length
          bs.Write(rle_res.rl_pairs[i].repeats, 2);
      end
      else if k = 17 then begin  //Repeat a code length of 0, 3 bits of length
          bs.Write(rle_res.rl_pairs[i].repeats, 3);
      end
      else if k = 18 then begin  //Repeat a code length of 0, 7 bits of length
          bs.Write(rle_res.rl_pairs[i].repeats, 7);
      end;
      i += 1;
  end;
end;


{
  GetHclen

  Reduce the number of header tree code lengths that need to be stored.
  Saves a couple of bits per block.
}
function GetHclen(const min_length, max_length: integer): integer;
var
  i, length: integer;
begin
  result := 19;
  for i := 18 downto 5 do begin
      length := HeaderCodeLengthOrder[i];
      if (length >= min_length) and (length <= max_length) then
          break
      else
          result -= 1;
  end;
  result -= 4;
end;


function RleCodeBuffer(const src: array of byte; const size: integer): TRleData;
var
  i, k, m: integer;
  value, next_value: byte;
  max_lookahead: integer;
  run_length: integer;
  nonzero: integer;

  procedure AddRl(const code_len, repeats: byte);
  begin
    result.rl_pairs[m].code_len := code_len;
    result.rl_pairs[m].repeats := repeats;
    m += 1;
  end;

begin
  nonzero := 0;
  i := 0;
  m := 0;
  while i < size do begin
      value := src[i];
      //zero runs
      if value = 0 then begin
          max_lookahead := 137;
          if size - i < max_lookahead then
              max_lookahead := size - i;

          run_length := 1;
          for k := i + 1 to i + max_lookahead - 1 do begin
              next_value := src[k];
              if value = next_value then
                  run_length += 1
              else
                  break;
          end;
          if run_length < 3 then begin
              AddRl(0, 0);
              i += 1;
          end else begin
              i += run_length;
              if run_length < 11 then
                  AddRl(17, run_length - 3) //Repeat a code length of 0 for 3 - 10 times.
              else
                  AddRl(18, run_length - 11); //Repeat a code length of 0 for 11 - 138 times.
          end;
      end
      //nonzero runs
      else begin
          max_lookahead := 6;
          if size - i <= max_lookahead then
              max_lookahead := size - i - 1;

          run_length := 0;
          for k := i + 1 to i + max_lookahead do begin
              next_value := src[k];
              if value = next_value then
                  run_length += 1
              else
                  break;
          end;
          if run_length >= 3 then begin
              AddRl(value, 0);
              AddRl(16, run_length - 3); //Copy the previous code length 3 - 6 times.
              i += run_length + 1;
              nonzero += run_length + 1;
          end else begin
              AddRl(value, 0);
              i += 1;
              nonzero += 1;
          end;
      end;
  end;

  result.size := m;
  result.nonzero := nonzero;

  for i := 0 to result.size - 1 do
      result.code_lengths[i] := result.rl_pairs[i].code_len;
end;


{ TBlockWriter }

{
  BeginBlock

  Write block header.
  header bytes:
    BFINAL - 1 bit
    BTYPE  - 2 bits
}
procedure TBlockWriter.BeginBlock;
begin
  bitWriter.Write(longword( _last ) and 1);
  bitWriter.Write(longword( _block_type ), 2);
end;

{ BuildFixedHuffCodes
  Create vlc trees for blocks compressed using fixed Huffman codes
}
procedure TBlockWriter.BuildFixedHuffCodes;
var
  i, bits: integer;

  function vlc(const b, len: integer): TVlcCode;
  begin
    result.bits := SwapBits(b, len);
    result.code_len := len;
    bits += 1;
  end;

begin
  bits := 0;
  for i := 256 to 279 do literal_codes[i] := vlc(bits, 7);
  bits := bits << 1;
  for i :=   0 to 143 do literal_codes[i] := vlc(bits, 8);
  for i := 280 to 287 do literal_codes[i] := vlc(bits, 8);
  bits := bits << 1;
  for i := 144 to 255 do literal_codes[i] := vlc(bits, 9);
  for i := 0 to 29 do distance_codes[i] := vlc(i, 5);
end;

{ BuildHuffCodes
  Create vlc trees from accumulated statistics for literal/length and distance coding
}
procedure TBlockWriter.BuildHuffCodes;
var
  i: integer;
  tree: THuffTree;
begin
  //generate literal/match codes
  huff_init(tree, huff_memory);
  for i := 0 to LITERAL_MATCH_ELEMENTS - 1 do
      tree.counts[i] := literal_match_stats[i];
  tree.counts[END_OF_BLOCK_CODE] := 1;

  build_limited_tree(tree, 15, LITERAL_MATCH_ELEMENTS);
  Move(tree.codes ^, literal_codes ^, LITERAL_MATCH_ELEMENTS * sizeof(TVlcCode));

  //generate distance codes
  huff_init(tree, huff_memory);
  for i := 0 to DISTANCE_ELEMENTS - 1 do
      tree.counts[i] := distance_stats[i];

  build_limited_tree(tree, 15, DISTANCE_ELEMENTS, true);
  Move(tree.codes ^, distance_codes ^, DISTANCE_ELEMENTS * sizeof(TVlcCode));
end;

{
  WriteCodingTrees

  Store literal/length and distance trees.
  Canonical huff coding is used, so code lengths are enough to store the trees.
}
procedure TBlockWriter.WriteCodingTrees;
var
  max_used_codelength, min_used_codelength: integer;
  block_header_tree: THuffTree;

  header_codes: PVlcCode;
  hclen, hlit, hdist: integer;
  i, length: integer;

  codelen_buffer: array[0..MAX_CODE_LENGTHS - 1] of byte;
  pcb: pbyte;
  rle_data: TRleData;
begin
  //build header tree for coding the literal/length and distance tree code lengths
  pcb := @codelen_buffer;
  for i := 0 to LITERAL_MATCH_ELEMENTS - 1 do
      pcb[i] := literal_codes[i].code_len;

  pcb += LITERAL_MATCH_ELEMENTS;
  for i := 0 to DISTANCE_ELEMENTS - 1 do
      pcb[i] := distance_codes[i].code_len;

  rle_data := RleCodeBuffer(codelen_buffer, LITERAL_MATCH_ELEMENTS + DISTANCE_ELEMENTS);

  huff_init(block_header_tree, huff_memory);
  max_used_codelength := 0;
  min_used_codelength := 15;
  for i := 0 to rle_data.size - 1 do begin
      length := rle_data.code_lengths[i];
      huff_symbol_inc(block_header_tree, length);
      if (length < min_used_codelength) and (length > 0) then min_used_codelength := length;
      if length > max_used_codelength then max_used_codelength := length;
  end;
  build_limited_tree(block_header_tree, 7, 19);

  //store the header tree code lengths
  //when do we want to define a smaller hdist? if we used a smaller encoding window, thus limiting the distances?
  hlit := 286 - 257;
  hdist := 29;
  hclen := GetHclen(min_used_codelength, max_used_codelength);
  bitWriter.Write(hlit, 5);
  bitWriter.Write(hdist, 5);
  bitWriter.Write(hclen, 4);
  header_codes := block_header_tree.codes;
  for i := 0 to hclen + 4 - 1 do
      bitWriter.Write( header_codes[ HeaderCodeLengthOrder[i] ].code_len, 3 );

  //store codes of literal/length and distance trees
  WriteCodeLengthsRle(bitWriter, header_codes, rle_data);
end;

{ WriteBlockEncoded
  Write a complete block into bitstream: literals and match length / distance pairs, END_OF_BLOCK symbol.
  Handles distinction between blocks compressed using fixed or dynamic huff codes
}
procedure TBlockWriter.WriteBlockEncoded(const search_results: PLiteralMatch; const size: integer);
var
  i: integer;
  lm: TLiteralMatch;
  vlc: TVlcWriter;
begin
  if _block_type = BTDynamic then begin
      BuildHuffCodes;
      WriteCodingTrees();
  end else
      BuildFixedHuffCodes();
  vlc.SetTrees(bitWriter, literal_codes, distance_codes);

  for i := 0 to size - 1 do begin
      lm := search_results[i];
      if lm.match_length > 0 then
          vlc.WriteMatch(lm.match_length, lm.offset)
      else
          vlc.WriteLiteral(lm.literal);
  end;

  vlc.WriteBlockEnd();
end;

{
  WriteBlockRaw

  Write a raw block into bitstream: copy input values.
  Raw block header:
    n bits  - byte alignment
    16 bits - data length
    16 bits - inverted data length
}
procedure TBlockWriter.WriteBlockRaw(const rawdata: pbyte; const rawsize: integer);
var
  i: integer;
begin
  bitWriter.ByteAlign;
  bitWriter.Write(rawsize, 16);
  bitWriter.Write(rawsize xor $ffff, 16);

  for i := 0 to rawsize - 1 do
      bitWriter.Write(rawdata[i], 8);  //todo: memcpy
end;

constructor TBlockWriter.Create(const output_buffer: pbyte);
begin
  bitWriter := TBitstreamWriter.Create(output_buffer);
  bs_cache := bitWriter.GetState;

  literal_match_stats := GetMem(LITERAL_MATCH_ELEMENTS * sizeof(integer));
  distance_stats := Getmem(DISTANCE_ELEMENTS * sizeof(integer));

  literal_codes := GetMem(LITERAL_MATCH_ELEMENTS * sizeof(TVlcCode));
  distance_codes := GetMem(DISTANCE_ELEMENTS * sizeof(TVlcCode));

  huff_memory := huff_alloc();
end;

destructor TBlockWriter.Destroy;
begin
  inherited Destroy;
  bitWriter.Free;
  Freemem(literal_match_stats);
  Freemem(distance_stats);
  Freemem(literal_codes);
  Freemem(distance_codes);
  huff_free(huff_memory);
end;

procedure TBlockWriter.InitNewBlock(const block_type: TBlockTypeEnum);
begin
  FillDWord(literal_match_stats^, LITERAL_MATCH_ELEMENTS, 0);
  FillDWord(distance_stats^, DISTANCE_ELEMENTS, 0);

  bitWriter.SetState(bs_cache);
  _block_type := block_type;
  _last := false;
end;

procedure TBlockWriter.SetLast;
begin
  _last := true;
end;

procedure TBlockWriter.UpdateStatsMatch(const len, dist: longword);
begin
  literal_match_stats[ Length2code(len) ] += 1;
  distance_stats[ Distance2code(dist) ] += 1;
end;

procedure TBlockWriter.UpdateStatsLiteral(const literal: byte);
begin
  literal_match_stats[ literal ] += 1;
end;

procedure TBlockWriter.WriteBlock(const rawdata: pbyte; const rawsize: integer;
  const search_results: PLiteralMatch; const size: integer; const keep_buffer: boolean);
begin
  if not keep_buffer then
      bitWriter.ResetBufferPosition;
  BeginBlock();
  if _block_type <> BTRaw then
      WriteBlockEncoded(search_results, size)
  else
      WriteBlockRaw(rawdata, rawsize);
end;

procedure TBlockWriter.Done;
begin
  //throw away bs cache
  bs_cache := bitWriter.GetState;
end;

{
  GetStreamSize

  Returns number of whole bytes that were written into bitstream for current block.
  The last written bit doesn't have to be at a byte aligned position,
  so we need to cache the write buffer and mask to put the bits in the next processed block.
  If the current block is the last one processed, the outstanding bits must be counted into the stream size
  (they would be lost otherwise).
}
function TBlockWriter.GetStreamSize: integer;
begin
  if not _last then begin
      result := bitWriter.GetUnbufferedByteSize;
  end else begin
      bitWriter.Close;
      result := bitWriter.GetByteSize;
  end;
end;


{ TBlockReader }

{ ReadHeaderCodes
  Read code lengths and generate tables for dynamic block decoding
}
procedure TBlockReader.ReadHeaderCodes(const bs: TBitstreamReader);
var
  literal_dectable,                       //literal/length decoding table
  distance_dectable: TDecodeLookupTables; //distance decoding table
  hlit, hdist, hclen: word;
  len, last_len: integer;  //code length, previous code length
  i, k, extra_bits: longword;
  code_lengths: array[0..MAX_CODE_LENGTHS-1] of byte;
  dt: TDecodeLookupTables;
begin
  hlit  := bs.Read(5) + 257;
  hdist := bs.Read(5) + 1;
  hclen := bs.Read(4) + 4;

  //get code_len codes
  FillByte(code_lengths, 19, 0);
  for i := 0 to hclen - 1 do begin
      k := HeaderCodeLengthOrder[i];
      code_lengths[k] := bs.Read(3);
  end;
  dt := InitDecodeLut(code_lengths, 19);

  //decode symbols
  //refill could be called less often, but this is not perf critical
  FillByte(code_lengths, MAX_CODE_LENGTHS, 0);
  i := 0;
  last_len := 16;
  while i < hlit + hdist do begin
      bs.Refill;
      len := vlc_ReadCode(bs, dt);

      if len < 16 then begin
           code_lengths[i] := len;
           i += 1;
           last_len := len;
      end
      else
          case len of
              16: begin  //rep previous length
                  Assert(last_len <> 16, 'dynamic block header error');
                  extra_bits := bs.Read(2);
                  k := i;
                  i += extra_bits + 3;
                  for k := k to i - 1 do
                      code_lengths[k] := last_len;
                  end;
              17: begin  //rep zero length
                  extra_bits := bs.Read(3);
                  i += extra_bits + 3;
                  end;
              18: begin  //rep zero length
                  extra_bits := bs.Read(7);
                  i += extra_bits + 11;
                  end
          end;
  end;

  literal_dectable  := InitDecodeLut(pbyte(@code_lengths), hlit);
  huff_FillCanonDecodingTable(literal_dectable.canon_table, pbyte(@code_lengths), hlit);

  distance_dectable := InitDecodeLut(pbyte(@code_lengths) + hlit, hdist);
  huff_FillCanonDecodingTable(distance_dectable.canon_table, pbyte(@code_lengths) + hlit, hdist);

  _vlc.SetTables(bs, literal_dectable, distance_dectable);
end;

procedure TBlockReader.InitFixedCodes(const bs: TBitstreamReader);
var
  literal_dectable,                       //literal/length decoding table
  distance_dectable: TDecodeLookupTables; //distance decoding table
  code_lengths: array[0..287] of byte;
  i: integer;
begin
  for i := 256 to 279 do code_lengths[i] := 7;
  for i :=   0 to 143 do code_lengths[i] := 8;
  for i := 280 to 287 do code_lengths[i] := 8;
  for i := 144 to 255 do code_lengths[i] := 9;
  literal_dectable := InitDecodeLut(pbyte(@code_lengths), 288);
  huff_FillCanonDecodingTable(literal_dectable.canon_table, pbyte(@code_lengths), 288);

  for i := 0 to 31 do code_lengths[i] := 5;
  distance_dectable := InitDecodeLut(pbyte(@code_lengths), 32);
  huff_FillCanonDecodingTable(distance_dectable.canon_table, pbyte(@code_lengths), 32);

  _vlc.SetTables(bs, literal_dectable, distance_dectable);
end;

constructor TBlockReader.Create;
begin
  _vlc := TVlcReader.Create;
end;

destructor TBlockReader.Destroy;
begin
  _vlc.Free;
  inherited;
end;

{ ReadBlockHeader
  Reads block header including length trees for codes
}
function TBlockReader.ReadBlockHeader(const bs: TBitstreamReader): TBlockContext;
var
  block: TBlockContext;
  t: integer;
begin
  block.last  := bs.Read() = 1;
  block.btype := TBlockTypeEnum( bs.Read(2) );
  _block_type := block.btype;

  case block.btype of
      BTRaw: begin
          while not bs.IsByteAligned() do
              bs.Read();
          block.size := bs.Read(16);
          t := not integer(bs.Read(16));
          Assert(block.size = t, 'blk size mismatch');
      end;
      BTDynamic: begin
          ReadHeaderCodes(bs);
      end;
      BTFixed: begin
          InitFixedCodes(bs);
      end;
  end;

  Result := block;
end;


function TBlockReader.GetVlcReader(): TVlcReader;
begin
  result := _vlc;
end;


end.

(*******************************************************************************
blocks.pas
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
