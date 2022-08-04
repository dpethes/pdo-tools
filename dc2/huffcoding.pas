unit huffcoding;
{$mode objfpc}{$H+}

interface

uses
  sysutils;

const
  END_OF_STREAM = 285;
  TOP_NODE      = END_OF_STREAM * 2 + 1;

type
  //huff tree definitions
  TTreeNode = record
      child_0: word;
      child_1: word;
  end;
  PTreeNode = ^TTreeNode;

  TVlcCode = record
      bits: word;
      code_len: byte;
  end;
  PVlcCode = ^TVlcCode;

  THuffTree = record
      counts: plongword;
      nodes:  PTreeNode;
      codes:  PVlcCode;
      root_node: longword;
  end;

  //fixed huffcodes are actually constructed using codes <0..287>, but last 2 are never used
  TDecodeTable = record
      codes_of_legth: array[0..15] of word;     //number of codes for given length
      code_value: array[0..END_OF_STREAM + 2] of word; //map code to literal/length value
  end;
  PDecodeTable = ^TDecodeTable;

procedure huff_FillCanonDecodingTable(var tab: TDecodeTable; const code_lengths: pbyte; const count: integer);
procedure huff_code2canon(const codes: PVlcCode);

procedure huff_init(out h: THuffTree; const tree_memory: pointer);
function huff_alloc: pointer;
procedure huff_free(tree_memory: pointer);

procedure huff_symbol_inc(var h: THuffTree; const val: word); inline;
procedure huff_build_tree(var h: THuffTree; max_count: word = 255);
procedure huff_build_distance_tree(var h: THuffTree; max_count: word = 255);

(******************************************************************************)
implementation

{ huff_FillCanonDecodingTable

  code_lengths - array of lengths, indexed by code
  count - number of codes to fill
}
procedure huff_FillCanonDecodingTable(var tab: TDecodeTable; const code_lengths: pbyte; const count: integer);
var
  len: integer; //current length; all deflate code lengths are between 1 and 15
  same_length_count: integer;
  i, j: integer;
begin
  j := 0;
  tab.codes_of_legth[0] := 0;
  for len := 1 to 15 do begin
      same_length_count := 0;

      for i := 0 to count - 1 do begin
          if code_lengths[i] = len then begin
              tab.code_value[j] := i;
              j += 1;
              same_length_count += 1;
              if j = count then
                  break;
          end;
      end;

      tab.codes_of_legth[len] := same_length_count;
  end;
end;

{ huff_code2canon
  write canonical codes for each symbol
}
procedure huff_code2canon(const codes: PVlcCode);
var
  len: integer;
  b, i: integer;
begin
  b := 0;
  for len := 1 to 15 do begin
      for i := 0 to END_OF_STREAM do begin
          if codes[i].code_len = len then begin
              codes[i].bits := b;
              b += 1;
          end;
      end;
      b := b shl 1;
  end;
end;

{
  memory allocation
  As there's only one hufftree used at time, we can get away with reallocating the trees onto the
  same buffer
}
const
  TH_counts = sizeof(longword ) * (END_OF_STREAM + 1);
  TH_nodes  = sizeof(TTreeNode) * (END_OF_STREAM + 1) * 2;
  TH_codes  = sizeof(TVlcCode ) * (END_OF_STREAM + 1);
  TH_size = TH_counts + TH_nodes + TH_codes;

procedure huff_init(out h: THuffTree; const tree_memory: pointer);
var
  p: PByte;
begin
  p := tree_memory;
  Assert(p <> nil);
  FillByte(p^, TH_size, 0);
  h.counts := PLongWord( p );
  h.nodes  := PTreeNode( p + TH_counts );
  h.codes  := PVlcCode ( p + TH_counts + TH_nodes );
  h.root_node := 0;
end;

function huff_alloc: pointer;
begin
  result := GetMem(TH_size);
end;

procedure huff_free(tree_memory: pointer);
begin
  Freemem(tree_memory);
end;

{
  element counting & scaling
}
procedure huff_symbol_inc(var h: THuffTree; const val: word);
begin
  Assert(val <= END_OF_STREAM);
  h.counts[val] += 1;
end;

procedure scale_symbol_count(const counts: plongword; const max_count: word);
var
  b: integer;
  max: longword;
  new: longword;
  ratio: single;
begin
  max := 0;
  for b := 0 to END_OF_STREAM do
      if counts[b] > max then max := counts[b];

  if max <= max_count then exit;

  ratio := single( max ) / max_count;
  for b := 0 to END_OF_STREAM do
      if counts[b] > 0 then begin
          new := round( counts[b] / ratio );
          if new = 0 then
              counts[b] := 1
          else
              counts[b] := new;
      end;
end;

{ build_tree
  use the symbol counts as weights, keep the original counts if the tree needs to be rebuilt
}
procedure build_tree(var h: THuffTree);
var
  weights: array[0..TOP_NODE + 1] of longword;
  nodes: PTreeNode;
  next_free: integer;
  i: integer;
  min_1,
  min_2: integer;
begin
  nodes := h.nodes;
  FillByte(weights, TOP_NODE * 4, 0);  //slight overlap, but no effect on perf
  move(h.counts^, weights, (END_OF_STREAM + 1) * 4);
  weights[TOP_NODE] := High(longword);

  next_free := END_OF_STREAM;
  while true do begin
      next_free := next_free + 1;
      min_1 := TOP_NODE;
      min_2 := TOP_NODE;

      for i := 0 to next_free - 1 do begin
          if weights[i] > 0 then begin
              if weights[i] < weights[min_1] then begin
                  min_2 := min_1;
                  min_1 := i;
              end
              else
                  if weights[i] < weights[min_2] then
                      min_2 := i;
          end;
      end;

      if min_2 = TOP_NODE then
          break;

      weights[next_free] := weights[min_1] + weights[min_2];
      weights[min_1] := 0;
      weights[min_2] := 0;
      nodes[next_free].child_0 := min_1;
      nodes[next_free].child_1 := min_2;
  end;


  h.root_node := next_free - 1;
end;


{ tree_to_code
  recursively traverse the tree and write the bitcount for each symbol
}
procedure tree_to_code(var p: THuffTree; code_len_current, node: integer);
begin
  if node <= END_OF_STREAM then
      p.codes[node].code_len := code_len_current
  else begin
      code_len_current := code_len_current + 1;
      tree_to_code (p, code_len_current, p.nodes[node].child_0 );
      tree_to_code (p, code_len_current, p.nodes[node].child_1 );
  end;
end;


{ generate canonical huff codes
}
procedure huff_build_tree(var h: THuffTree; max_count: word = 255);
begin
  scale_symbol_count(h.counts, max_count);
  build_tree(h);
  tree_to_code(h, 0, h.root_node);
  huff_code2canon(h.codes);
end;

//special case, when there is only one symbol in alphabet - can happen with distance trees
procedure huff_build_distance_tree(var h: THuffTree; max_count: word = 255);
const
  MAX_DIST_CODES = 32;
var
  i: integer;
  last_nonzero_idx: integer;
  used_symbols_count: integer;
begin
  used_symbols_count := 0;
  last_nonzero_idx := 0;
  for i := 0 to MAX_DIST_CODES - 1 do
      if h.counts[i] > 0 then begin
          used_symbols_count += 1;
          if used_symbols_count > 1 then
              break;
          last_nonzero_idx := i;
      end;

  if used_symbols_count > 1 then begin
      huff_build_tree(h, max_count);
  end else begin
      h.codes[last_nonzero_idx].code_len := 1;
      h.codes[last_nonzero_idx].bits := 0;
      h.root_node := last_nonzero_idx;
  end
end;

end.

(*******************************************************************************
huffcoding.pas
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
