unit dict_search;
{$mode objfpc}{$H+}

interface

uses
  common, math;

const
  MAX_COMPRESSION_LEVEL = 7;
  DICT_SIZE = MAX_BLOCK_SIZE;
  MAX_DEFLATE_MATCH_LENGTH = 258;

type
  TSearchResult = record
    distance,
    length: word;
  end;

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

  { TMatchSearcher }
  TMatchSearcher = class
  private
      _max_search_depth: integer;  //limit how many positions we want to check
      _max_search_match_length: integer;  //limit how long match needs to be to satisfy search conditions
      _links: pinteger;                   //linked list of hash occurences
      _last_seen_idx: pinteger;           //last known position of a hash in the stream
      _bytes_processed: integer;
      _current_chunk_size: integer;
      _sbuffer: TSlidingBuffer;    //sliding buffer for search window data

      function Search(const window_end_ptr, str: pbyte; const current_idx, max_match_length: integer
        ): TSearchResult;

  public
      constructor Create;
      destructor Destroy; override;
      procedure SetCompressionLevel(const level: integer);

      { New chunk of data that to be processed. }
      procedure NewData (const data: pbyte; const size: integer);

      {
        Find previous occurence of bytes in str.
          str         - searched data pointer
          data_index  - searched data index relative to current chunk
      }
      function FindMatch(const str: pbyte; const data_index: integer): TSearchResult;
  end;


(*******************************************************************************
*******************************************************************************)
implementation

const
  SEARCH_DEPTH: array[0..MAX_COMPRESSION_LEVEL] of Integer =
    (0, 4, 8, 16, 32, 48, 64, 128);
  SEARCH_MATCH_DIVIDER: array[0..MAX_COMPRESSION_LEVEL] of Integer =
    (1, 8, 4,  4,  4,  4,  2,   1);
  HASH_BITS = 18;

{
  make HASH_BITS bits long hash of first 3 bytes
}
function hash3(const x: pbyte): integer; inline;
begin
  result := ((x+2)^ shl 10) xor ((x+1)^ shl 5) xor x^;
end;


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


{ TMatchSearcher }

constructor TMatchSearcher.Create;
begin
  _sbuffer.Init();
  _max_search_depth := SEARCH_DEPTH[0];
  _max_search_match_length := MAX_DEFLATE_MATCH_LENGTH div SEARCH_MATCH_DIVIDER[0];

  _links := getmem(2 * DICT_SIZE * sizeof(integer));
  _last_seen_idx := getmem(1 shl HASH_BITS * sizeof(integer));  //must be equal to hash bits
  Filldword(_last_seen_idx^, 1 shl HASH_BITS, $ffffffff );  //negative indices don't get searched, so use -1
  _current_chunk_size := 0;
  _bytes_processed := 0;
end;

destructor TMatchSearcher.Destroy;
begin
  freemem(_links);
  freemem(_last_seen_idx);
  _sbuffer.Done;
  inherited;
end;

procedure TMatchSearcher.SetCompressionLevel(const level: integer);
begin
  Assert(level <= MAX_COMPRESSION_LEVEL, 'invalid compression level');
  _max_search_depth := SEARCH_DEPTH[level];
  _max_search_match_length := MAX_DEFLATE_MATCH_LENGTH div SEARCH_MATCH_DIVIDER[level];
end;

{
  Take next data chunk and create links between the occurences of the same hash
}
procedure TMatchSearcher.NewData(const data: pbyte; const size: integer);
var
  i, key, last_seen: integer;
begin
  _sbuffer.InsertData(data, size);
  _bytes_processed += _current_chunk_size;
  _current_chunk_size := size;

  move((_links + DICT_SIZE)^, _links^, DICT_SIZE * sizeof(integer));
  for i := 0 to size - 1 do begin
      key := hash3(data + i);
      last_seen := _last_seen_idx[key];
      _last_seen_idx[key] := i + _bytes_processed;
      _links[DICT_SIZE + i] := last_seen;
  end;
end;

{
  Compare strings, return length of the match.
  There must be at least max_match_length valid bytes in window.
}
function compare_strings(const window, string_data: pbyte;
   const max_match_length: integer): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to max_match_length - 1 do
      if window[i] = string_data[i] then
          result += 1
      else
          exit;
end;

{
  Compare last byte of the window against current string.
}
function compare_strings_rle(const string_data: pbyte; const byte_value, max_match_length: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to max_match_length - 1 do
      if byte_value = string_data[i] then
          result += 1
      else
          exit;
end;


function InitSearchResult(const distance, best_match: longword): TSearchResult; inline;
begin
  longword(result) := longword( best_match << 16 or distance );
end;

function TMatchSearcher.Search(const window_end_ptr, str: pbyte;
  const current_idx, max_match_length: integer): TSearchResult;
var
  i: integer;
  links: pinteger;
  best_match_distance: integer;
  best_match_length: integer;
  last_seen_idx: IntPtr;
  min_allowed_idx: integer;
  previous_idx: integer;
  length: integer;
  distance: integer;
begin
  Assert(max_match_length >= 3);

  //test if searched string is a repetition of the last byte before full search
  best_match_length := compare_strings_rle(str, window_end_ptr[-1], max_match_length);
  result := InitSearchResult(1, best_match_length);
  if best_match_length >= _max_search_match_length then
      exit;

  last_seen_idx := current_idx - _bytes_processed;
  links := _links + DICT_SIZE;
  best_match_distance := 1;
  min_allowed_idx := max(0, current_idx - DICT_SIZE);

  //early termination if links of the next searched position are much closer than current ones
  if links[last_seen_idx] < links[last_seen_idx + 1] - (DICT_SIZE shr 1) then
      exit;
    //does this help much? probably disable if lazymatching

  for i := _max_search_depth - 1 downto 0 do begin
      //if the position falls out of the sliding window_end_ptr range, it's too old and cannot be searched
      previous_idx := links[last_seen_idx];
      if previous_idx < min_allowed_idx then begin
          break;
      end;
      last_seen_idx := previous_idx - _bytes_processed;

      //compare data at given positions
      distance := current_idx - previous_idx;
      length := compare_strings(window_end_ptr - distance, str, max_match_length);

      if length > best_match_length then begin
          best_match_length := length;
          best_match_distance := distance;
          if length >= _max_search_match_length then
              break;
      end;
  end;

  Assert(best_match_distance >= 0);
  result := InitSearchResult(best_match_distance, best_match_length);
end;

{
  Find best match between current bytes and bytes already seen.
  If distance = 0 & length = 0 - no occurences were found
}
function TMatchSearcher.FindMatch(const str: pbyte; const data_index: integer): TSearchResult;
var
  max_match_length: integer;
  current_idx: integer;
  window_end_ptr: pbyte;
begin
  result := InitSearchResult(0, 0);

  //reduce maximum possible match length at the end of the stream
  //we need at least 3 bytes to be able to run search (hash function takes 3 bytes as input)
  max_match_length := min(MAX_DEFLATE_MATCH_LENGTH, _current_chunk_size - data_index);
  if max_match_length <= 2 then
      exit;

  //beginning of a stream, nothing to search
  if _bytes_processed + data_index = 0 then
      exit;

  //get proper search window and currently searched string's file index
  window_end_ptr := _sbuffer.GetWindow + data_index;
  current_idx := _bytes_processed + data_index;

  result := Search(window_end_ptr, str, current_idx, max_match_length);
end;


end.

(*******************************************************************************
dict_search.pas
Copyright (c) 2010-2018 David Pethes

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
