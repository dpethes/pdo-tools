unit dc2_decoder;
{$mode objfpc}{$H+}

interface

uses
  sysutils,
  common, bitstream, blocks, vlc;

type
  TDecodedSlice = record
      data: pbyte;
      size: integer;
  end;

  { TDc2Decoder }

  TDc2Decoder = class
  private
    bitReader: TBitstreamReader;       //bitstream reader
    blockDecoder: TBlockReader;
    output_buffer: pbyte;              //buffer for decoded data

    bs_buffer: pbyte;                  //buffer for decoded data, used by bitstream reader
    bs_byte_count: longword;

    block: TBlockContext;

    stream_start: boolean;             //set after decoder initialization
    stream_end: boolean;               //must be set if last data from the stream is passed to DecodeSlice
    blocks_end: boolean;               //set if end of last block is reached
    is_new_data: boolean;              //expect new data passed to DecodeSlice
    previous_bytes_count: integer;     //number of bytes decoded in previous DecodeSlice() call

    stats: TStats;                     //block stats
    function DecodeBlock: TDecodedSlice;

  public
    constructor Create;
    destructor Destroy; override;
    //Decode given compressed data
    procedure DecodeSlice(const data: pbyte; const input_size: longword; out slice: TDecodedSlice);
    //Signal the last input block
    procedure SetLastSlice;
  end;



//******************************************************************************
implementation

const
  PAD_BITS = 2048;  //should be enough to store the whole dynamic code header
  CHECK_PASSES = 32;  //how often should the decoding loop buffers be checked
  OUT_BUFFER_PAD_BYTES = CHECK_PASSES * 258;  //(# of passes between checks) * (max Deflate match length)
  OUT_BUFFER_SIZE = MAX_BLOCK_SIZE + OUT_BUFFER_PAD_BYTES;
  IN_VLC_PAD_BYTES = CHECK_PASSES * MAX_VLC_LENGTH;

{ TDc2Decoder }

constructor TDc2Decoder.Create;
const
  BS_BUFFER_SIZE = 128 * 1024;
begin
  bs_buffer := getmem(BS_BUFFER_SIZE);
  bitReader := TBitstreamReader.Create(bs_buffer);

  is_new_data := false;
  output_buffer := getmem(2 * OUT_BUFFER_SIZE);
  output_buffer += OUT_BUFFER_SIZE;

  blockDecoder := TBlockReader.Create;

  block.last := false;
  block.unfinished := false;
  stream_start := true;
  stream_end := false;
  blocks_end := false;
  Fillbyte(stats, sizeof(TStats), 0);
end;

destructor TDc2Decoder.Destroy;
begin
  inherited Destroy;
  freemem(bs_buffer);
  bitReader.Free;

  freemem(output_buffer - OUT_BUFFER_SIZE);
  blockDecoder.Free;
end;

procedure TDc2Decoder.SetLastSlice;
begin
  stream_end := true;
end;

procedure TDc2Decoder.DecodeSlice(const data: pbyte; const input_size: longword; out slice: TDecodedSlice);
var
  bytes_used: longword;
  cache: TBitstreamBufferState;
begin
  slice.size := 0;
  if blocks_end then exit;

  //open stream or just add new data
  if stream_start then begin
      move( data^, bs_buffer^, input_size );
      bs_byte_count := input_size;
      stream_start := false;
      bitReader.Start();
  end
  else if is_new_data then begin
      //delete processed part and store state (unprocessed bits)
      bytes_used := bitReader.GetUncachedPosition();
      cache := bitReader.GetState;

      //move the rest to the beginning of the buffer
      Assert(bytes_used <= bs_byte_count, 'buffer error');
      bs_byte_count -= bytes_used;
      if bs_byte_count > 0 then
          move( (bs_buffer + bytes_used)^, bs_buffer^, bs_byte_count );

      //insert new bitstream data
      move( data^, (bs_buffer + bs_byte_count)^, input_size );
      bs_byte_count += input_size;

      bitReader.ResetBufferPosition;
      bitReader.SetState(cache);
  end;

  //do we still need to process some data?
  if (not stream_end) and (bitReader.GetPosition() + PAD_BITS >= bs_byte_count) then begin
      is_new_data := true;
      exit;
  end;

  //move previously decoded data to dictionary buffer
  if previous_bytes_count > 0 then
      move((output_buffer + previous_bytes_count - OUT_BUFFER_SIZE)^,
           (output_buffer - OUT_BUFFER_SIZE)^,
           OUT_BUFFER_SIZE);

  slice := DecodeBlock();
  previous_bytes_count := slice.size;
end;

{ copy_block
  Move 8 bytes at time from src to dest. If length isn't a multiple of 8, some extra bytes
  are copied as well - the output buffer needs some padding for this case. This is faster
  than copying precisely length bytes
}
procedure copy_block(src, dst: pbyte; length: integer); inline;
begin
  while length > 0 do begin
      PUInt64(dst)^ := PUInt64(src)^;
      dst += 8;
      src += 8;
      length -= 8;
  end;
end;

{ copy_overlap
  Copy overlapping chunks of memory.
  It's worth having extra code for the special case when we are actually just copying one byte.
  Also copying from the chunk that just has been written to seems to be slower than copying
  from chunks that have been written to earlier, so try to avoid it
}
procedure copy_overlap(src, dst: pbyte; length: integer);
var
  k: integer;
  src_loop: pbyte;
  diff: integer;
begin
  diff := dst - src;

  if diff = 1 then begin
      FillByte(dst^, length, src^);
      exit;
  end;

  while length >= diff do begin
      src_loop := src;
      for k := 0 to diff - 1 do begin
          dst^ := src_loop^;
          dst += 1;
          src_loop += 1;
      end;
      length -= diff;
  end;
  src_loop := src;
  for k := 0 to length - 1 do begin
      dst^ := src_loop^;
      dst += 1;
      src_loop += 1;
  end;
end;

{ DecodeBlock
  Decode single Deflate block. Stops at the end of the block, or when output buffer is full or
  there's not enough input data. Block can be already started by previous call to this function.
}
function TDc2Decoder.DecodeBlock: TDecodedSlice;
var
  i: longword;
  offs, mlen: word;
  dst: pbyte;
  stream_check: integer;
  vlc: TVlcReader;
begin  
  if not block.unfinished then begin
      block := blockDecoder.ReadBlockHeader(bitReader);
      stats.blocks[block.btype] += 1;
  end;

  i := 0;
  case block.btype of

      //copy block without change
      BTRaw: begin
          while block.size > 0 do begin
              output_buffer[i] := bitReader.Read(8);
              i += 1;
              block.size -= 1;

              if (not stream_end) and (bitReader.GetPosition() + 8 >= bs_byte_count) then
                  break;
          end;

          block.unfinished := block.size > 0;
          if block.last and not block.unfinished then
              blocks_end := true;
      end;

      //inflate block
      BTFixed, BTDynamic: begin
          vlc := blockDecoder.GetVlcReader();
          stream_check := CHECK_PASSES;
          dst := output_buffer;
          while true do begin
              vlc.ReadCodePair(mlen, offs);

              //literal
              if mlen = 1 then begin
                  dst^ := byte( offs );
              end
              //match, no loop
              else if offs >= mlen then begin
                  copy_block(dst - offs, dst, mlen);
              end
              //match with loop
              else if mlen <> END_OF_BLOCK then begin
                  copy_overlap(dst - offs, dst, mlen);
              end
              else begin
                  block.unfinished := false;
                  if block.last then blocks_end := true;

                  { if we decoded only the EOB symbol in this run, we have to run decoding on the next block,
                    because zero-size slice means there wasn't any more data to decode,
                    which is simply not true if we got to block decoding
                  }
                  if (i = 0) and not block.last then begin
                      result := DecodeBlock();
                      exit;
                  end;
                  break;
              end;
              i += mlen;
              dst += mlen;

              stream_check -= 1;
              if stream_check = 0 then begin
                  //check if there's enough bits to decode in current buffer
                  if (not stream_end) and (bitReader.GetPosition() + IN_VLC_PAD_BYTES >= bs_byte_count) then begin
                      block.unfinished := true;
                      break;
                  end;
                  //check if there's enough space to output decoded data
                  if i + OUT_BUFFER_PAD_BYTES - OUT_BUFFER_SIZE > 0 then begin
                      block.unfinished := true;
                      break;
                  end;
                  stream_check := CHECK_PASSES;
              end;
          end;
      end;

      //block type error
      BTError: begin
          writeln( stderr, 'error in input stream: reserved block type used' );
          blocks_end := true;
          halt();
      end;
  end;

  is_new_data := i = 0;

  result.data := output_buffer;
  result.size := i;
end;

end.

(*******************************************************************************
dc2_decoder.pas
Copyright (c) 2014-2018 David Pethes

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
