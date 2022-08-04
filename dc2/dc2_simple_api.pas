unit dc2_simple_api;
{$mode objfpc}{$H+}
{define use_zstream}

interface

uses
  Classes, SysUtils,
  {$ifdef use_zstream}zstream,{$endif}
  common, dc2_encoder, dc2_decoder;

type
  { TLzEncoder }
  TLzEncoder = class
  private
      _ctx: TDc2Encoder;
      _encoded_size: integer;

  public
      constructor Create(const compression_level: byte = 2);
      destructor Destroy; override;
      function EncodeBytesToStream(const src: pbyte; const size: integer; var dest: TMemoryStream): integer;
      procedure WriteStats;
  end;

procedure LzDecodeBytes(const src: pbyte; const size: integer; const dest: pbyte);

implementation

procedure LzDecodeBytes(const src: pbyte; const size: integer; const dest: pbyte);
var
  compressed_size: longword;
  decoder: TDc2Decoder;
  read_buffer_size: longword;
  src_data, decoded_data: pbyte;
  decoded_data_counter: longword;
  decdata: TDecodedSlice;
begin
  src_data := src;
  decoded_data := dest;
  read_buffer_size := MAX_BLOCK_SIZE;
  decoder := TDc2Decoder.Create;
  decoded_data_counter := 0;

  while decoded_data_counter < size - 1 do begin
      if size - 1 - decoded_data_counter > read_buffer_size then begin
          compressed_size := read_buffer_size;
      end else begin
          compressed_size := size - 1 - decoded_data_counter;
          decoder.SetLastSlice;
      end;
      repeat
          decoder.DecodeSlice(src_data, compressed_size, decdata);
          move(decdata.data^, decoded_data^, decdata.size);
          decoded_data += decdata.size;
      until decdata.size = 0;
      src_data += compressed_size;
      decoded_data_counter += compressed_size;
  end;
  decoder.Free;
end;

{
//decoding through zstream for comparison only
procedure LzDecodeBytes(const src: pbyte; const size: integer; const dest: pbyte);
var
  src_stream, dest_stream: TMemoryStream;
  zs: Tdecompressionstream;
begin
  src_stream := TMemoryStream.Create;
  src_stream.WriteBuffer(src^, size);
  src_stream.WriteDWord(0);  //workaround: paszlib can fail in rare cases if it approaches the end of the stream
  src_stream.Seek(0, soFromBeginning);

  zs := Tdecompressionstream.create(src_stream, true);
  dest_stream := TMemoryStream.Create;
  dest_stream.CopyFrom(zs, 0);
  Move(dest_stream.Memory^, dest^, dest_stream.Size);

  dest_stream.Free;
  zs.Free;
  src_stream.Free;
end;     }

{ TLzEncoder }

constructor TLzEncoder.Create(const compression_level: byte);
begin
  _ctx := TDc2Encoder.Create(compression_level);
  _encoded_size := 0;
end;

destructor TLzEncoder.Destroy;
begin
  inherited Destroy;
  _ctx.free;
end;

function TLzEncoder.EncodeBytesToStream(const src: pbyte; const size: integer; var dest: TMemoryStream): integer;
var
  src_buffer: pbyte;
  chunk_size: integer;
  bytes_to_process: integer;
  encdata: TEncodedSlice;
  {$ifdef use_zstream}
  zs: Tcompressionstream;
  {$endif}
begin
  {$ifdef use_zstream}
  zs := Tcompressionstream.create(cldefault, dest, true);
  zs.WriteBuffer(src^, size);
  zs.Free;
  result := _encoded_size;
  exit;
  {$endif}
  _encoded_size := 0;
  src_buffer := src;
  chunk_size := MAX_BLOCK_SIZE;
  bytes_to_process := size;

  while bytes_to_process > 0 do begin
      if bytes_to_process <= chunk_size then begin
          chunk_size := bytes_to_process;
          _ctx.SetLastSlice();
      end;

      _ctx.EncodeSlice(src_buffer, chunk_size, encdata);
      dest.Write(encdata.data^, encdata.size);
      _encoded_size += encdata.size;

      src_buffer += chunk_size;
      bytes_to_process -= chunk_size;
  end;

  result := _encoded_size;
end;

procedure TLzEncoder.WriteStats;
begin
  //if _ctx^.stats.onb > 0 then
  //    writeln('avg. offset: ', _ctx^.stats.osum / _ctx^.stats.onb:8:1);
  //if _ctx^.stats.mnb > 0 then
  //    writeln('avg. match : ', _ctx^.stats.msum / _ctx^.stats.mnb:8:2);
  //with _ctx^.stats do
  //    writeln('block types (raw/fix/dyn): ', blocks[0]:6, blocks[1]:6, blocks[2]:6);
  //writeln('deflate bytes: ', _encoded_size);
end;

end.

