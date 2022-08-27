unit tex_storage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dc2_simple_api, utils;

type
  TTexData = record
      width: integer;   //texture width in pixels
      height: integer;  //texture height in pixels
      data: pbyte;      //compressed texture data
      data_size: integer;  //compressed data size
      pixels: pbyte;       //decoded RGB pixels
      checksum: longword;  //texture checksum taken from PDO data
      decompressed: boolean;  //texture data already decompressed flag
  end;
  PTexData = ^TTexData;

  { TTextureStorage }

  TTextureStorage = class
  private
      textures: array of TTexData;
      _current_alloc_tex: TTexData;
      _allocated: longword;

      procedure Decode(var tex: TTexData);
  public
      constructor Create;
      destructor Destroy; override;

      //Allocate new texture memory
      function Alloc(const data_size, width, height: integer): pbyte;

      (* Store new texture and return its ID. *)
      function Insert(const checksum: longword): longword;

      (* Decodes all compressed data *)
      procedure DecodePixels;

      (* Returns a reference to decoded pixel data for given texture ID *)
      function GetPixels(const texture_id: longword): pbyte;

      (* Returns a reference to allocated memory for compressed data of given texture ID *)
      function GetTexData(const texture_id: longword): TTexData;

      //write all textures to file
      procedure DumpTextures(const path: string);

      procedure Recompress;
  end;

implementation

uses
  gzip_format, crc;

const
  DECODE_PADDING = 4;

{ TTextureStorage }

procedure TTextureStorage.Decode(var tex: TTexData);
var
  size, compressed_size: longword;
begin
  if tex.decompressed then
      exit;

  size := tex.width * tex.height * 3;
  tex.pixels := Getmem(size);

  compressed_size := tex.data_size + DECODE_PADDING;  //TODO why is this padded?
  LzDecodeBytes(tex.data, compressed_size, tex.pixels);
  tex.decompressed := true;
end;

constructor TTextureStorage.Create;
begin
  _allocated := 0;
end;

destructor TTextureStorage.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to Length(textures) - 1 do begin
      freemem(textures[i].data);
      freemem(textures[i].pixels);
  end;
end;

{ Allocate memory for a new texture }
function TTextureStorage.Alloc(const data_size, width, height: integer): pbyte;
begin
  _current_alloc_tex.data := GetMem(data_size + DECODE_PADDING);  //TODO why padding?
  _current_alloc_tex.data_size := data_size;
  _current_alloc_tex.width := width;
  _current_alloc_tex.height := height;

  result := _current_alloc_tex.data;
  _allocated += 1;
end;

{ Insert new texture and return its ID.
  In case of a duplicate, return previous ID
}
function TTextureStorage.Insert(const checksum: longword): longword;
var
  idx, i: integer;
  t: TTexData;
begin
  Assert(_current_alloc_tex.data <> nil, 'No texture allocated');
  t.data      := _current_alloc_tex.data;
  t.data_size := _current_alloc_tex.data_size;
  t.width     := _current_alloc_tex.width;
  t.height    := _current_alloc_tex.height;
  t.decompressed := false;
  t.pixels := nil;
  t.checksum := BEtoN(checksum); //probably don't need to bswap it

  _current_alloc_tex.data := nil;

  for i := 0 to Length(textures) - 1 do begin
      if textures[i].checksum = t.checksum then begin
          Freemem(t.data);  //we won't need the data anymore
          result := i;
          exit;
      end;
  end;

  idx := Length(textures);
  SetLength(textures, idx + 1);
  textures[idx] := t;

  result := idx;
end;

procedure TTextureStorage.DecodePixels;
var
  i: Integer;
begin
  for i := 0 to Length(textures) - 1 do begin
      Decode(textures[i]);
  end;
  //writeln('allocated ', _allocated, ', used ', Length(textures));
end;

procedure TTextureStorage.DumpTextures(const path: string);
var
  i: Integer;
  name: string;

  procedure DumpCompressedTex(const texture: TTexData; const tex_dump_file_name: string);
  var
    checksum: longword;
    raw_size: integer;
    gz_dump: file;
  begin
    AssignFile(gz_dump, tex_dump_file_name + '.gz');
    Rewrite(gz_dump, 1);

    gzf_write_header(gz_dump, tex_dump_file_name + '.raw');
    blockwrite(gz_dump, (texture.data + 2)^, texture.data_size - 6);
    raw_size := texture.width * texture.height * 3;
    checksum := crc32(0, nil, 0);
    checksum := crc32(checksum, texture.pixels, raw_size);
    gzf_write_end(gz_dump, raw_size, checksum);

    CloseFile(gz_dump);
  end;

begin
  if (textures = nil) or (Length(textures) = 0) then
      exit;
  if not textures[0].decompressed then
      DecodePixels;
  for i := 0 to Length(textures) - 1 do begin
      name := path + 'tex' + IntToStr(i) + '.pnm';
      PnmSave(name, textures[i].pixels, textures[i].width, textures[i].height);
      DumpCompressedTex(textures[i], path + 'tex_compressed' + IntToStr(i));
  end;
end;

procedure TTextureStorage.Recompress;
var
  size: longword;
  encoder: TLzEncoder;
  ms: TMemoryStream;
  reenc_size: Int64;
  tex: PTexData;
  i: Integer;
begin
  for i := 0 to Length(textures) - 1 do begin
      encoder := TLzEncoder.Create(9);
      ms := TMemoryStream.Create;

      tex := @textures[i];
      size := tex^.width * tex^.height * 3;
      encoder.EncodeBytesToStream(tex^.pixels, size, ms);
      reenc_size := ms.Position;
      if reenc_size < tex^.data_size then begin
          Move(ms.Memory^, tex^.data^, reenc_size);
          tex^.data_size := reenc_size;
      end;

      ms.Free;
      encoder.Free;
  end;
end;

{
  TTextureStorage.GetPixels
  Returns decoded pixels if the texture was already decoded, otherwise runs decoding first.
}
function TTextureStorage.GetPixels(const texture_id: longword): pbyte;
var
  tex: PTexData;
begin
  Assert(texture_id < Length(textures));
  tex := @textures[texture_id];
  if not tex^.decompressed then
      Decode(tex^);
  Result := tex^.pixels;
end;

function TTextureStorage.GetTexData(const texture_id: longword): TTexData;
begin
  Assert(texture_id < Length(textures));
  result := textures[texture_id];
end;

end.

