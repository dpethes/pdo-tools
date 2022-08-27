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

  { TTextureStorage }

  TTextureStorage = class
  private
      textures: array of TTexData;

      procedure Decode(const id: integer);
  public
      constructor Create;
      destructor Destroy; override;

      (* Creates new texture from compressed data and returns its ID.
         Only reference to compressed data is stored, so the data has to remain valid during object usage
         *)
      function Insert(const data: pbyte; const data_size, width, height: integer): integer;

      (* Decodes all compressed data *)
      procedure DecodePixels;

      (* Returns a reference to decoded pixel data for given texture ID *)
      function GetPixels(const texture_id: integer): pbyte;

      //write all textures to file
      procedure DumpTextures(const path: string);
  end;

implementation

uses
  gzip_format, crc;

{ Get the Adler32 checksum stored at the end of zlib wrapper stream }
function GetChecksum(const src: pbyte; const size: integer): longword;
begin
  result := BEtoN(PLongWord(src + size - 4)^);
end;

{ TTextureStorage }

procedure TTextureStorage.Decode(const id: integer);
var
  compressed: pbyte;
  compressed_size: longword;
  decoded_data: pbyte;
begin
  compressed := textures[id].data;
  compressed_size := textures[id].data_size + 4;  //TODO this is just a padding, why?
  decoded_data := textures[id].pixels;
  LzDecodeBytes(compressed, compressed_size, decoded_data);
end;

constructor TTextureStorage.Create;
begin

end;

destructor TTextureStorage.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to Length(textures) - 1 do begin
      freemem(textures[i].pixels);
  end;
end;

{ Insert new texture and return its ID.
  In case of a duplicate, return previous ID
}
function TTextureStorage.Insert(const data: pbyte; const data_size, width, height: integer): integer;
var
  idx: integer;
  tex: TTexData;
  i: Integer;
begin
  tex.data := data;
  tex.data_size := data_size;
  tex.width := width;
  tex.height := height;
  tex.decompressed := false;
  tex.pixels := nil;
  tex.checksum := GetChecksum(data, data_size);

  for i := 0 to Length(textures) - 1 do begin
      if textures[i].checksum = tex.checksum then begin
          result := i;
          exit;
      end;
  end;

  idx := Length(textures);
  SetLength(textures, idx + 1);
  textures[idx] := tex;

  result := idx;
end;

procedure TTextureStorage.DecodePixels;
var
  i: Integer;
  size: integer;
begin
  for i := 0 to Length(textures) - 1 do begin
      size := textures[i].width * textures[i].height * 3;
      textures[i].pixels := Getmem(size);
      Decode(i);
      textures[i].decompressed := true;
  end;
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

{
  TTextureStorage.GetPixels
  Returns decoded pixels if the texture was already decoded, otherwise runs decoding first.
}
function TTextureStorage.GetPixels(const texture_id: integer): pbyte;
var
  tex: TTexData;
  size: integer;
begin
  Assert(texture_id >= 0);
  tex := textures[texture_id];
  if not tex.decompressed then begin
      size := tex.width * tex.height * 3;
      tex.pixels := Getmem(size);
      Decode(texture_id);

      //store back
      textures[texture_id].pixels := tex.pixels;
      textures[texture_id].decompressed := true;
  end;
  Result := tex.pixels;
end;

end.

