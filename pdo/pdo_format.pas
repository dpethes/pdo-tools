unit pdo_format;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pdo_common, pdo_util, tex_storage;

type

  { TPdoStream }

  TPdoStream = class (TMemoryStream)
  public
    StringShift: byte;
    multiByteC: boolean;
    constructor Create;
    procedure ReadBytes(out buffer; count: integer);
    function ReadString(const shift: byte = 0): string;
    function ReadShiftedString: string;
  end;

  { TPdoParser }

  TPdoParser = class
    public
      constructor Create(const log_to_console: boolean = false);
      destructor Destroy; override;

      procedure OpenFile(input_file: string);
      procedure Load;
      procedure Error(s: string); virtual;  //error handler
      function GetStructure: TPdoStructure;

    private
      enable_console_log: boolean;
      fpdo: TPdoStream;
      header: TPdoHeader;
      objects: array of TPdoObject;
      materials: array of TPdoMaterial;
      textblocks: array of TPdoTextBlock;
      parts: array of TPdoPart;
      images: array of TPdoImage;
      settings: TPdoSettings;
      unfold: TPdoUnfold;
      tex_storage: TTextureStorage;

      procedure ReadHeader;
      procedure ReadObjects;
      procedure ReadMaterials;
      procedure ReadUnfoldData;
      procedure ReadSettings;

      function ReadEdge(var f: TPdoStream): TPdoEdge;
      procedure ReadMajorVersion(var f:TPdoStream);
      function ReadTextBlock(var f:TPdoStream):TPdoTextBlock;
      procedure ReadTextBlocks;
      procedure ReadImages;
      function ReadLine(var f:TPdoStream):TPdoLine;
      function ReadPart(var f:TPdoStream):TPdoPart;
      procedure ReadParts;
      function ReadFace(var f:TPdoStream): TPdoFace;
      function Read2DVertex(var f:TPdoStream): TPdoFace2DVertex;
      function ReadMaterial(var f:TPdoStream): TPdoMaterial;
      function ReadObject(var f:TPdoStream): TPdoObject;
      function ReadTexture(var f:TPdoStream): TPdoTexture;
  end;


{**************************************************************************************************}
implementation

const
  DECODE_PADDING = 4;

{ TPdoStream }

constructor TPdoStream.Create;
begin
  StringShift := 0;
  multiByteC := false;
end;

procedure TPdoStream.ReadBytes(out buffer; count: integer);
begin
  ReadBuffer(buffer, count);
end;

{ Read single byte or multi byte strings. Exports do their own codepage conversions
  and during pdo processing we don't touch the strings, but we should probably
  use RawByteString anyway
}
function TPdoStream.ReadString(const shift: byte = 0): string;
var
  len: integer;
  b: byte;
  w: word;
  i: integer;
  ucs: UnicodeString;
  utf8s: UTF8String;
begin
  result := '';
  ReadBytes(len, 4);
  if len = 0 then
      exit;
  if multiByteC then begin
      len := len div 2;
      ucs := '';
      for i := 1 to len - 1 do begin
          ReadBytes(w, 2);
          //w := w and $ff;
          ucs += widechar((w - shift) and $ff);
      end;
      ReadBytes(w, 2);
      utf8s := ucs;
      result := utf8s;
  end else begin
      for i := 1 to len - 1 do begin
          ReadBytes(b, 1);
          result += char(b - shift);
      end;
      ReadBytes(b, 1);
  end;
  //result := '(' + IntToStr(len) + ') ' + result;  //for checking string's lengths in dump / text output
end;

function TPdoStream.ReadShiftedString: string;
begin
  result := ReadString(StringShift);
end;


constructor TPdoParser.Create(const log_to_console: boolean);
begin
  enable_console_log := log_to_console;
  tex_storage := TTextureStorage.Create;
end;

destructor TPdoParser.Destroy;
var
  i, k: integer;
begin
  inherited Destroy;

  //free texture and image data
  for i := 0 to Length(materials) - 1 do
      if materials[i].has_texture then
          Freemem(materials[i].texture.data);
  materials := nil;
  for i := 0 to Length(images) - 1 do
      Freemem(images[i].texture.data);
  images := nil;
  tex_storage.Free;

  for i := 0 to Length(objects) - 1 do begin
      for k := 0 to Length(objects[i].faces) - 1 do
          objects[i].faces[k].vertices := nil;
      objects[i].faces := nil;
      objects[i].vertices := nil;
  end;
  objects := nil;

  for i := 0 to Length(parts) - 1 do
      parts[i].lines := nil;
  parts := nil;

  for i := 0 to Length(textblocks) - 1 do
      textblocks[i].lines := nil;
  textblocks := nil;
end;

procedure TPdoParser.OpenFile(input_file: string);
begin
  fpdo := TPdoStream.Create;
  fpdo.LoadFromFile(input_file);
end;

procedure TPdoParser.Error(s:string);
begin
  writeln(s);
  halt;
end;

function TPdoParser.GetStructure:TPdoStructure;
begin
  Result.header := header;
  Result.objects := objects;
  Result.materials := materials;
  Result.parts := parts;
  Result.textblocks := textblocks;
  Result.images := images;
  Result.settings := settings;
  Result.unfold := unfold;
  Result.tex_storage := tex_storage;
end;

procedure TPdoParser.Load;
begin
  ReadHeader;
  ReadObjects;
  ReadMaterials;
  ReadUnfoldData;
  ReadSettings;
  fpdo.Free;
  tex_storage.DecodePixels;
end;


procedure TPdoParser.ReadMajorVersion(var f: TPdoStream);
var
  i: integer;
  s: string;
  c: char;
begin
  s := '';
  for i := 1 to length(FileMagic) do begin
      f.ReadBytes(c, 1);
      s += c;
  end;
  if s <> FileMagic then Error('this isn''t a version 3 file');
end;


procedure TPdoParser.ReadHeader;
var
  unknown_int: integer;
  i: word;
  junk: array[0..15] of byte;
  peeksize: Byte;
begin
  header.string_shift := 0;
  ReadMajorVersion(fpdo);
  fpdo.ReadBytes(header.version, 4);
  fpdo.ReadBytes(header.multi_byte_chars, 4);
  fpdo.multiByteC := header.multi_byte_chars = 1;

  fpdo.ReadBytes(unknown_int, 4); //310 in p4file, 490 in p5file
  if header.version > PDO_V4 then begin
      header.designer_id := fpdo.ReadString;
      fpdo.ReadBytes(header.string_shift, 4);
      fpdo.StringShift := header.string_shift;
  end;
  header.locale   := fpdo.ReadShiftedString;
  header.codepage := fpdo.ReadShiftedString;

  { locks, passwords, junk }
  fpdo.ReadBytes(header.texlock, 4);
  if header.version = PDO_V6 then begin
      fpdo.ReadBytes(header.show_startup_notes, 1);
      fpdo.ReadBytes(header.password_flag, 1);
  end;

  { check for dodgy files:
    - old v2 format file imported into Pepakura designer 4 (which messed up the objects' names), then saved as newer format
    - I haven't seen this kind of file in the wild, so just assert for now
  }
  peeksize := pbyte(fpdo.Memory)[fpdo.Position];
  Assert((fpdo.multiByteC and (peeksize = 66)) or (not fpdo.multiByteC and (peeksize = 33)), 'header string error');
  //fix for testing
  //if fpdo.multiByteC and (peeksize = 33) then
  //    fpdo.multiByteC := false;

  header.key := fpdo.ReadShiftedString;

  if header.version = PDO_V6 then begin
      fpdo.ReadBytes(header.v6_lock, 4);
      if header.v6_lock > 0 then begin    //set in password locked files?
          for i := 0 to header.v6_lock - 1 do
              fpdo.ReadBytes(junk, 8);
      end;
  end
  else begin
      if header.version > PDO_V4 then begin
          fpdo.ReadBytes(header.show_startup_notes, 1);
          fpdo.ReadBytes(header.password_flag, 1);
      end;
  end;
  Assert(header.show_startup_notes <= 1);
  Assert(header.password_flag <= 1);

  { model parameters }
  fpdo.ReadBytes(header.assembled_height, 8);
  fpdo.ReadBytes(header.origin_offset, 24);

  if not enable_console_log then exit;
  WriteHeaderInfo(header);
end;


function TPdoParser.Read2DVertex(var f:TPdoStream): TPdoFace2DVertex;
const
  FlapFoldInfoLen = 3*4+3*4;
var
  flap_fold_info: array[0..FlapFoldInfoLen] of byte;
begin
  f.ReadBytes(result.id_vertex, 4);
  f.ReadBytes(result.x, 8);
  f.ReadBytes(result.y, 8);
  f.ReadBytes(result.u, 8);
  f.ReadBytes(result.v, 8);
  f.ReadBytes(result.flap, 1);
  f.ReadBytes(result.flap_height, 8);
  f.ReadBytes(result.flap_a_angle, 8);
  f.ReadBytes(result.flap_b_angle, 8);
  f.ReadBytes(flap_fold_info, FlapFoldInfoLen);
end;


function TPdoParser.ReadFace(var f:TPdoStream):TPdoFace;
var
  face_verts_count: integer;
  i: integer;
begin
  f.ReadBytes(result.material_index, 4);
  f.ReadBytes(result.part_index, 4);
  f.ReadBytes(result.Nx, 8);
  f.ReadBytes(result.Ny, 8);
  f.ReadBytes(result.Nz, 8);
  f.ReadBytes(result.coord, 8);
  f.ReadBytes(face_verts_count, 4);
  SetLength(result.vertices, face_verts_count);
  for i := 0 to face_verts_count - 1 do
      result.vertices[i] := Read2DVertex(f);
end;

function TPdoParser.ReadEdge(var f: TPdoStream): TPdoEdge;
begin
  f.ReadBytes(Result, 22);
end;


function TPdoParser.ReadObject(var f:TPdoStream): TPdoObject;
var
  i, numvertices, numfaces, numedges: integer;
begin
  result.name := f.ReadShiftedString;
  f.ReadBytes(result.visible, 1);
  Assert(result.visible <= 1);

  f.ReadBytes(numvertices, 4);
  SetLength(result.vertices, numvertices);
  for i := 0 to numvertices - 1 do begin
      f.ReadBytes(result.vertices[i].x, 8);
      f.ReadBytes(result.vertices[i].y, 8);
      f.ReadBytes(result.vertices[i].z, 8);
  end;

  f.ReadBytes(numfaces, 4);
  SetLength(result.faces, numfaces);
  for i := 0 to numfaces - 1 do begin
      result.faces[i] := ReadFace(f);
  end;

  f.ReadBytes(numedges, 4);
  SetLength(result.edges, numedges);
  for i := 0 to numedges - 1 do begin
      result.edges[i] := ReadEdge(f);
  end;
end;


procedure TPdoParser.ReadObjects;
var
  i: integer;
  objnum: integer;
begin
  fpdo.ReadBytes(objnum, 4);
  SetLength(objects, objnum);
  for i := 0 to objnum - 1 do begin
      objects[i] := ReadObject(fpdo);
  end;

  if not enable_console_log then exit;
  WriteObjectsInfo(objects);
end;


function TPdoParser.ReadTexture(var f:TPdoStream): TPdoTexture;
begin
  f.ReadBytes(result.width, 4);
  f.ReadBytes(result.height, 4);
  f.ReadBytes(result.data_size, 4);

  //padded to mod4 because of bitstream 4B buffer used during decoding
  result.data := getmem(result.data_size + DECODE_PADDING);

  f.ReadBytes(result.data^, result.data_size);
  result.texture_id := tex_storage.Insert(result.data, result.data_size, result.width, result.height);
end;


function TPdoParser.ReadMaterial(var f:TPdoStream):TPdoMaterial;
var
  texture_flag: byte;
begin
  result.name := f.ReadShiftedString;

  //3D colors
  f.ReadBytes(result.color3d, 16*4);

  //2D color
  f.ReadBytes(result.color2d_rgba[3], 4);
  f.ReadBytes(result.color2d_rgba[0], 4);
  f.ReadBytes(result.color2d_rgba[1], 4);
  f.ReadBytes(result.color2d_rgba[2], 4);

  //texture
  f.ReadBytes(texture_flag, 1);
  result.has_texture := texture_flag = 1;
  if result.has_texture then
      result.texture := ReadTexture(f)
  else begin
      result.texture.data := nil;
      result.texture.data_size := 0;
      result.texture.texture_id := -1;
  end;
end;


procedure TPdoParser.ReadMaterials;
var
  i: integer;
  matnum: integer;
begin
  fpdo.ReadBytes(matnum, 4);
  SetLength(materials, matnum);
  for i := 0 to Length(materials) - 1 do begin
      materials[i] := ReadMaterial(fpdo);
      if materials[i].name = '' then
          materials[i].name := 'named_material' + IntToStr(i);
  end;
  if not enable_console_log then exit;
  WriteMaterialsInfo(materials);
end;


function TPdoParser.ReadTextBlock(var f:TPdoStream):TPdoTextBlock;
var
  i: integer;
  lines_count: integer;
  color: integer;
begin
  f.ReadBytes(Result.bounding_box, SizeOf(TPdoRect));
  f.ReadBytes(Result.line_spacing, 8);
  f.ReadBytes(color, 4);   //text color - RGB0
  f.ReadBytes(Result.font_size, 4);
  Result.font_name := f.ReadShiftedString;

  f.ReadBytes(lines_count, 4);
  SetLength(result.lines, lines_count);
  for i := 0 to lines_count - 1 do
      Result.lines[i] := f.ReadShiftedString;
end;


procedure TPdoParser.ReadTextBlocks;
var
  text_block_count: integer;
  i: integer;
begin
  fpdo.ReadBytes(text_block_count, 4);
  SetLength(textblocks, text_block_count);
  for i := 0 to text_block_count - 1 do
      textblocks[i] := ReadTextBlock(fpdo);
end;

{ Read images. Pixel data is stored in the same format as material textures.
  Images can be split between two blocks
}
procedure TPdoParser.ReadImages;
var
  img_count, additional_img_count: integer;
  i: integer;
  img: TPdoImage;
begin
  fpdo.ReadBytes(img_count, 4);
  SetLength(images, img_count);
  for i := 0 to img_count - 1 do begin
      fpdo.ReadBytes(img.bounding_box, SizeOf(TPdoRect));
      img.texture := ReadTexture(fpdo);
      images[i] := img;
  end;
  //additional image block
  fpdo.ReadBytes(additional_img_count, 4);
  SetLength(images, Length(images) + additional_img_count);
  for i := 0 to additional_img_count - 1 do begin
      fpdo.ReadBytes(img.bounding_box, SizeOf(TPdoRect));
      img.texture := ReadTexture(fpdo);
      images[img_count + i] := img;
  end;
end;


function TPdoParser.ReadLine(var f: TPdoStream): TPdoLine;
var
  unknown_byte: byte;
  is_hidden: byte;
  second_index: byte;
begin
  f.ReadBytes(is_hidden, 1);
  Result.hidden := is_hidden = 1;
  f.ReadBytes(Result.type_, 4);  //Write(unknown_int, ' ');
  f.ReadBytes(unknown_byte, 1);  //Write(unknown_byte, ' ');
  f.ReadBytes(Result.face_index, 4);    //Write(' face idx: ', Result.face_index);
  f.ReadBytes(Result.vertex_index, 4);  //Write(' id_vertex: ', Result.vertex_index);
  f.ReadBytes(second_index, 1);
  Result.is_connecting_faces := second_index = 1;
  if Result.is_connecting_faces then begin
      f.ReadBytes(Result.face2_index, 4);    //Write(' face idx: ', Result.face2_index);
      f.ReadBytes(Result.vertex2_index, 4);  //Write(' id_vertex: ', Result.vertex2_index);
  end;
end;


function TPdoParser.ReadPart(var f:TPdoStream):TPdoPart;
var
  lines_count: integer;
  i: integer;
begin
  f.ReadBytes(result.object_index, 4);
  f.ReadBytes(result.bounding_box, SizeOf(result.bounding_box));
  if header.version > PDO_V4 then begin
      Result.name := fpdo.ReadShiftedString;
  end;
  if Result.object_index >= Length(objects) then
      Error('part links to invalid object');

  fpdo.ReadBytes(lines_count, 4);
  SetLength(Result.lines, lines_count);
  for i := 0 to lines_count - 1 do begin
      Result.lines[i] := ReadLine(f);
  end;
end;


procedure TPdoParser.ReadParts;
var
  part_count: integer;
  i: integer;
begin
  fpdo.ReadBytes(part_count, 4);
  SetLength(parts, part_count);
  for i := 0 to Length(parts) - 1 do begin
      parts[i] := ReadPart(fpdo);
  end;
end;


procedure TPdoParser.ReadUnfoldData;
var
  has_unfold: byte;
  padding: byte;
begin
  fpdo.ReadBytes(has_unfold, 1);
  if has_unfold = 0 then begin
      exit;
  end;

  fpdo.ReadBytes(unfold.scale, 8);
  fpdo.ReadBytes(padding, 1);  //WriteLn(padding);
  fpdo.ReadBytes(unfold.bounding_box, 4*8);

  ReadParts;
  ReadTextBlocks;
  ReadImages;

  if not enable_console_log then exit;
  writeln('unfold scale: ', unfold.scale:7:3);
  write('page parts bounding box (mm): ');
  WriteRect(unfold.bounding_box);
  WriteUnfoldParts(objects, parts);
  WriteTexts(textblocks);
  WriteImages(images);
end;

{
  Read misc. settings related to pattern presentation.
  Unluckily the structure has variable size, so we can't read it whole in one go.
}
procedure TPdoParser.ReadSettings;

  procedure ReadUnknown;
  var
    buf: array[byte] of Int32;
    unknown_item_parts: integer;
    unknown_items_count: integer;
    i: integer;
  begin
    fpdo.ReadBytes(unknown_items_count, 4);
    for i := 0 to unknown_items_count - 1 do begin
        fpdo.ReadBytes(unknown_item_parts, 4);
        FillByte(buf, sizeof(buf), 0);
        fpdo.ReadBytes(buf, 4 * unknown_item_parts);
    end;
  end;

begin
  if (header.version = PDO_V6) and (Length(parts) > 0) then
      ReadUnknown;
  fpdo.ReadBytes(settings.show_flaps, 1);
  fpdo.ReadBytes(settings.show_edge_id, 1);
  fpdo.ReadBytes(settings.edge_id_placement, 1);
  fpdo.ReadBytes(settings.face_material, 1);

  fpdo.ReadBytes(settings.hide_almost_flat_fold_lines, 1);
  fpdo.ReadBytes(settings.fold_lines_hiding_angle, 4);

  fpdo.ReadBytes(settings.draw_white_line_under_dot_line, 1);
  fpdo.ReadBytes((@settings.mountain_fold_line_style)^, 4*4);

  //page settings
  fpdo.ReadBytes(settings.page.type_, 4);
  Assert(settings.page.type_ <= PdoPageTypeOther, 'invalid page type');
  if settings.page.type_ = PdoPageTypeOther then begin
      fpdo.ReadBytes(settings.page.custom_width, 8);
      fpdo.ReadBytes(settings.page.custom_height, 8);
  end;
  fpdo.ReadBytes(settings.page.orientation, 4);
  fpdo.ReadBytes(settings.page.margin_side, 4);
  fpdo.ReadBytes(settings.page.margin_top, 4);

  //fold line patterns
  fpdo.ReadBytes((@settings.mountain_fold_line_pattern)^, 6*8*2);

  fpdo.ReadBytes(settings.add_outline_padding, 1);
  fpdo.ReadBytes(settings.scale_factor, 8);
  if header.version > PDO_V4 then begin
      settings.author_name := fpdo.ReadShiftedString;
      settings.comment := fpdo.ReadShiftedString;
  end;
  if not enable_console_log then exit;
  WriteSettingsInfo(settings);
end;

end.

