unit pdo_util;
{$mode objfpc}{$H+}
{ Utility and debugging functions
}
{$define DEBUG_INFO}

interface

uses
  Classes, SysUtils, LConvEncoding, contnrs, math,
  pdo_common;

{
  Convert PDO strings to UTF-8 encoding.
  Codepage 'Shift-JIS' means conversion from CP932,
  'us-ascii' codepage is unreliable and we have to guess the encoding, which is unreliable too.
  Encountered files with:
    - ASCII
    - CP932 - japanese
    - CP1251 - russian
    - unknown
}
function PdoStringsToUtf8(const pdo: TPdoStructure; const codepage: string): TFPStringHashTable;

{ Returns page dimensions in mm for given PDO page type }
procedure GetPageSizeForPageType(const page_type: integer; out w, h: single);

procedure WriteHeaderInfo(const header: TPdoHeader);
procedure WriteObjectsInfo(const objects: array of TPdoObject);
procedure WriteMaterialsInfo(const materials: array of TPdoMaterial);
procedure WriteRect(const r: TPdoRect);
procedure WriteUnfoldParts(const objects: array of TPdoObject; const parts: array of TPdoPart);
procedure WriteTexts(const textblocks: array of TPdoTextBlock);
procedure WriteImages(const images: array of TPdoImage);
procedure WriteSettingsInfo(const settings: TPdoSettings);

implementation

function PdoStringsToUtf8(const pdo: TPdoStructure; const codepage: string): TFPStringHashTable;
var
  encoding: string;
  map: TFPStringHashTable;

  procedure ToUtf8(const s: string); inline;
  var
    converted: boolean;
  begin
    map[s] := ConvertEncodingToUTF8(s, encoding, converted);
  end;

var
  i: integer;
  s: string;
begin
  encoding := codepage;
  if codepage = CodepageShiftJIS then
      encoding := EncodingCP932;

  map := TFPStringHashTable.Create;
  ToUtf8(pdo.settings.author_name);
  ToUtf8(pdo.settings.comment);
  for i := 0 to Length(pdo.textblocks) - 1 do
      for s in pdo.textblocks[i].lines do
          ToUtf8(s);
  result := map;
end;

procedure GetPageSizeForPageType(const page_type: integer; out w, h: single);
const
  //'A4', 'A3', 'A2', 'A1', 'B5', 'B4', 'B3', 'B2', 'B1', 'letter', 'legal', 'other');
  PageWidths: array[0..10] of single =
    (210, 297, 420, 594, {Bs} 176, 250, 353, 500, 707, {US} 215.9, 215.9);
  PageHeights: array[0..10] of single =
    (297, 420, 594, 841, {Bs} 250, 353, 500, 707, 1000, {US} 279.4, 355.6);
begin
  w := 0;
  h := 0;
  if page_type < PdoPageTypeOther then begin
      w := PageWidths[page_type];
      h := PageHeights[page_type];
  end;
end;


{$ifdef DEBUG_INFO}
procedure WriteHeaderInfo(const header: TPdoHeader);
begin
  writeln('minor version: ', header.version);
  writeln('string shift: ', header.string_shift);
  writeln('locale: ', header.locale);
  writeln('codepage: ', header.codepage);
  writeln('texture lock: ', header.texlock);
  writeln('key: ', header.key);
  if header.version > PDO_V4 then begin
      writeln('show author and comment at startup: ', header.show_startup_notes);
      Writeln('password_flag: ', header.password_flag);
  end;
  writeln('assembled height: ', header.assembled_height:7:3);
  writeln('x offset', header.origin_offset[0]:10:5);
  writeln('y offset', header.origin_offset[1]:10:5);
  writeln('z offset', header.origin_offset[2]:10:5);
end;


procedure WriteObjectsInfo(const objects: array of TPdoObject);
var
  i, k: integer;
  obj: TPdoObject;
  face: TPdoFace;
  vert: TPdoFace2DVertex;
  vert3d: TPdo3DVertex;
  edge: TPdoEdge;
begin
  writeln('Objects: ', length(objects));
  for obj in objects do begin
      writeln('name: ' + obj.name);

      writeln('3d vertices: ', length(obj.vertices));
      for i := 0 to length(obj.vertices) - 1 do begin
          vert3d := obj.vertices[i];
          writeln('  id: ', i:4, '  x,y,z: ', vert3d.x:10:6, ' , ', vert3d.y:10:6, ' , ', vert3d.z:10:6);
      end;

      writeln('faces: ', length(obj.faces));
      i := 0;
      for face in obj.faces do begin
          writeln('face: ', i);
          i += 1;
          if face.material_index = -1 then
              writeln('no material')
          else
              writeln ('material index: ', face.material_index);
          writeln('part index: ', face.part_index);
          writeln('vertices: ', length(face.vertices));
          for k := 0 to length(face.vertices) - 1 do begin
              vert := face.vertices[k];
              write('  id_vertex: ', vert.id_vertex:4);
              if vert.flap = 1 then
                  writeln('  flap: ', vert.flap, ' height: ', vert.flap_height:4:1,
                             ' a|b: ', radtodeg(vert.flap_a_angle):5:3, '|',
                                       radtodeg(vert.flap_b_angle):5:3);
              writeln('  x,y (u,v): ', vert.x:10:6, ' , ', vert.y:10:6, ' (', vert.u:10:6, ', ', vert.v:10:6, ')');
          end;
      end;

      writeln('edges: ', length(obj.edges));
      for edge in obj.edges do begin
          write('edge faces: ', edge.face1_index, ', ', edge.face2_index);
          write(' vertices: ', edge.vertex1index, ' - ', edge.vertex2index);
          if edge.no_connected_face = 1 then
              write(' one-sided');
          if edge.connects_faces = 1 then
              write(' two-sided');
          writeln(' (', edge.no_connected_face, '/', edge.connects_faces, ')');
      end;
  end;
end;


procedure WriteMaterialsInfo(const materials: array of TPdoMaterial);
var
  mat: TPdoMaterial;
  i: integer;
begin
  writeln('Materials: ', length(materials));
  for mat in materials do begin
      writeln('material: ', mat.name);
      writeln('  texture: ', mat.has_texture);
      write('  color: ');
      for i := 0 to 3 do
          write(mat.color2d_rgba[i]:10:5);
      writeln;
  end;
end;


procedure WriteRect(const r: TPdoRect);
begin
  write('top, left: ', r.top:8:4, ' ', r.left:8:4);
  writeln('  WxH: ', r.width:8:4, ' ', r.height:8:4);
end;


procedure WriteUnfoldParts(const objects: array of TPdoObject; const parts: array of TPdoPart);
var
  part: TPdoPart;
  line: TPdoLine;
begin
  for part in parts do begin
      writeln('part name: ', part.name);
      writeln('part object: ', part.object_index);
      write('  part bounding box (mm): ');
      WriteRect(part.bounding_box);
      if part.object_index < Length(objects) then
          //Error('part links to invalid object')
      else
          Writeln('  linked object: ', objects[part.object_index].name);

      writeln('  lines count: ', Length(part.lines));
      for line in part.lines do begin
          write('line: ');
          write('(t=', line.type_, ')');
          Write(' face idx: ', line.face_index);
          Write(' vertex idx: ', line.vertex_index);

          if line.is_connecting_faces then begin
              Write(' --> ');
              Write(' face idx: ', line.face2_index);
              Write(' vertex idx: ', line.vertex2_index);

              //if (objects[part.object_index].edges[line.vertex_index].connects_faces = 1)
              //  or (objects[part.object_index].edges[line.vertex2_index].connects_faces = 1) then
              //    write(' (double edge) ');
          end;
          if line.hidden then
              write(' (hidden)');
          writeln;
      end;
  end;
end;

procedure WriteTexts(const textblocks: array of TPdoTextBlock);
var
  textblock: TPdoTextBlock;
  textline: string;
begin
  writeln('text blocks: ', Length(textblocks));
  for textblock in textblocks do begin
      write('textblock font: ' + textblock.font_name + ', size: ', textblock.font_size);
      write('  bounding box (mm): ');
      WriteRect(textblock.bounding_box);
      write('  text: ');
      for textline in textblock.lines do
          writeln(textline);
  end;
end;


procedure WriteImages(const images: array of TPdoImage);
var
  img: TPdoImage;
begin
  writeln('images: ', Length(images));
  for img in images do begin
      write('  bounding box (mm): ');
      WriteRect(img.bounding_box);
      writeln;
  end;
end;


procedure WriteSettingsInfo(const settings: TPdoSettings);
begin
  writeln('page type: ', PdoPageTypes[settings.page.type_]);
  writeln('horizontal margin: ', settings.page.margin_side);
  writeln('vertical margin:   ', settings.page.margin_top);
  writeln('author: ', settings.author_name);
  writeln('comment: ', settings.comment);
  writeln('hide fold lines: ', settings.hide_almost_flat_fold_lines);
  writeln('hiding angle: ', settings.fold_lines_hiding_angle);
end;
{$else}
procedure WriteHeaderInfo(const header: TPdoHeader); begin end;
procedure WriteObjectsInfo(const objects: array of TPdoObject); begin end;
procedure WriteMaterialsInfo(const materials: array of TPdoMaterial); begin end;
procedure WriteRect(const r: TPdoRect); begin end;
procedure WriteUnfoldParts(const objects: array of TPdoObject; const parts: array of TPdoPart); begin end;
procedure WriteTexts(const textblocks: array of TPdoTextBlock); begin end;
procedure WriteImages(const images: array of TPdoImage); begin end;
procedure WriteSettingsInfo(const settings: TPdoSettings); begin end;
{$endif}

end.

