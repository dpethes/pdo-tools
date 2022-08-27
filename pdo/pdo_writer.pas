unit pdo_writer;
{$mode objfpc}{$H+}
{ experimental PDO writing }

interface

uses
  Classes, SysUtils,
  pdo_common, tex_storage;

type

  { TPdoStream
    TMemoryStream with some convenience functions
  }

  TPdoStream = class (TMemoryStream)
  private
    _shift: byte;
  public
    procedure SetStringShift(const v: byte);
    procedure WriteQWord(const q: double);
    procedure Write4b(const q: single);
    procedure WritePdoString(const s: string);
    procedure WritePdoString0(const s: string);
    procedure WriteBool(const b: boolean);
    procedure WriteBBox(const bb: TPdoRect);
    procedure PrintPos;
  end;

//Store PDO structure
procedure WritePdo(const pdo: TPdoStructure; const fname: string);


implementation

var
  g_texStorage: TTextureStorage; //well well well...

{ Write PDO v4 or v5 header.
  Disables string shifting for v5 files. One hardcoded unknown value
}
procedure WriteHeader(const f: TPdoStream; const h: TPdoHeader);
const
  NO_SHIFT = true;
var
  shift: integer = 0;
begin
  shift := h.string_shift;
  f.SetStringShift(0);  //first string in v5 is unshifted, in v4 all are unshifted

  f.WriteBuffer(FileMagic[1], Length(FileMagic));
  f.WriteDWord(h.version);
  f.WriteDWord(0);
  f.WriteDWord(300); //unknown
  if h.version > PDO_V4 then begin
      f.WritePdoString0(h.designer_id); //seems that zero strings are only used in header? weird
      if NO_SHIFT then
          shift := 0;
      f.WriteDWord(shift);
      f.SetStringShift(shift);
  end;
  f.WritePdoString0(h.locale);
  f.WritePdoString0(h.codepage);

  { locks, passwords, junk }
  //f.WriteDWord(h.texlock);
  f.WriteDWord(0);
  if h.version = PDO_V6 then begin
      f.WriteByte(h.show_startup_notes);
      f.WriteByte(h.password_flag);
  end;
  f.WritePdoString(h.key);

  if h.version = PDO_V6 then begin
      f.WriteDWord(0);
  end else begin
      if h.version > PDO_V4 then begin
          f.WriteByte(h.show_startup_notes);
          f.WriteByte(h.password_flag);
      end;
  end;
  f.WriteBuffer(h.assembled_height, 8);
  f.WriteBuffer(h.origin_offset, 24);
end;


{ Write single 3D face with its components.
  Uses unknown values
}
procedure WriteFace(const f: TPdoStream; const face: TPdoFace);
const
  FlapFoldInfoLen = 3*4+3*4;
var
  v: TPdoFace2DVertex;
  i: integer;
begin
  f.WriteDWord(face.material_index);
  f.WriteDWord(face.part_index);
  f.WriteBuffer(face.Nx, 3*8);
  f.WriteQWord(face.coord);
  f.WriteDWord(Length(face.vertices));
  for v in face.vertices do begin
      f.WriteDWord(v.id_vertex);
      f.WriteQWord(v.x);
      f.WriteQWord(v.y);
      f.WriteQWord(v.u);
      f.WriteQWord(v.v);
      f.WriteByte(v.flap);
      f.WriteQWord(v.flap_height);
      f.WriteQWord(v.flap_a_angle);
      f.WriteQWord(v.flap_b_angle);
      for i := 0 to FlapFoldInfoLen - 1 do f.WriteByte(0);
  end;
end;

{ Write edges of 3D model
}
procedure WriteEdge(const f: TPdoStream; const e: TPdoEdge);
begin
  f.WriteDWord(e.face1_index);
  f.WriteDWord(e.face2_index);
  f.WriteDWord(e.vertex1index);
  f.WriteDWord(e.vertex2index);
  f.WriteWord(e.connects_faces);
  f.WriteDWord(e.no_connected_face);
end;

{ Write single vertex of 3D model }
procedure WriteVertex(const f: TPdoStream; const v: TPdo3DVertex);
begin
  f.WriteQWord(v.x);
  f.WriteQWord(v.y);
  f.WriteQWord(v.z);
end;

{ Write 3D objects }
procedure WriteObjects(const f: TPdoStream; const objs: array of TPdoObject);
var
  o: TPdoObject;
  face: TPdoFace;
  edge: TPdoEdge;
  vertex: TPdo3DVertex;
begin
  Assert(SizeOf(TPdo3DVertex) = 24, 'TPdo3DVertex size is wrong');
  f.WriteDWord(Length(objs));
  for o in objs do begin
      f.WritePdoString(o.name);
      f.WriteByte(o.visible);

      f.WriteDWord(Length(o.vertices));
      for vertex in o.vertices do
          WriteVertex(f, vertex);

      f.WriteDWord(Length(o.faces));
      for face in o.faces do
          WriteFace(f, face);

      f.WriteDWord(Length(o.edges));
      for edge in o.edges do
          WriteEdge(f, edge);
  end;
end;

{ Write single texture }
procedure WriteTexture(const f: TPdoStream; const t: TPdoTexture);
var
  tex: TTexData;
begin
  tex := g_texStorage.GetTexData(t.texture_id);

  f.WriteDWord(t.width);
  f.WriteDWord(t.height);
  f.WriteDWord(tex.data_size + TEXTURE_DATA_WRAPPER_SIZE);
  f.WriteWord(t.data_header);
  f.WriteBuffer(tex.data^, tex.data_size);
  f.WriteDWord(t.data_hash);
end;

{ Write materials }
procedure WriteMaterials(const f: TPdoStream; const mats: array of TPdoMaterial);
var
  m: TPdoMaterial;
begin
  f.WriteDWord(Length(mats));
  for m in mats do begin
      f.WritePdoString(m.name);
      f.WriteBuffer(m.color3d, 16*4);
      f.Write4b(m.color2d_rgba[3]);
      f.Write4b(m.color2d_rgba[0]);
      f.Write4b(m.color2d_rgba[1]);
      f.Write4b(m.color2d_rgba[2]);

      if m.has_texture then begin
          f.WriteByte(1);
          WriteTexture(f, m.texture);
      end else
          f.WriteByte(0);
  end;
end;

{ Write part structure, including its foldlines }
procedure WritePart(const f: TPdoStream; const p: TPdoPart; const v5_file: boolean);
var
  l: TPdoLine;
begin
  f.WriteDWord(p.object_index);
  f.WriteBBox(p.bounding_box);
  if v5_file then
      f.WritePdoString(p.name);
  f.WriteDWord(Length(p.lines));
  for l in p.lines do begin
      f.WriteBool(l.hidden);
      f.WriteDWord(l.type_);
      f.WriteByte(1);  //padding?
      f.WriteDWord(l.face_index);
      f.WriteDWord(l.vertex_index);
      f.WriteBool(l.is_connecting_faces);
      if l.is_connecting_faces then begin
          f.WriteDWord(l.face2_index);
          f.WriteDWord(l.vertex2_index);
      end;
  end;
end;

procedure WriteText(const f: TPdoStream; const t: TPdoTextBlock);
var
  s: String;
begin
  f.WriteBBox(t.bounding_box);
  f.WriteQWord(t.line_spacing);
  f.WriteDWord(0);
  f.WriteDWord(t.font_size);
  f.WritePdoString(t.font_name);
  f.WriteDWord(Length(t.lines));
  for s in t.lines do
      f.WritePdoString(s);
end;

procedure WriteImage(const f: TPdoStream; const i: TPdoImage);
begin
  f.WriteBBox(i.bounding_box);
  WriteTexture(f, i.texture);
end;

{ Write unfolded data.
  Skips texts and images
}
procedure WriteUnfold(const f: TPdoStream; const pdo: TPdoStructure);
var
  part: TPdoPart;
  image: TPdoImage;
  text: TPdoTextBlock;
begin
  if Length(pdo.parts) = 0 then begin
      f.WriteByte(0);  //no unfold data
      Exit;
  end;
  f.WriteByte(1);
  f.WriteQWord(pdo.unfold.scale);
  f.WriteByte(0);  //padding
  f.WriteBuffer(pdo.unfold.bounding_box, 4*8);
  //parts
  f.WriteDWord(Length(pdo.parts));
  for part in pdo.parts do
      WritePart(f, part, pdo.header.version > PDO_V4);
  //texts
  f.WriteDWord(Length(pdo.textblocks));
  for text in pdo.textblocks do
      WriteText(f, text);
  //images
  f.WriteDWord(Length(pdo.images));
  for image in pdo.images do
      WriteImage(f, image);
  //images under unfold
  f.WriteDWord(0);
end;

{ Write program settings.
  Uses some write combines
}
procedure WriteSettings(const f: TPdoStream; const pdo: TPdoStructure; const s: TPdoSettings);
begin
  if (pdo.header.version = PDO_V6) and (Length(pdo.parts) > 0) then begin
      f.WriteDWord(0);
  end;
  f.WriteByte(s.show_flaps);
  f.WriteByte(s.show_edge_id);
  f.WriteByte(s.edge_id_placement);
  f.WriteByte(s.face_material);

  f.WriteByte(s.hide_almost_flat_fold_lines);
  f.WriteDWord(s.fold_lines_hiding_angle);
  f.WriteByte(s.draw_white_line_under_dot_line);

  f.WriteBuffer(s.mountain_fold_line_style, 4*4);

  //page settings
  f.WriteDWord(s.page.type_);
  if s.page.type_ = PdoPageTypeOther then begin
      f.WriteQWord(s.page.custom_width);
      f.WriteQWord(s.page.custom_height);
  end;
  f.WriteDWord(s.page.orientation);
  f.WriteDWord(s.page.margin_top);
  f.WriteDWord(s.page.margin_side);

  f.WriteBuffer(s.mountain_fold_line_pattern, 6*8*2);
  f.WriteByte(s.add_outline_padding);
  f.WriteQWord(s.scale_factor);
  if pdo.header.version > PDO_V4 then begin
      f.WritePdoString(s.author_name);
      f.WritePdoString(s.comment);
  end;
  f.WriteDWord(FileEndMagic);
end;

procedure WritePdo(const pdo: TPdoStructure; const fname: string);
var
  f: TPdoStream;
begin
  f := TPdoStream.Create;
  g_texStorage := pdo.tex_storage;

  WriteHeader(f, pdo.header);
  WriteObjects(f, pdo.objects);
  WriteMaterials(f, pdo.materials);
  WriteUnfold(f, pdo);
  WriteSettings(f, pdo, pdo.settings);

  f.SaveToFile(fname);
  f.Free;
end;

{ TPdoStream }

procedure TPdoStream.SetStringShift(const v: byte);
begin
  _shift := v;
end;

procedure TPdoStream.WriteQWord(const q: double);
begin
  WriteBuffer(q, 8);
end;

procedure TPdoStream.Write4b(const q: single);
begin
  WriteBuffer(q, 4);
end;

procedure TPdoStream.WritePdoString(const s: string);
var
  i: Integer;
  b: byte;
begin
  WriteDWord(Length(s)+1);
  if _shift = 0 then
      WriteBuffer(s[1], Length(s))
  else begin
      for i := 1 to Length(s) do begin
          b := byte(s[i]) + _shift;
          WriteByte(b);
      end;
  end;
  WriteByte(0);
end;

procedure TPdoStream.WritePdoString0(const s: string);
begin
  if Length(s) > 0 then
      WritePdoString(s)
  else
      WriteDWord(0);
end;

procedure TPdoStream.WriteBool(const b: boolean);
begin
  if b then
      WriteByte(1)
  else
      WriteByte(0);
end;

procedure TPdoStream.WriteBBox(const bb: TPdoRect);
begin
  WriteBuffer(bb, 8*4);
end;

procedure TPdoStream.PrintPos;
begin
  WriteLn(hexStr(Position, 8));
end;

end.

