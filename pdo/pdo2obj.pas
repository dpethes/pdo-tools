unit pdo2obj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, crc,
  pdo_common, utils;

type
  TObjExportOptions = record
      normalize: boolean;
      flip_v_coordinate: boolean;
  end;

//Writes 3d object to Wavefront obj format.
procedure WriteObj(const pdo: TPdoStructure; const obj_name: string; const opts: TObjExportOptions);

implementation

const
  HeaderComment = 'Created with PdoTools';
  DefaultMaterial = 'default';

function GetHash(buffer: pbyte; length: longword): longword;
begin
  result := crc32(0, nil, 0);
  result := crc32(result, buffer, length);
end;

{
  StoreTexture
  Save texture in TGA format: needs to swap pixels from RGB to BGR order that TGA uses.
}
procedure StoreTexture(const fname: string; const pixdata: pbyte; const w, h: integer);
var
  i: integer;
  t: byte;
  size: integer;
  pixels: pbyte;
begin
  size := w * h * 3;
  pixels := getmem(size);
  move(pixdata^, pixels^, size);
  for i := 0 to w * h - 1 do begin
      t := pixels[i * 3];
      pixels[i * 3] := pixels[i * 3 + 2];
      pixels[i * 3 + 2] := t;
  end;
  WriteTga(fname, pixels, w, h, size);
  freemem(pixels);
end;

{
  DumpTextures
  Saves unique textures to disk.
}
procedure DumpTextures(const pdo: TPdoStructure; texture_names: TStringList; const tex_name_prefix: string);
var
  tex_counter: integer;
  name: string;
  tex: TPdoTexture;
  material: TPdoMaterial;
  pixdata: pbyte;
  hashes: TStringList;
  hash: string;
  hash_index: integer;
  pixel_size: integer;
begin
  tex_counter := 0;
  hashes := TStringList.Create;

  for material in pdo.materials do begin
      if material.has_texture then
      begin
          tex := material.texture;
          name := tex_name_prefix + IntToStr(tex_counter) + '.tga';
          pixdata := pdo.tex_storage.GetPixels(tex.texture_id);
          pixel_size := tex.width * tex.height * 3;

          hash := IntToStr( GetHash(pixdata, pixel_size) );
          hash_index := hashes.IndexOf(hash);
          if hash_index > -1 then begin
              hashes.Add(hash);
              texture_names.Add(texture_names[hash_index]);  //duplicate
              continue;
          end else
              hashes.Add(hash);

          texture_names.Add(name);
          tex_counter += 1;

          StoreTexture(name, pixdata, tex.width, tex.height);
      end;
  end;

  hashes.Free;
end;


procedure SaveMaterials(const pdo: TPdoStructure; const obj_name: string);
var
  mtl_file:TextFile;
  mat: TPdoMaterial;
  texmap: TStringList;
  tex_counter: integer;
  tex_name: string;

procedure WriteBaseAttrs;
begin
  writeln(mtl_file, 'Ka 1.000 1.000 1.000');  //ambient color
  writeln(mtl_file, 'Kd 1.000 1.000 1.000');  //diffuse color
  writeln(mtl_file, 'Ks 1.000 1.000 1.000');  //specular color
  writeln(mtl_file, 'Ns 100.0');              //specular weight
  writeln(mtl_file, 'illum 2');               //Color on and Ambient on, Highlight on
end;

procedure WriteMaterial(const name, texture: string);
begin
  writeln(mtl_file, 'newmtl ', name);  //begin new material
  if texture <> '' then
      writeln(mtl_file, 'map_Kd ' + texture);  //texture
end;

begin
  texmap := TStringList.Create;
  DumpTextures(pdo, texmap, obj_name + '_tex');

  AssignFile(mtl_file, obj_name + '.mtl');
  Rewrite(mtl_file);

  writeln(mtl_file, '# ' + HeaderComment);
  WriteMaterial(DefaultMaterial, '');
  WriteBaseAttrs;

  tex_counter := 0;
  for mat in pdo.materials do begin
      if mat.has_texture then begin
          tex_name := texmap[tex_counter];
          tex_counter += 1;
      end else
          tex_name := '';
      WriteMaterial(mat.name, tex_name);
      WriteBaseAttrs;
      writeln(mtl_file);
  end;

  CloseFile(mtl_file);
  texmap.Free;
end;


procedure WriteObj(const pdo: TPdoStructure; const obj_name: string; const opts: TObjExportOptions);
const
  DesiredUnitSize = 2;
var
  objfile: TextFile;
  obj: TPdoObject;
  face: TPdoFace;
  vertex: TPdoFace2DVertex;
  vertex3d: TPdo3DVertex;
  x, y, z: double;
  u, v: double;
  scaling_factor: double;
  coord_max: double;
  uv_counter: integer;
  vertex3d_offset: integer;
  last_material_index: integer;

function GetMaxCoord: double;
begin
  result := 0;
  for obj in pdo.objects do begin
      for vertex3d in obj.vertices do begin
          x := abs(vertex3d.x - pdo.header.origin_offset[0]);
          y := abs(vertex3d.y - pdo.header.origin_offset[1]);
          z := abs(vertex3d.z - pdo.header.origin_offset[2]);
          coord_max := Max(z, Max(x, y));
          if coord_max > result then
              result := coord_max;
      end;
  end;
end;

begin
  AssignFile(objfile, obj_name);
  Rewrite(objfile);

  writeln(objfile, '# ' + HeaderComment);
  writeln(objfile, 'mtllib ', obj_name + '.mtl');

  //scale pass
  scaling_factor := 1;
  if opts.normalize then begin
      scaling_factor := DesiredUnitSize / GetMaxCoord;
      //writeln(stderr, scaling_factor);
  end;

  //vertex pass
  for obj in pdo.objects do begin
      for vertex3d in obj.vertices do begin
          x := (vertex3d.x - pdo.header.origin_offset[0]) * scaling_factor;
          y := (vertex3d.y - pdo.header.origin_offset[1]) * scaling_factor;
          z := (vertex3d.z - pdo.header.origin_offset[2]) * scaling_factor;
          writeln(objfile, 'v ', x:10:6, ' ', y:10:6, ' ', z:10:6);
      end;
  end;

  //uv pass
  for obj in pdo.objects do begin
      for face in obj.faces do begin
          for vertex in face.vertices do begin
              u := vertex.u;
              v := -vertex.v;
              if opts.flip_v_coordinate then
                  v := 1 - vertex.v;
              writeln(objfile, 'vt ', u:10:6, ' ', v:10:6);
          end;
      end;
  end;

  //face / material pass
  uv_counter := 0;
  vertex3d_offset := 1;
  last_material_index := -1;
  for obj in pdo.objects do begin
      writeln(objfile, 'g ' + obj.name);
      for face in obj.faces do begin
          if face.material_index <> last_material_index then begin
              if face.material_index = -1 then
                  writeln(objfile, 'usemtl ' + DefaultMaterial)
              else
                  writeln(objfile, 'usemtl ' + pdo.materials[face.material_index].name);
              last_material_index := face.material_index;

          end;
          write(objfile, 'f ');
          for vertex in face.vertices do begin
              uv_counter += 1;
              write(objfile, vertex.id_vertex + vertex3d_offset);
              write(objfile, '/', uv_counter);
              write(objfile, ' ');
          end;
          writeln(objfile);
      end;
      vertex3d_offset += Length(obj.vertices);
  end;

  CloseFile(objfile);

  SaveMaterials(pdo, obj_name);
end;

end.

