unit pdo_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tex_storage;

const
  FileMagic = 'version 3'+#10;
  PdoV5Magic = 'Pepakura Designer 3';
  PDO_V4 = 4;
  PDO_V5 = 5;
  PDO_V6 = 6;
  CodepageShiftJIS = 'Sfhit-JIS';
  PdoPageTypes: array[0..11] of string[6] =
             ('A4', 'A3', 'A2', 'A1', 'B5', 'B4', 'B3', 'B2', 'B1', 'letter', 'legal', 'other');
  PdoPageTypeOther = 11;
  FileEndMagic = $270f;  //9999

type
  TPdoTexture = record
      width,
      height: integer;
      data_size: longword;
      data: pbyte;
      texture_id: integer;
  end;

  TPdoMaterial = record
      name: string;
      color2d_rgba: array[0..3] of single;
      has_texture: boolean;
      texture: TPdoTexture;
      color3d: array[0..15] of single;
  end;

  TPdo3DVertex = record
      x, y, z: double;
  end;

  TPdoFace2DVertex = record
      id_vertex: integer;
      x, y,
      u, v: double;
      flap: byte;
      flap_height: double;
      flap_a_angle: double;
      flap_b_angle: double;
  end;

  TPdoLineType = (ltCut = 0, ltMountain = 1, ltValley = 2);

  TPdoEdge = packed record
      face1_index: integer;
      face2_index: integer;
      vertex1index: integer;
      vertex2index: integer;
      connects_faces: smallint;
      no_connected_face: integer;
      //added
      linetype: integer; //0,1,2  cut,mountain,valley
  end;

  TPdoFace = record
      material_index: integer;
      part_index: integer;
      Nx, Ny, Nz: double;
      coord: double;
      vertices: array of TPdoFace2DVertex;
  end;

  TPdoObject = record
      name: string;
      visible: byte;
      vertices: array of TPdo3DVertex;
      faces: array of TPdoFace;
      edges: array of TPdoEdge;
  end;

  TPdoRect = packed record
      left,
      top,
      width,
      height: double;
  end;

  TPdoTextBlock = record
      font_name: string;
      font_size: integer;
      bounding_box: TPdoRect;
      line_spacing: double; //space between lines in mm
      lines: array of string;
  end;

  TPdoImage = record
      bounding_box: TPdoRect;
      texture: TPdoTexture;
  end;

  TPdoLine = record
      hidden: boolean;
      type_: integer;   {line type for flaps added to unjoined lines}
      face_index: integer;
      vertex_index: integer; {objects vertex list}
      is_connecting_faces: boolean;
      face2_index: integer;
      vertex2_index: integer;   {objects vertex list}
  end;

  TPdoLineExtra = record
      Ltype: integer;     {0,1,2  cut,maountain,valley}
      isflap: boolean;    {line has flap flag}
      vert2didx1: integer;   {2d face vertex idx 0-2/3}  {line1 start}
      vert2didx2: integer;   {2d face vertex idx 0-2/3}  {line1 end}
      flapvert1x: double;
      flapvert1y: double;
      flapvert2x: double;
      flapvert2y: double;
      flaptype: integer; {0 = quad, 1= tri}
  end;

  TPdoPart = record
      object_index: integer;
      name: string;
      bounding_box: TPdoRect;
      lines: array of TPdoLine;
      lines_extra: array of TPdoLineExtra;
  end;

  TPdoHeader = record
      version: integer;
      multi_byte_chars: integer;
      designer_id: string;
      string_shift: integer;
      texlock: integer;
      locale, codepage: string;
      key: string;
      v6_lock: integer;
      show_startup_notes: byte;
      password_flag: byte;
      assembled_height: double;
      origin_offset: array [0..2] of double;
  end;

  TPdoSettings = record
      //misc options
      show_flaps,
      show_edge_id,
      edge_id_placement,
      face_material: byte;

      hide_almost_flat_fold_lines: byte;
      fold_lines_hiding_angle: integer;
      draw_white_line_under_dot_line: byte;

      mountain_fold_line_style,
      valley_fold_line_style,
      cut_line_style: integer;

      edge_id_font_size: integer;

      //page setup
      page: record
          type_: integer;
          custom_width: double;
          custom_height: double;
          orientation: integer;
          margin_top: integer;
          margin_side: integer;
      end;

      //line patterns: 3-times pair of line length + gap length (mm)
      mountain_fold_line_pattern: array[0..5] of double;
      valley_fold_line_pattern: array[0..5] of double;

      add_outline_padding: byte;
      scale_factor: double;
      author_name: string;
      comment: string;
  end;

  TPdoUnfold = record
      scale: double;
      bounding_box: TPdoRect;
  end;

  TPdoStructure = record
      header: TPdoHeader;
      objects: array of TPdoObject;
      materials: array of TPdoMaterial;
      textblocks: array of TPdoTextBlock;
      parts: array of TPdoPart;
      images: array of TPdoImage;
      settings: TPdoSettings;
      unfold: TPdoUnfold;
      tex_storage: TTextureStorage;
  end;


implementation


end.

