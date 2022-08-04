program pdodump;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  sysutils, classes, crc,
  gzip_format, dc2_simple_api, utils, hpdf, progress_report,
  pdo_format, pdo_common, pdo_writer, pdo2obj, pdo2opf, opf2vector;

const
  DefaultDpi = 150;
  ImageDumpPath = 'r:\';

type
  TCliParams = record
    detailedTime: boolean;
    dumpStructure: boolean;
    dumpPng: boolean;
    multifile: boolean;
  end;

var
  g_params: TCliParams;

function optSet(const c: char): boolean;
begin
  result := (Paramcount > 1) and (Pos(c, ParamStr(2)) <> 0)
end;


{ ExportPatterns
  Export 2d patterns from the pdo data as either SVG or PDF files.
}
procedure ExportPatterns(const pdo: TPdoParser; const name: string;
  const exportFormat: TVectorExportFormat);
var
  log: T2dTransformPartProgressLog;
  transform: PdoToOpf2dTransform;
  pdo_struct: TPdoStructure;
  opt: TVectorExportOptions;
  vec_export: TOpf2dVectorExport;
  t: longword;

  procedure TimedProc(const procName: string);
  begin
    t := GetMsecs;
    writeln(procName);
  end;
  procedure EndTimer();
  begin
    if not g_params.detailedTime then exit;
    t := GetMsecs - t;
    writeln(Format('  took %.2fs', [t / 1000]));
  end;

begin
  if exportFormat = TvfSvg then begin
      writeln('exporting to svg file: ', name);
  end else begin
      writeln('exporting to pdf file: ', name);
      if LoadHaru then
          writeln('using libHaru ', HPDF_GetVersion)
      else begin
          writeln('Couldn''t load libHaru!');
          exit;
      end;
  end;

  TimedProc('transforming parts to 2D');
  pdo_struct := pdo.GetStructure;
  log := T2dTransformPartProgressLog.Create;
  transform := PdoToOpf2dTransform.Create(pdo_struct, log);
  transform.PartsTo2D;
  EndTimer;

  TimedProc('rasterizing and compressing');
  transform.RasterizeToPngStream(DefaultDpi);
  EndTimer;

  if g_params.dumpPng then
      transform.DumpPng(ImageDumpPath);

  TimedProc('output writing');
  DefaultVectorExportOptions(opt);
  opt.tabs := pdo_struct.settings.show_flaps = 1;
  opt.faces := exportFormat = TvfSvg;
  opt.multi_file := g_params.multifile;
  //opt.debug_edges := true;

  vec_export := TOpf2dVectorExport.Create(pdo_struct, transform.GetParts, transform.GetPageSetup);
  vec_export.Prepare(opt);
  vec_export.ExportToFile(name, exportFormat);
  vec_export.Free;
  EndTimer;

  log.Free;
  transform.Free;
end;


{ Gzcompress
  Compress SVG with Gzip, aka SVGZ.
}
procedure Gzcompress(const svg_name, svgz_name: string);
var
  svg, svgz: TMemoryStream;
  svgz_file: file;
  checksum: longword;
  encoder: TLzEncoder;
begin
  writeln('compress to svgz file: ', svgz_name);
  AssignFile(svgz_file, svgz_name);
  Rewrite(svgz_file, 1);
  gzf_write_header(svgz_file, svg_name);

  svg := TMemoryStream.Create;
  svg.LoadFromFile(svg_name);
  svgz := TMemoryStream.Create;

  encoder := TLzEncoder.Create(1);
  encoder.EncodeBytesToStream(svg.Memory, svg.Size, svgz);
  encoder.Free;
  svg.Free;

  checksum := crc32(0, nil, 0);
  checksum := crc32(checksum, svgz.Memory, svgz.Size);
  BlockWrite(svgz_file, svgz.Memory^, svgz.Size);
  gzf_write_end(svgz_file, svgz.Size, checksum);
  CloseFile(svgz_file);
  svgz.Free;
end;

{ ExportMesh
  Export the 3d mesh from the pdo data.
}
procedure ExportMesh(const name: string; const pdo: TPdoParser);
var
  opt: TObjExportOptions;
begin
  writeln('exporting obj: ', name);
  opt.normalize := true;
  opt.flip_v_coordinate := false;
  WriteObj(pdo.GetStructure, name, opt);
end;

{ RewritePdo
  Write a copy of the unmodified input file using the PDO writing functions
}
procedure RewritePdo(const name: string; const pdo: TPdoParser);
begin
  writeln('rewriting: ', name);
  WritePdo(pdo.GetStructure, name);
end;

{ Main
}
var
  infile: string;
  pdo: TPdoParser;
  fname: string;
  t: LongWord;

begin
  if Paramcount < 1 then begin
      writeln('no input file specified!');
      writeln('usage: pdodump file [params] [output name]');
      writeln('export parameters:');
      writeln('  d - dump pdo structure');
      writeln('  e - resave pdo file');
      writeln('  o - output obj');
      writeln('  f - output pdf');
      writeln('  s - output svg');
      writeln('optional:');
      writeln('  m - export each page separately');
      writeln('  p - dump rasterized textures to png');
      writeln('  t - print elapsed time for 2D export subparts');
      writeln('  z - compress the svg output to svgz');
      writeln('  x - dump pdo textures to pnm');
      halt;
  end;
  infile := ParamStr(1);
  writeln('file: ', infile);
  if not FileExists(infile) then begin
      writeln('file doesn''t exist!');
      halt;
  end;

  g_params.dumpStructure := false;
  if Paramcount > 1 then
      g_params.dumpStructure := optSet('d');

  t := GetMsecs;
  pdo := TPdoParser.Create(g_params.dumpStructure);
  try
    pdo.OpenFile(infile);
  except
    on e: Exception do begin
        writeln(e.Message);
        writeln('could not open file, ending');
        halt;
    end;
  end;
  pdo.Load;
  t := GetMsecs - t;
  writeln('parsing time: ', (t / 1000):5:2);

  //process data
  t := GetMsecs;
  if Paramcount > 1 then begin
      fname := ExtractFilePath(infile);
      if fname <> '' then
          fname += DirectorySeparator;
      if Paramcount > 2 then
          fname += ParamStr(3)
      else
          fname += ExtractFileName(ParamStr(1));

      g_params.dumpPng := optSet('p');
      g_params.multifile := optSet('m');
      g_params.detailedTime := optSet('t');

      if optSet('o') then
          ExportMesh(fname + '.obj', pdo);
      if optSet('s') then begin
          ExportPatterns(pdo, fname, TvfSvg);
          if optSet('z') then begin
              Gzcompress(fname + '.svg', fname + '.svgz');
              DeleteFile(fname + '.svg');
          end;
      end;
      if optSet('f') then
          ExportPatterns(pdo, fname, TvfPdf);
      if optSet('e') then
          RewritePdo(fname + '_new.pdo', pdo);
      if optSet('x') then
          pdo.GetStructure.tex_storage.DumpTextures(ImageDumpPath);
  end;
  t := GetMsecs - t;
  writeln('processing time: ', (t / 1000):5:2);

  pdo.Free;
  writeln('done');
end.

