unit m_export_svg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pdo_common, pdo2opf, opf2vector, progress_report;

type
  { TProgressTracker }
  TPtState = (ptParsing, ptTransform, ptExport, ptDone);
  TProgressTracker = class(TPartProgressLog)
  private
    _state: TPtState;
    _parts: integer;
    _current: integer;
  public
    procedure SetState(const state: TPtState);
    function GetState: TPtState;
    function GetPartCount: integer;
    function GetCurrent: integer;
    procedure BeginWriting(const parts: integer); override;
    procedure EndWriting; override;
    procedure PartWritten; override;
  end;

  TExportParams = record
      Tracker: TProgressTracker;
      FileName: string;
      Dpi: integer;
      MinDpi: integer;
      UsePdf: boolean;
      Opts: TVectorExportOptions;
  end;

  { TVectorExportThread }

  TVectorExportThread = class(TThread)
  private
    _pdo_struct: TPdoStructure;
    _params: TExportParams;
  protected
    procedure Execute; override;
  public
    procedure SetupExport(var pdo_struct: TPdoStructure; const params: TExportParams);
  end;


implementation

{ TProgressTracker }

procedure TProgressTracker.SetState(const state: TPtState);
begin
  _state := state;
end;

function TProgressTracker.GetState: TPtState;
begin
  result := _state
end;

function TProgressTracker.GetPartCount: integer;
begin
  result := _parts;
end;

function TProgressTracker.GetCurrent: integer;
begin
  result := _current;
end;

procedure TProgressTracker.BeginWriting(const parts: integer);
begin
  _parts := parts;
  _current := 0;
end;

procedure TProgressTracker.EndWriting;
begin
end;

procedure TProgressTracker.PartWritten;
begin
  InterLockedIncrement(_current);
end;


procedure ExportVectors(var pdo_struct: TPdoStructure; params: TExportParams);
var
  xform: PdoToOpf2dTransform;
  format: TVectorExportFormat;
  progress: TProgressTracker;
  vec_export: TOpf2dVectorExport;
begin
  if params.MinDpi > params.Dpi then
      params.Dpi := params.MinDpi;

  progress := params.Tracker;
  progress.SetState(ptTransform);
  xform := PdoToOpf2dTransform.Create(pdo_struct, progress);
  xform.PartsTo2D;
  xform.RasterizeToPngStream(params.dpi, params.MinDpi);

  progress.SetState(ptExport);
  format := TvfSvg;
  if params.UsePdf then
      format := TvfPdf;
  params.Opts.debug_edges := false;

  vec_export := TOpf2dVectorExport.Create(pdo_struct, xform.GetParts, xform.GetPageSetup);
  vec_export.Prepare(params.Opts);
  vec_export.ExportToFile(params.FileName, format);
  vec_export.Free;

  xform.Free;
  progress.SetState(ptDone);
end;


{ TVectorExportThread }

procedure TVectorExportThread.Execute;
begin
  ExportVectors(_pdo_struct, _params);
end;

procedure TVectorExportThread.SetupExport(var pdo_struct: TPdoStructure; const params: TExportParams);
begin
  _pdo_struct := pdo_struct;
  _params := params;
end;

end.

