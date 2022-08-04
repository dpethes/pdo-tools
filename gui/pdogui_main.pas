unit pdogui_main;

{$mode objfpc}{$H+}

interface

uses
  pdo_common, pdo_format, pdo2obj, md5, m_export_svg,
  Classes, SysUtils, LazFileUtils, LazUTF8, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, LCLIntf,
  hpdf, LConvEncoding;

type

  { TPdoGui }

  TPdoGui = class(TPdoParser)
    public
      procedure Error(s: string); override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnOpen: TButton;
    btnSaveSvg: TButton;
    btnSaveObj: TButton;
    btnSavePdf: TButton;
    cbMinDpi: TComboBox;
    cbDpi: TComboBox;
    chbMergeTabsToOutline: TCheckBox;
    chbFoldLinesEx: TCheckBox;
    chbMultifile: TCheckBox;
    chbNormalize: TCheckBox;
    chbOutlineLayer: TCheckBox;
    chbTextureLayer: TCheckBox;
    chbFlipVCoord: TCheckBox;
    chbFoldLines: TCheckBox;
    chbTabs: TCheckBox;
    chbFaceLayer: TCheckBox;
    cbCharset: TComboBox;
    dlgSaveExport: TSaveDialog;
    lblCharset: TLabel;
    lblDpi: TLabel;
    lblDpiMin: TLabel;
    StatusBar: TStatusBar;
    SvgExportTimer: TTimer;
    txtAbout: TMemo;
    tabAbout: TTabSheet;
    txtFile: TEdit;
    dlgOpenPdo: TOpenDialog;
    gbSvg: TGroupBox;
    gbObj: TGroupBox;
    PageCtrl: TPageControl;
    tabExports: TTabSheet;
    tabTools: TTabSheet;
    lblFile: TLabel;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveObjClick(Sender: TObject);
    procedure btnSavePdfClick(Sender: TObject);
    procedure btnSaveSvgClick(Sender: TObject);
    procedure chbTextureLayerChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure SvgExportTimerTimer(Sender: TObject);
  private
    { private declarations }
    pdo: TPdoParser;
    tracker: TProgressTracker;
    _svg_export_tstart: int64;
    _pdoFileName: string;

    procedure btnPatternExportClick(const use_pdf: boolean);
    procedure ExportToSvg(const fname: string; const use_pdf: boolean);
    procedure InitScreen;
    procedure OpenPdoFile(const pdo_name: string);
    procedure DisableInput;
    procedure EnableInput;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

const
  DpiOpts: array[0..4] of integer = (100, 150, 200, 300, 400);

var
  LibHaruLoaded: boolean;

{ TForm1 }

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if not dlgOpenPdo.Execute then
      exit;
  OpenPdoFile(dlgOpenPdo.FileName);
end;

procedure TForm1.btnSaveObjClick(Sender: TObject);
var
  obj_name: string;
  pwd, hash: string;
  obj: TPdoObject;
  sum: integer;
  opt: TObjExportOptions;
begin
  pwd := PasswordBox('Enter password for editing', 'Password:');

  sum := 0;
  for obj in pdo.GetStructure.objects do
      sum += length(obj.faces) + length(obj.edges);
  hash := IntToStr(sum) + pwd + IntToStr(pdo.GetStructure.header.texlock);
  hash := MD5Print( MD5String(hash) );
  pwd := pdo.GetStructure.header.key;
  if hash <> pwd then begin
      ShowMessage('That''s not the right password');
      exit;
  end;

  dlgSaveExport.Filter := 'Wavefront OBJ|*.obj';
  dlgSaveExport.DefaultExt := '.obj';
  dlgSaveExport.FileName := ExtractFileNameOnly(_pdoFileName) + dlgSaveExport.DefaultExt;
  if not dlgSaveExport.Execute then
      exit;
  obj_name := dlgSaveExport.FileName;
  opt.normalize := chbNormalize.Checked;
  opt.flip_v_coordinate := chbFlipVCoord.Checked;
  WriteObj(pdo.GetStructure, obj_name, opt);
  ShowMessage('Obj export finished.');
end;

procedure TForm1.btnSavePdfClick(Sender: TObject);
begin
  btnPatternExportClick(true);
end;

procedure TForm1.btnSaveSvgClick(Sender: TObject);
begin
  btnPatternExportClick(false);
end;

procedure TForm1.btnPatternExportClick(const use_pdf: boolean);
var
  file_name: string;
begin
  dlgSaveExport.Filter := 'Inkscape SVG|*.svg';
  dlgSaveExport.DefaultExt := '.svg';
  if use_pdf then begin
      dlgSaveExport.Filter := 'Portable document format|*.pdf';
      dlgSaveExport.DefaultExt := '.pdf';
  end;
  dlgSaveExport.FileName := ExtractFileNameOnly(_pdoFileName);

  if not dlgSaveExport.Execute then
      exit;
  file_name := dlgSaveExport.FileName;
  DisableInput;
  ExportToSvg(file_name, use_pdf);
  SvgExportTimer.Enabled := true;
  _svg_export_tstart := GetTickCount64;
end;

procedure TForm1.chbTextureLayerChange(Sender: TObject);
begin
  cbDpi.Enabled := chbTextureLayer.Checked;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  OpenPdoFile(FileNames[0]);
end;

procedure TForm1.FormShow(Sender: TObject);
{$i version.inc}
begin
  if RevisionStr <> '' then
  Caption := Caption + ' ' + RevisionStr;
  btnSaveSvg.Enabled := false;
  btnSavePdf.Enabled := false;
  btnSaveObj.Enabled := false;
  InitScreen;
  tracker := TProgressTracker.Create;

  LibHaruLoaded := LoadHaru;
  if not LibHaruLoaded then ShowMessage('Couldn''t load libhpdf!');
end;

procedure TForm1.SvgExportTimerTimer(Sender: TObject);

  function PartStatus: string;
  begin
    result := IntToStr(tracker.GetCurrent) + '/' + IntToStr(tracker.GetPartCount);
  end;

var
  t: integer;

begin
  case tracker.GetState of
  ptTransform:
      Form1.StatusBar.SimpleText := 'Processing parts: ' + PartStatus;
  ptExport:
      Form1.StatusBar.SimpleText := 'Saving parts to file: ' + PartStatus;
  ptDone: begin
      SvgExportTimer.Enabled := false;
      t := round((GetTickCount64 - _svg_export_tstart) / 1000);
      Form1.StatusBar.SimpleText := '';
      ShowMessage('Export finished in ' + IntToStr(t) + ' seconds.');
      EnableInput;
  end;
  end;
end;

procedure TForm1.InitScreen;
var
  i: integer;
  encodings: TStrings;
begin
  PageCtrl.TabIndex := 0;

  cbDpi.Items.Clear;
  for i := 0 to Length(DpiOpts) - 1 do
      cbDpi.Items.Add(IntToStr(DpiOpts[i]));
  cbDpi.ItemIndex := 1;

  encodings := TStringList.Create;
  GetSupportedEncodings(encodings);
  cbCharset.Items.Clear;
  cbCharset.Items.AddStrings(encodings);
  cbCharset.ItemIndex := 2;  //ansi
end;


procedure TForm1.ExportToSvg(const fname: string; const use_pdf: boolean);
var
  expThr: TVectorExportThread;
  params: TExportParams;
  pdo_struct: TPdoStructure;

  function GetDpi(dpiIdx: integer): integer;
  begin
    if (dpiIdx < 0) or (dpiIdx > length(DpiOpts) - 1) then
       dpiIdx := 0;
    result := DpiOpts[dpiIdx];
  end;

begin
  //setup params:
  //layers
  params.opts.tabs := chbTabs.Checked;
  params.opts.foldlines := chbFoldLines.Checked;
  params.opts.extended_foldlines := chbFoldLinesEx.Checked;
  params.opts.outlines := chbOutlineLayer.Checked;
  params.opts.textures := chbTextureLayer.Checked;
  params.opts.faces := chbFaceLayer.Checked;
  //texture settings
  params.Dpi    := GetDpi(cbDpi.ItemIndex);
  params.MinDpi := GetDpi(cbMinDpi.ItemIndex);
  //misc
  params.opts.multi_file := chbMultifile.Checked;
  params.Opts.outlines_with_tabs := chbMergeTabsToOutline.Checked;
  params.Opts.codepage := cbCharset.Items[cbCharset.ItemIndex];
  //export specifics
  params.FileName := fname;
  params.Tracker := tracker;
  params.UsePdf := use_pdf;

  //setup worker thread
  expThr := TVectorExportThread.Create(true);
  expThr.FreeOnTerminate := true;
  pdo_struct := pdo.GetStructure;
  expThr.SetupExport(pdo_struct, params);
  expThr.Start;
end;

procedure TForm1.OpenPdoFile(const pdo_name: string);
var
  fname: string;
begin
  try
    fname := UTF8ToSys(pdo_name);
    pdo := TPdoGui.Create;
    pdo.OpenFile(fname);
    pdo.Load;
  except
    on e: EInOutError do begin
      Form1.StatusBar.SimpleText := 'Could not read file.';
      exit;
    end;
  end;

  _pdoFileName := fname;
  txtFile.Text := fname;
  btnSaveSvg.Enabled := true;
  btnSavePdf.Enabled := LibHaruLoaded;
  btnSaveObj.Enabled := true;
  Form1.StatusBar.SimpleText := 'Opened file ' + ExtractFileNameOnly(pdo_name);
end;

procedure TForm1.DisableInput;
begin
  PageCtrl.Enabled := false;
  btnOpen.Enabled := false;
end;

procedure TForm1.EnableInput;
begin
  PageCtrl.Enabled := true;
  btnOpen.Enabled := true;
end;


{ TPdoGui }

procedure TPdoGui.Error(s: string);
begin
  MessageDlg('Reading error', s, mtError, [mbOK], 'there''s a problem with this file');
  halt;
end;

end.

