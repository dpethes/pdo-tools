unit utils;

{$mode objfpc}{$H+}

interface

uses classes, sysutils;

procedure WriteTga(const filename: string; const data: pbyte; const width, height, data_length: integer);
procedure PgmSave(const fname: string; p: pbyte; w, h: integer);
procedure PnmSave(const fname: string; const p: pbyte; const w, h: integer);
function clip3(const a, b, c: single): single;
function clip3(const a, b, c: integer): integer;
function GetMsecs: longword;
procedure Swap2f(var a, b: single);

implementation

procedure WriteTga(const filename: string; const data: pbyte; const width, height, data_length: integer);
const
  HeaderComment = 'Pdo tools';
var
  f: file;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create();
  stream.WriteByte(Length(HeaderComment)); //id field length
  stream.WriteByte (0);  //color map type
  stream.WriteByte (2);  //image type: 2 = uncompressed true-color image
  //5B color map specification
  stream.WriteDWord(0);  //2B origin, 2B length
  stream.WriteByte (0);  //1B Color Map Entry Size.
  //10B image specification
  stream.WriteDWord(0);      //X-origin, Y-origin
  stream.WriteWord (width);  //width in pixels
  stream.WriteWord (height); //height in pixels
  stream.WriteByte (24);     //bits per pixel
  stream.WriteByte ($20);    //image descriptor
  stream.Write(HeaderComment, Length(HeaderComment));

  AssignFile(f, filename);
  Rewrite(f, 1);
  blockwrite(f, stream.Memory^, stream.Size);
  blockwrite(f, data^, data_length);
  CloseFile(f);
  stream.Free;
end;

procedure PgmSave(const fname: string; p: pbyte; w, h: integer);
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P5'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h);
  CloseFile (f);
end;

procedure PnmSave(const fname: string; const p: pbyte; const w, h: integer);
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P6'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h * 3);
  CloseFile (f);
end;


function clip3(const a, b, c: single): single;
begin
  if b < a then begin
    result := a;
    //writeln('range error: ', b);
  end
  else if b > c then begin
    result := c;
    //writeln('range error: ', b);
  end
  else result := b;
end;

function clip3(const a, b, c: integer): integer;
begin
  if b < a then
    result := a
  else if b > c then
    result := c
  else result := b;
end;

function GetMsecs: longword;
var
  h, m, s, ms: word;
begin
  DecodeTime (sysutils.Time(), h, m, s, ms);
  Result := (h * 3600*1000 + m * 60*1000 + s * 1000 + ms);
end;

procedure Swap2f(var a, b: single);
var
  t: single;
begin
  t := a;
  a := b;
  b := t;
end;

end.

