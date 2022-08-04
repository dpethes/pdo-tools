unit progress_report;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type

TPartProgressLog = class
  public
    procedure BeginWriting(const parts: integer); virtual; abstract;
    procedure PartWritten; virtual; abstract;
    procedure EndWriting; virtual; abstract;
end;

{ TSvgPartProgressLog }

TSvgPartProgressLog = class (TPartProgressLog)
  private
    _parts: integer;
    _current: integer;
  public
    procedure BeginWriting(const parts: integer); override;
    procedure EndWriting; override;
    procedure PartWritten; override;
end;

{ T2dTransformPartProgressLog }

T2dTransformPartProgressLog = class (TPartProgressLog)
  private
    _lock: TCriticalSection;
    _parts: integer;
    _current: integer;
  public
    procedure BeginWriting(const parts: integer); override;
    procedure EndWriting; override;
    procedure PartWritten; override;
end;

//**************************************************************************************************
implementation

{ T2dTransformPartProgressLog }

procedure T2dTransformPartProgressLog.BeginWriting(const parts: integer);
begin
  _parts := parts;
  _current := 0;
  _lock := TCriticalSection.Create;
end;

procedure T2dTransformPartProgressLog.EndWriting;
begin
  write(#10);
  _lock.Destroy;
end;

procedure T2dTransformPartProgressLog.PartWritten;
begin
  _lock.Enter;
  _current += 1;
  write('processed ', _current, '/', _parts, #13);
  _lock.Leave;
end;

{ TSvgPartProgressLog }

procedure TSvgPartProgressLog.BeginWriting(const parts: integer);
begin
  _parts := parts;
  _current := 0;
  writeln('writing parts to svg: ', parts);
end;

procedure TSvgPartProgressLog.EndWriting;
begin
  write(#10);
end;

procedure TSvgPartProgressLog.PartWritten;
begin
  _current += 1;
  write('processed ', _current, '/', _parts, #13);
end;

end.

