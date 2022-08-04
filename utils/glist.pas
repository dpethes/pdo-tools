{
  workarounds for TVector annoyances  / FPGList incompatibilities:
  * Size is unsigned: for loops need explicit IsEmpty checks to prevent overflows in case when Size is 0:
     for i := 0 to v.Size - 1 do
  * PushBack instead of Add
}
unit glist;
{$mode objfpc}

interface

type

  { customized TVector from gvector }

  generic TList<T> = class
  private
  type
    TArr = array of T;
  var
    FCapacity:SizeUInt;
    FDataSize:SizeUInt;
    FData:TArr;

    procedure SetValue(Position: SizeUInt; const Value: T); inline;
    function GetValue(Position: SizeUInt): T; inline;
    procedure IncreaseCapacity; inline;

    property Size: SizeUInt read FDataSize;

  const
    // todo: move these constants to implementation when
    // mantis #0021310 will be fixed.
    SVectorPositionOutOfRange      = 'Vector position out of range';

  public
    constructor Create;
    procedure Add(const Value: T); //inline;
    function IsEmpty: boolean; inline;
    procedure Remove(Position: SizeUInt); inline;
    procedure Clear; inline;
    procedure Reserve(Num: SizeUInt);
    procedure Resize(Num: SizeUInt);
    function Count: integer; inline;
    function Copy: TList;

    property Items[i : SizeUInt]: T read getValue write setValue; default;
end;


implementation

{ TVector }

constructor TList.Create();
begin
  FCapacity:=0;
  FDataSize:=0;
end;

procedure TList.SetValue(Position: SizeUInt; const Value: T);
begin
  Assert(position < size, SVectorPositionOutOfRange);
  FData[Position]:=Value;
end;

function TList.GetValue(Position: SizeUInt): T;
begin
  Assert(position < size, SVectorPositionOutOfRange);
  GetValue:=FData[Position];
end;

function TList.IsEmpty(): boolean;
begin
  IsEmpty := (Size = 0);
end;

procedure TList.Add(const Value: T);
begin
  if FDataSize=FCapacity then
    IncreaseCapacity;
  FData[FDataSize]:=Value;
  inc(FDataSize);
end;

procedure TList.IncreaseCapacity();
begin
  if FCapacity=0 then
    FCapacity:=1
  else
    FCapacity:=FCapacity*2;
  SetLength(FData, FCapacity);
end;

procedure TList.Remove(Position: SizeUInt);
begin
  if Position < Size then
  begin
    dec(FDataSize);
    // ensure that the data we want to Remove is released
    FData[Position] := Default(T);
    Move(FData[Position+1], FData[Position], (FDataSize - Position) * SizeOf(T));
  end;
end;

procedure TList.Clear;
begin
  FDataSize:=0;
end;

procedure TList.Reserve(Num: SizeUInt);
begin
  if(Num < FCapacity) then
    exit
  else if(Num <= 2*FCapacity) then
    IncreaseCapacity
  else begin
    SetLength(FData, Num);
    FCapacity:=Num;
  end;
end;

procedure TList.Resize(Num: SizeUInt);
begin
  Reserve(Num);
  FDataSize:=Num;
end;

function TList.Count: integer;
begin
  Assert(Size < MaxInt);
  result := integer(Size);
end;

//todo memcpy?
function TList.Copy: TList;
var
  i: Integer;
begin
  result := TList.Create;
  result.Reserve(Count);
  for i := 0 to Count - 1 do
      result.Add(Items[i]);
end;

end.

