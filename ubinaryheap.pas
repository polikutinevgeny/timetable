unit UBinaryHeap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TIDDateData = record
    ID: Integer;
    EndDate: TDateTime;
    Data: array of String;
  end;

  TIDDateCapacity = record
    ID: Integer;
    EndDate: TDateTime;
    Capacity: Integer
  end;

  { TBinaryHeap }

  generic TBinaryHeap<T> = class
    private
      procedure Heapify(i: Integer);
    public
      Values: array of T;
      procedure Push(AValue: T);
      procedure Pop;
      destructor Destroy; override;
  end;

  TIDDateBinaryHeap = specialize TBinaryHeap<TIDDateData>;
  TIDDateCapacityBinaryHeap = specialize TBinaryHeap<TIDDateCapacity>;

function IDDateData(AID: Integer; ADate: TDateTime; AData: array of String): TIDDateData;
operator < (const a, b: TIDDateData): Boolean;

function IDDateCapacity(AID: Integer; ADate: TDateTime; ACapacity: Integer): TIDDateCapacity;
operator < (const a, b: TIDDateCapacity): Boolean;

implementation

function IDDateData(AID: Integer; ADate: TDateTime; AData: array of String
  ): TIDDateData;
var i: Integer;
begin
  Result.EndDate := ADate;
  Result.ID := AID;
  SetLength(Result.Data, Length(AData));
  for i := 0 to High(AData) do
    Result.Data[i] := AData[i];
end;

operator < (const a, b: TIDDateData): Boolean;
begin
  Result := a.EndDate < b.EndDate;
end;

function IDDateCapacity(AID: Integer; ADate: TDateTime; ACapacity: Integer
  ): TIDDateCapacity;
begin
  Result.EndDate := ADate;
  Result.ID := AID;
  Result.Capacity := ACapacity;
end;

operator < (const a, b: TIDDateCapacity): Boolean;
begin
  Result := a.EndDate < b.EndDate;
end;

{ TBinaryHeap }

procedure TBinaryHeap.Heapify(i: Integer);
var
  smallest: Integer;
  temp: T;
begin
  if ((2 * i + 1 < Length(Values)) and (Values[2 * i + 1] < Values[i])) then
    smallest := 2 * i + 1
  else
    smallest := i;
  if ((2 * i + 2 < Length(Values)) and (Values[2 * i + 2] < Values[smallest])) then
    smallest := 2 * i + 2;
  if (smallest <> i) then
  begin
    temp := Values[i];
    Values[i] := Values[smallest];
    Values[smallest] := temp;
    Heapify(smallest);
  end;
end;

procedure TBinaryHeap.Push(AValue: T);
var i: Integer;
begin
  SetLength(Values, Length(Values) + 1);
  i := Length(Values) - 1;
  while (i <> 0) and (AValue < Values[(i - 1) div 2]) do
  begin
    Values[i] := Values[(i - 1) div 2];
    i := (i - 1) div 2;
  end;
  Values[i] := AValue;
end;

procedure TBinaryHeap.Pop;
begin
  if (Length(Values) > 0) then
  begin
    Values[0] := Values[High(Values)];
    SetLength(Values, Length(Values) - 1);
    Heapify(0);
  end;
end;

destructor TBinaryHeap.Destroy;
begin
  inherited Destroy;
  SetLength(Values, 0);
end;

end.

