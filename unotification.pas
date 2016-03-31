unit UNotification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataUpdateEvent = procedure of object;

procedure RegisterListener(AListener: TDataUpdateEvent);
procedure RemoveListener(AListener: TDataUpdateEvent);
procedure OnDataUpdate;

implementation

var Listeners: array of TDataUpdateEvent;

procedure RegisterListener(AListener: TDataUpdateEvent);
begin
  SetLength(Listeners, Length(Listeners) + 1);
  Listeners[High(Listeners)] := AListener;
end;

procedure RemoveListener(AListener: TDataUpdateEvent);
var i, j: Integer;
begin
  for i := 0 to High(Listeners) do
    if Listeners[i] = AListener then
    begin
      for j := i to High(Listeners) - 1 do
        Listeners[i] := Listeners[i + 1];
      SetLength(Listeners, Length(Listeners) - 1);
      Exit;
    end;
  raise Exception.Create('Attempt to remove a not existing listener');
end;

procedure OnDataUpdate;
var i: Integer;
begin
  for i := 0 to High(Listeners) do
    Listeners[i];
end;

end.

