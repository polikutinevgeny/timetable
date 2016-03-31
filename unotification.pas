unit UNotification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataUpdateEvent = procedure of object;

procedure RegisterDataUpdateListener(AListener: TDataUpdateEvent);
procedure RemoveDataUpdateListener(AListener: TDataUpdateEvent);
procedure OnDataUpdate;

implementation

var DataUpdateListeners: array of TDataUpdateEvent;

procedure RegisterDataUpdateListener(AListener: TDataUpdateEvent);
begin
  SetLength(DataUpdateListeners, Length(DataUpdateListeners) + 1);
  DataUpdateListeners[High(DataUpdateListeners)] := AListener;
end;

procedure RemoveDataUpdateListener(AListener: TDataUpdateEvent);
var i, j: Integer;
begin
  for i := 0 to High(DataUpdateListeners) do
    if DataUpdateListeners[i] = AListener then
    begin
      for j := i to High(DataUpdateListeners) - 1 do
        DataUpdateListeners[i] := DataUpdateListeners[i + 1];
      SetLength(DataUpdateListeners, Length(DataUpdateListeners) - 1);
      Exit;
    end;
  raise Exception.Create('Attempt to remove a not existing listener');
end;

procedure OnDataUpdate;
var i: Integer;
begin
  for i := 0 to High(DataUpdateListeners) do
    DataUpdateListeners[i];
end;

end.

