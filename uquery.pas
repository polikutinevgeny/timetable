unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UFilters;

type

  { TQuery }

  TQuery = class
    private
      FTables: TTableArray;
      FCols: TColArray;
      FFilters: TFilterArray;
      function GetBaseTable: TTable;
      function GetSelectAsText: String;
      function GetFromAsText: String;
      function GetFiltersAsText: String;
      procedure SetFilters(AFilters: TFilterArray);
      procedure SetCols;
      procedure SetTables(ATable: TTable);
    public
      constructor Create(ATable: TTable; AFilterList: TFilterArray);
      function QueryAsText: String;//returns parametrized query, without parameters
      function DeleteQueryAsText: String;
      property Cols: TColArray read FCols;
      property BaseTable: TTable read GetBaseTable;
  end;

implementation

{ TQuery }

function TQuery.GetSelectAsText: String;
var i, j: Integer;
begin
  Result := Format('SELECT %s.%s AS "%s",', [
    FTables[0].SQLName, FTables[0].PrimaryKey.SQLName,
    FTables[0].PrimaryKey.DisplayName]);
  for i := 0 to High(FTables) do
    for j := 0 to High(FTables[i].Cols) do
      Result += Format('%s.%s AS "%s",', [
        FTables[i].SQLName, FTables[i].Cols[j].SQLName,
        FTables[i].Cols[j].DisplayName]);
  Result[High(Result)] := ' ';
end;

function TQuery.QueryAsText: String;
begin
  Result := Format('%s %s %s', [
    GetSelectAsText, GetFromAsText, GetFiltersAsText]);
end;

function TQuery.GetBaseTable: TTable;
begin
  Result := FTables[0];
end;

function TQuery.GetFromAsText: String;
var i: Integer;
begin
  Result := Format('FROM %s ', [FTables[0].SQLName]);
  for i := 0 to High(FTables[0].ForeignKeys) do
    Result += Format('INNER JOIN %s ON %s.%s = %s.%s ', [
      FTables[0].ForeignKeys[i].Reference.Table.SQLName,
      FTables[0].SQLName, FTables[0].ForeignKeys[i].SQLName,
      FTables[0].ForeignKeys[i].Reference.Table.SQLName,
      FTables[0].ForeignKeys[i].Reference.SQLName]);
end;

function TQuery.GetFiltersAsText: String;
var i: Integer;
begin
  if Length(FFilters) = 0 then
  begin
    Result := '';
    Exit;
  end;
  Result := 'WHERE ';
  for i := 0 to High(FFilters) do
  begin
    Result += Format('%s.%s %s %s ', [
      FFilters[i].Column.Table.SQLName, FFilters[i].Column.SQLName,
      FFilters[i].Action, ':p' + IntToStr(i)]);
    if i < High(FFilters) then
      Result += 'AND ';
  end;
end;

procedure TQuery.SetFilters(AFilters: TFilterArray);
var i: Integer;
begin
  SetLength(FFilters, Length(AFilters));
  for i := 0 to High(AFilters) do
    FFilters[i] := AFilters[i];
end;

procedure TQuery.SetCols;
var i, j: Integer;
begin
  for i := 0 to High(FTables) do
    for j := 0 to High(FTables[i].Cols) do
    begin
      SetLength(FCols, Length(FCols) + 1);
      FCols[High(FCols)] := FTables[i].Cols[j];
    end;
end;

procedure TQuery.SetTables(ATable: TTable);
var
  i: Integer;
begin
  SetLength(FTables, Length(ATable.ForeignKeys) + 1);
  FTables[0] := ATable;
  for i := 1 to High(FTables) do
    FTables[i] := FTables[0].ForeignKeys[i - 1].Reference.Table;
end;

constructor TQuery.Create(ATable: TTable; AFilterList: TFilterArray);
begin
  SetTables(ATable);
  SetFilters(AFilterList);
  SetCols;
end;

function TQuery.DeleteQueryAsText: String;
begin
  Result := 'DELETE FROM ' + FTables[0].SQLName + ' WHERE ' +
    FTables[0].PrimaryKey.SQLName + ' = :' + FTables[0].PrimaryKey.SQLName;
end;

end.

