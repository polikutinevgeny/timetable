unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UFilters;

type

  { TDirectoryQuery }

  TDirectoryQuery = class
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
      function SelectQueryAsText(ASortColumn: String = '';
        ASortDirection: String = ''): String;//returns parametrized query, without parameters
      function DeleteQueryAsText: String;
      property Cols: TColArray read FCols;
      property BaseTable: TTable read GetBaseTable;
  end;

  { TCardQuery }

  TCardQuery = class
    private
      FTable: TTable;
      FID: Integer;
    public
      constructor Create(ATable: TTable; AID: Integer);
      function SelectQueryAsText: String;
      function ComboboxQueryAsText(ACol: TCol): String;
      function InsertQueryAsText: String;
      function UpdateQueryAsText: String;
  end;

const SortDirection: array[0..1] of String = ('DESC', 'ASC');

implementation

{ TCardQuery }

constructor TCardQuery.Create(ATable: TTable; AID: Integer);
begin
  FTable := ATable;
  FID := AID;
end;

function TCardQuery.SelectQueryAsText: String;
var i: Integer;
begin
  Result := 'SELECT ';
  for i := 0 to High(FTable.Cols) - 1 do
    Result += FTable.Cols[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) = 0 then
    Result += FTable.Cols[High(FTable.Cols)].SQLName + ' ';
  for i := 0 to High(FTable.ForeignKeys) - 1 do
    Result += FTable.ForeignKeys[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) <> 0 then
    Result += FTable.ForeignKeys[High(FTable.ForeignKeys)].SQLName + ' ';
  Result += Format(' FROM %s WHERE %s = %d', [
    FTable.SQLName, FTable.PrimaryKey.SQLName, FID]);
end;

function TCardQuery.ComboboxQueryAsText(ACol: TCol): String;
var i: Integer;
begin
  Result := 'SELECT ' + ACol.Reference.Table.PrimaryKey.SQLName + ', ';
  for i := 0 to High(ACol.Reference.Table.Cols) - 1 do
    Result += ACol.Reference.Table.Cols[i].SQLName + ' || '' '' || ';
  Result += ACol.Reference.Table.Cols[High(ACol.Reference.Table.Cols)].SQLName;
  Result += ' AS Data FROM ' + ACol.Reference.Table.SQLName;
end;

function TCardQuery.InsertQueryAsText: String;
var i: Integer;
begin
  Result := 'INSERT INTO ' + FTable.SQLName + ' (';
  for i := 0 to High(FTable.Cols) - 1 do
    Result += FTable.Cols[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) = 0 then
    Result += FTable.Cols[High(FTable.Cols)].SQLName + ' ) ';
  for i := 0 to High(FTable.ForeignKeys) - 1 do
    Result += FTable.ForeignKeys[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) <> 0 then
    Result += FTable.ForeignKeys[High(FTable.ForeignKeys)].SQLName + ' ) ';
  Result += 'VALUES(';
  for i := 0 to High(FTable.Cols) - 1 do
    Result += ':' + FTable.Cols[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) = 0 then
    Result += ':' + FTable.Cols[High(FTable.Cols)].SQLName + ' )';
  for i := 0 to High(FTable.ForeignKeys) - 1 do
    Result += ':' + FTable.ForeignKeys[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) <> 0 then
    Result += ':' + FTable.ForeignKeys[High(FTable.ForeignKeys)].SQLName + ' )';
end;

function TCardQuery.UpdateQueryAsText: String;
var i: Integer;
begin
  Result := 'UPDATE ' + FTable.SQLName + ' SET ';
  for i := 0 to High(FTable.Cols) - 1 do
    Result += FTable.Cols[i].SQLName + '=:' + FTable.Cols[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) = 0 then
    Result += FTable.Cols[High(FTable.Cols)].SQLName + '=:' +
      FTable.Cols[High(FTable.Cols)].SQLName;
  for i := 0 to High(FTable.ForeignKeys) - 1 do
    Result += FTable.ForeignKeys[i].SQLName + '=:' +
      FTable.ForeignKeys[i].SQLName + ', ';
  if Length(FTable.ForeignKeys) <> 0 then
    Result += FTable.ForeignKeys[High(FTable.ForeignKeys)].SQLName + '=:' +
      FTable.ForeignKeys[High(FTable.ForeignKeys)].SQLName;
  Result += Format(' WHERE %s = %d', [FTable.PrimaryKey.SQLName, FID]);
end;

{ TDirectoryQuery }

function TDirectoryQuery.GetSelectAsText: String;
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

function TDirectoryQuery.SelectQueryAsText(ASortColumn: String;
  ASortDirection: String): String;
begin
  Result := Format('%s %s %s', [
    GetSelectAsText, GetFromAsText, GetFiltersAsText]);
  if (ASortColumn <> '') and (ASortDirection <> '') then
    Result += Format(' ORDER BY "%s" %s', [ASortColumn, ASortDirection]);
end;

function TDirectoryQuery.GetBaseTable: TTable;
begin
  Result := FTables[0];
end;

function TDirectoryQuery.GetFromAsText: String;
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

function TDirectoryQuery.GetFiltersAsText: String;
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

procedure TDirectoryQuery.SetFilters(AFilters: TFilterArray);
var i: Integer;
begin
  SetLength(FFilters, Length(AFilters));
  for i := 0 to High(AFilters) do
    FFilters[i] := AFilters[i];
end;

procedure TDirectoryQuery.SetCols;
var i, j: Integer;
begin
  for i := 0 to High(FTables) do
    for j := 0 to High(FTables[i].Cols) do
    begin
      SetLength(FCols, Length(FCols) + 1);
      FCols[High(FCols)] := FTables[i].Cols[j];
    end;
end;

procedure TDirectoryQuery.SetTables(ATable: TTable);
var
  i: Integer;
begin
  SetLength(FTables, Length(ATable.ForeignKeys) + 1);
  FTables[0] := ATable;
  for i := 1 to High(FTables) do
    FTables[i] := FTables[0].ForeignKeys[i - 1].Reference.Table;
end;

constructor TDirectoryQuery.Create(ATable: TTable; AFilterList: TFilterArray);
begin
  SetTables(ATable);
  SetFilters(AFilterList);
  SetCols;
end;

function TDirectoryQuery.DeleteQueryAsText: String;
begin
  Result := Format('DELETE FROM %s WHERE %s = :"%s"', [
    FTables[0].SQLName, FTables[0].PrimaryKey.SQLName,
    FTables[0].PrimaryKey.DisplayName]);
end;

end.

