unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UFilters, Dialogs;

type

  TStringArray = array of String;

  { TBaseViewingQuery }

  TBaseViewingQuery = class abstract
    private
      FTables: TTableArray;
      FCols: TColArray;
      FFilters: TFilterArray;
      function GetBaseTable: TTable;
      function GetFromAsText: String;
      function GetFiltersAsText: String;
      procedure SetFilters(AFilters: TFilterArray);
      procedure SetCols(AColList: TColArray);
      procedure SetTables(ATable: TTable);
    public
      constructor Create(ATable: TTable; AFilterList: TFilterArray;
        AColList: TColArray);
      property BaseTable: TTable read GetBaseTable;
      property Cols: TColArray read FCols;
  end;

  { TDirectoryQuery }

  TDirectoryQuery = class(TBaseViewingQuery)
    private
      function GetSelectAsText: String;
    public
      function SelectQueryAsText(ASortColumn: String = '';
        ASortDirection: String = ''): String;
      function DeleteQueryAsText: String;
      class function GetFullColList(ATable: TTable): TColArray;
  end;

  { TTimetableQuery }

  TTimetableQuery = class(TBaseViewingQuery)
    private
      function GetSelectAsText: String;
    public
      function SelectQueryAsText(AVertColumn: TCol; AHorizColumn: TCol): String;
      class function GetFullColList(ATable: TTable): TColArray;
      class function GetValuesListQuery(ATable: TTable; ACol: TCol): String;
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

{ TBaseViewingQuery }

function TBaseViewingQuery.GetBaseTable: TTable;
begin
  Result := FTables[0];
end;

function TBaseViewingQuery.GetFromAsText: String;
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

function TBaseViewingQuery.GetFiltersAsText: String;
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
    if FFilters[i].Visible then
      Result += Format('%s.%s %s %s ', [
        FFilters[i].Column.Table.SQLName, FFilters[i].Column.SQLName,
        FFilters[i].Action, ':p' + IntToStr(i)])
    else
      Result += Format('%s %s %s ', [FFilters[i].Column.SQLName,
        FFilters[i].Action, ':p' + IntToStr(i)]);
    if i < High(FFilters) then
      Result += 'AND ';
  end;
end;

procedure TBaseViewingQuery.SetFilters(AFilters: TFilterArray);
var i: Integer;
begin
  SetLength(FFilters, Length(AFilters));
  for i := 0 to High(AFilters) do
    FFilters[i] := AFilters[i];
end;

procedure TBaseViewingQuery.SetCols(AColList: TColArray);
var i: Integer;
begin
  SetLength(FCols, Length(AColList));
  for i := 0 to High(FCols) do
    FCols[i] := AColList[i];
end;

procedure TBaseViewingQuery.SetTables(ATable: TTable);
var i: Integer;
begin
  SetLength(FTables, Length(ATable.ForeignKeys) + 1);
  FTables[0] := ATable;
  for i := 1 to High(FTables) do
    FTables[i] := FTables[0].ForeignKeys[i - 1].Reference.Table;
end;

constructor TBaseViewingQuery.Create(ATable: TTable; AFilterList: TFilterArray;
  AColList: TColArray);
begin
  SetTables(ATable);
  SetFilters(AFilterList);
  SetCols(AColList);
end;

{ TTimetableQuery }

function TTimetableQuery.GetSelectAsText: String;
var i: Integer;
begin
  Result := Format('SELECT %s.%s AS "%s",', [
    FTables[0].SQLName, FTables[0].PrimaryKey.SQLName,
    FTables[0].PrimaryKey.DisplayName]);
  for i := 0 to High(FCols) - 1 do
  begin
    if FCols[i].Real then
      Result += FCols[i].Table.SQLName;
    Result += Format('%s AS "%s", ', [FCols[i].SQLName, FCols[i].DisplayName]);
  end;
  if FCols[High(FCols)].Real then
    Result += FCols[High(FCols)].Table.SQLName;
  Result += Format('%s AS "%s" ', [FCols[High(FCols)].SQLName,
    FCols[High(FCols)].DisplayName]);
end;

function TTimetableQuery.SelectQueryAsText(AVertColumn: TCol; AHorizColumn: TCol
  ): String;
begin
  Result := Format('%s %s %s ORDER BY ', [
    GetSelectAsText, GetFromAsText, GetFiltersAsText]);
  if AVertColumn.Table.SortKey <> nil then
    Result += AVertColumn.Table.SortKey.Table.SQLName + '.' +
      AVertColumn.Table.SortKey.SQLName + ', '
  else
    Result += '"' + AVertColumn.DisplayName + '", ';
  if AHorizColumn.Table.SortKey <> nil then
    Result += AHorizColumn.Table.SortKey.Table.SQLName + '.' +
      AHorizColumn.Table.SortKey.SQLName
  else
    Result += '"' + AHorizColumn.DisplayName + '"';
end;

class function TTimetableQuery.GetFullColList(ATable: TTable): TColArray;
var
  i, j, w: Integer;
  s: String;
begin
  SetLength(Result, Length(ATable.Cols) + Length(ATable.ForeignKeys));
  for i := 0 to High(ATable.Cols) do
    Result[i] := ATable.Cols[i];
  for i := 0 to High(ATable.ForeignKeys) do
    with ATable.ForeignKeys[i] do
    begin
      s := '';
      w := 0;
      for j := 0 to High(Reference.Table.Cols) - 1 do
      begin
        s += Format('%s.%s || '' '' || ', [
          Reference.Table.SQLName,
          Reference.Table.Cols[j].SQLName]);
        w += Reference.Table.Cols[j].Width;
      end;
      s += Format('%s.%s', [Reference.Table.SQLName,
        Reference.Table.Cols[High(Reference.Table.Cols)].SQLName]);
      w += Reference.Table.Cols[High(Reference.Table.Cols)].Width;
      Result[i] := TCol.Create(s, DisplayName, w, False, False);
      Result[i].Table := Reference.Table;
    end;
end;

class function TTimetableQuery.GetValuesListQuery(ATable: TTable; ACol: TCol
  ): String;
begin
  Result := 'SELECT ' + ACol.SQLName;
  Result += ' AS Data FROM ' + ACol.Table.SQLName;
  Result += ' ORDER BY ';
  if ACol.Table.SortKey <> nil then
    Result += ACol.Table.SQLName + '.' + ACol.Table.SortKey.SQLName
  else
    Result += 'Data';
end;

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
var i: Integer;
begin
  Result := Format('SELECT %s.%s AS "%s",', [
    FTables[0].SQLName, FTables[0].PrimaryKey.SQLName,
    FTables[0].PrimaryKey.DisplayName]);
  for i := 0 to High(FCols) - 1 do
    Result += Format('%s.%s AS "%s",', [
      FCols[i].Table.SQLName, FCols[i].SQLName,
      FCols[i].DisplayName]);
  Result += Format('%s.%s AS "%s" ', [
      FCols[High(FCols)].Table.SQLName, FCols[High(FCols)].SQLName,
      FCols[High(FCols)].DisplayName]);
end;

function TDirectoryQuery.SelectQueryAsText(ASortColumn: String;
  ASortDirection: String): String;
begin
  Result := Format('%s %s %s', [
    GetSelectAsText, GetFromAsText, GetFiltersAsText]);
  if (ASortColumn <> '') and (ASortDirection <> '') then
    Result += Format(' ORDER BY "%s" %s', [ASortColumn, ASortDirection]);
end;

function TDirectoryQuery.DeleteQueryAsText: String;
begin
  Result := Format('DELETE FROM %s WHERE %s = :"%s"', [
    FTables[0].SQLName, FTables[0].PrimaryKey.SQLName,
    FTables[0].PrimaryKey.DisplayName]);
end;

class function TDirectoryQuery.GetFullColList(ATable: TTable): TColArray;
var i, j: Integer;
begin
  SetLength(Result, Length(ATable.Cols));
  for i := 0 to High(ATable.Cols) do
    Result[i] := ATable.Cols[i];
  for i := 0 to High(ATable.ForeignKeys) do
    for j := 0 to High(ATable.ForeignKeys[i].Reference.Table.Cols) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := ATable.ForeignKeys[i].Reference.Table.Cols[j];
    end;
end;

end.

