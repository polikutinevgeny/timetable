unit USQLQueryConstructor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UFilters;

type

  { TSQLQueryComponent }

  TSQLQueryComponent = class
    public
      Table: TTable;
      Columns: array of TCol;
      procedure AddColumn(AColumn: TCol);
      constructor Create(ATable: TTable);
  end;

  { TSQLQueryConstructor }

  TSQLQueryConstructor = class
    public
      function CreateQuery(
        ATableNumber: Integer; AFilterList: array of TFilter): String;
  end;

var
  SQLQueryConstructor: TSQLQueryConstructor;

implementation

{ TSQLQueryComponent }

procedure TSQLQueryComponent.AddColumn(AColumn: TCol);
begin
  SetLength(Columns, Length(Columns) + 1);
  Columns[High(Columns)] := AColumn;
end;

constructor TSQLQueryComponent.Create(ATable: TTable);
begin
  Table := ATable;
end;

{ TSQLQueryConstructor }

function TSQLQueryConstructor.CreateQuery(ATableNumber: Integer;
  AFilterList: array of TFilter): String;
var
  i, j: Integer;
  qc: array of TSQLQueryComponent;
  t1, t2: TTable;
  sel_str, join_str: String;
  //In zero element the current table is stored
  //In others other tables are stored (if exist)
begin
  SetLength(qc, 1);
  t1 := Metadata.Tables[ATableNumber];
  qc[0] := TSQLQueryComponent.Create(t1);
  join_str := '';
  for i := 0 to High(t1.Cols) do
  begin
    if t1.Cols[i].Relationship = nil then
    begin
      qc[0].AddColumn(t1.Cols[i]);
    end
    else
    begin
      t2 := t1.Cols[i].Relationship.Table;
      SetLength(qc, Length(qc) + 1);
      qc[High(qc)] := TSQLQueryComponent.Create(t2);
      for j := 0 to High(t2.Cols) do
      begin
        if t2.Cols[j] = t1.Cols[i].Relationship.Col then
          continue;
        qc[High(qc)].AddColumn(t2.Cols[j]);
      end;
      join_str += Format('INNER JOIN %s ON %s.%s = %s.%s ', [
        t2.SQLName, t1.SQLName, t1.Cols[i].SQLName, t2.SQLName,
        t1.Cols[i].Relationship.Col.SQLName]);
    end;
  end;
  sel_str := 'SELECT ';
  for i := 0 to High(qc) do
  begin
    for j := 0 to High(qc[i].Columns) do
    begin
      sel_str += Format('%s.%s AS "%s", ',
        [qc[i].Table.SQLName, qc[i].Columns[j].SQLName,
        qc[i].Columns[j].DisplayName]);
    end;
  end;
  sel_str[High(sel_str) - 1] := ' ';
  sel_str += Format('FROM %s ', [qc[0].Table.SQLName]);
  Result := sel_str + join_str;
  //Somebody must free memory and add filters!!!
end;

initialization
  SQLQueryConstructor := TSQLQueryConstructor.Create;
end.

