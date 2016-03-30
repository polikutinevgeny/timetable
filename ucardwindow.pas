unit UCardWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbCtrls, ExtCtrls, ComCtrls, UMetadata, UQuery, UDB, StdCtrls;

type

  { TCardWindow }

  TCardWindow = class(TForm)
    DataSource: TDataSource;
    ScrollBox: TScrollBox;
    ToolBarIL: TImageList;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    ToolBar: TToolBar;
    OKBtn: TToolButton;
    SaveBtn: TToolButton;
    CancelBtn: TToolButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    function MakeQuery: String;
    function MakeComboboxQuery(ACol: TCol): String;
    function MakeInsertQuery: String;
    function MakeUpdateQuery: String;
    procedure AddEdit(ACol: TCol);
    procedure AddCB(ACol: TCol);
  public
    Table: TTable;
    ID: Integer;
    Mode: TMode;
    procedure Setup(ATable: TTable; AID: Integer);
  end;

  procedure RegisterCard(ACard: TCardWindow);
  procedure RemoveCard(ACard: TCardWindow);
  function CheckCardExistence(ATable: TTable; AID: Integer): TCardWindow;

var
  Cards: array of TCardWindow;

implementation

procedure RegisterCard(ACard: TCardWindow);
begin
  SetLength(Cards, Length(Cards) + 1);
  Cards[High(Cards)] := ACard;
end;

procedure RemoveCard(ACard: TCardWindow);
var i, j: Integer;
begin
  for i := 0 to High(Cards) do
  begin
    if Cards[i] = ACard then
    begin
      for j := i + 1 to High(Cards) do
        Cards[j - 1] := Cards[j];
      SetLength(Cards, Length(Cards) - 1);
      Exit;
    end;
  end;
  raise Exception.Create('Error! An attempt to remove not existing card was made!');
end;

function CheckCardExistence(ATable: TTable; AID: Integer): TCardWindow;
var i: Integer;
begin
  for i := 0 to High(Cards) do
  begin
    if (Cards[i].Table = ATable) and (Cards[i].ID = AID) then
      Exit(Cards[i]);
  end;
  Exit(nil);
end;

{$R *.lfm}

{ TCardWindow }

procedure TCardWindow.FormShow(Sender: TObject);
var i: Integer;
begin
  SQLQuery.SQL.Text := MakeQuery;
  SQLQuery.UpdateSQL.Text := MakeUpdateQuery;
  SQLQuery.InsertSQL.Text := MakeInsertQuery;
  SQLQuery.Prepare;
  SQLQuery.Open;
  SQLQuery.UpdateMode := upWhereAll;
  for i := 0 to High(Table.Cols) do
    AddEdit(Table.Cols[i]);
  for i := 0 to High(Table.ForeignKeys) do
    AddCB(Table.ForeignKeys[i]);
end;

procedure TCardWindow.OKBtnClick(Sender: TObject);
begin
  SQLQuery.Edit;
  SQLQuery.Post;
  SQLQuery.ApplyUpdates;
  SQLTransaction.Commit;
  UDB.DB.SQLTransaction.CommitRetaining;
  Close;
end;

procedure TCardWindow.SaveBtnClick(Sender: TObject);
begin
  SQLQuery.Edit;
  SQLQuery.Post;
  SQLQuery.ApplyUpdates;
  SQLTransaction.CommitRetaining;
  UDB.DB.SQLTransaction.CommitRetaining;
end;

function TCardWindow.MakeQuery: String;
var i: Integer;
begin
  Result := 'SELECT ';
  for i := 0 to High(Table.Cols) - 1 do
    Result += Table.Cols[i].SQLName + ', ';
  if Length(Table.ForeignKeys) = 0 then
    Result += Table.Cols[High(Table.Cols)].SQLName + ' ';
  for i := 0 to High(Table.ForeignKeys) - 1 do
    Result += Table.ForeignKeys[i].SQLName + ', ';
  if Length(Table.ForeignKeys) <> 0 then
    Result += Table.ForeignKeys[High(Table.ForeignKeys)].SQLName + ' ';
  Result += 'FROM ' + Table.SQLName + ' ';
  Result += 'WHERE ' + Table.PrimaryKey.SQLName + ' = ' + IntToStr(ID)
end;

function TCardWindow.MakeComboboxQuery(ACol: TCol): String;
var i: Integer;
begin
  Result := 'SELECT ' + ACol.Reference.Table.PrimaryKey.SQLName + ', ';
  for i := 0 to High(ACol.Reference.Table.Cols) - 1 do
    Result += ACol.Reference.Table.Cols[i].SQLName + ' || '' '' || ';
  Result += ACol.Reference.Table.Cols[High(ACol.Reference.Table.Cols)].SQLName;
  Result += ' AS Data ';
  Result += ' FROM ' + ACol.Reference.Table.SQLName;
end;

function TCardWindow.MakeInsertQuery: String;
var i: Integer;
begin
  Result := 'INSERT INTO ' + Table.SQLName + ' (';
  for i := 0 to High(Table.Cols) - 1 do
    Result += Table.Cols[i].SQLName + ', ';
  if Length(Table.ForeignKeys) = 0 then
    Result += Table.Cols[High(Table.Cols)].SQLName + ' ) ';
  for i := 0 to High(Table.ForeignKeys) - 1 do
    Result += Table.ForeignKeys[i].SQLName + ', ';
  if Length(Table.ForeignKeys) <> 0 then
    Result += Table.ForeignKeys[High(Table.ForeignKeys)].SQLName + ' ) ';
  Result += 'VALUES(';
  for i := 0 to High(Table.Cols) - 1 do
    Result += ':' + Table.Cols[i].SQLName + ', ';
  if Length(Table.ForeignKeys) = 0 then
    Result += ':' + Table.Cols[High(Table.Cols)].SQLName + ' )';
  for i := 0 to High(Table.ForeignKeys) - 1 do
    Result += ':' + Table.ForeignKeys[i].SQLName + ', ';
  if Length(Table.ForeignKeys) <> 0 then
    Result += ':' + Table.ForeignKeys[High(Table.ForeignKeys)].SQLName + ' )';
end;

function TCardWindow.MakeUpdateQuery: String;
var i: Integer;
begin
  Result := 'UPDATE ' + Table.SQLName + ' SET ';
  for i := 0 to High(Table.Cols) - 1 do
    Result += Table.Cols[i].SQLName + '=:' + Table.Cols[i].SQLName + ', ';
  if Length(Table.ForeignKeys) = 0 then
    Result += Table.Cols[High(Table.Cols)].SQLName + '=:' +
      Table.Cols[High(Table.Cols)].SQLName;
  for i := 0 to High(Table.ForeignKeys) - 1 do
    Result += Table.ForeignKeys[i].SQLName + '=:' +
      Table.ForeignKeys[i].SQLName + ', ';
  if Length(Table.ForeignKeys) <> 0 then
    Result += Table.ForeignKeys[High(Table.ForeignKeys)].SQLName + '=:' +
      Table.ForeignKeys[High(Table.ForeignKeys)].SQLName;
  Result += ' WHERE ' + Table.PrimaryKey.SQLName + ' = ' + IntToStr(ID);
end;

procedure TCardWindow.AddEdit(ACol: TCol);
var
  te: TDBEdit;
  tl: TLabel;
begin
  tl := TLabel.Create(ScrollBox);
  tl.Caption := ACol.DisplayName;
  tl.Left := 20;
  tl.Top := 20 + 50 * ScrollBox.Tag;
  tl.Parent := ScrollBox;
  te := TDBEdit.Create(ScrollBox);
  te.DataSource := DataSource;
  te.DataField := ACol.SQLName;
  te.Width := 400;
  te.Left := 150;
  te.Top := 20 + 50 * ScrollBox.Tag;
  te.Parent := ScrollBox;
  ScrollBox.Tag := ScrollBox.Tag + 1;
end;

procedure TCardWindow.AddCB(ACol: TCol);
var
  tcb: TDBLookupComboBox;
  tds: TDataSource;
  tsq: TSQLQuery;
  tl: TLabel;
begin
  tsq := TSQLQuery.Create(ScrollBox);
  tsq.DataBase := UDB.DB.IBConnection;
  tsq.Transaction := SQLTransaction;
  tds := TDataSource.Create(ScrollBox);
  tds.DataSet := tsq;
  tsq.SQL.Text := MakeComboboxQuery(ACol);
  tsq.Open;
  tl := TLabel.Create(ScrollBox);
  tl.Caption := ACol.DisplayName;
  tl.Left := 20;
  tl.Top := 20 + 50 * ScrollBox.Tag;
  tl.Parent := ScrollBox;
  tcb := TDBLookupComboBox.Create(ScrollBox);
  tcb.DataSource := DataSource;
  tcb.DataField := ACol.SQLName;
  tcb.ListSource := tds;
  tcb.ListField := 'Data';
  tcb.KeyField := ACol.Reference.SQLName;
  tcb.Style := csDropDownList;
  tcb.Left := 150;
  tcb.Top := 20 + 50 * ScrollBox.Tag;
  tcb.Width := 400;
  tcb.Parent := ScrollBox;
  ScrollBox.Tag := ScrollBox.Tag + 1;
end;

procedure TCardWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  RemoveCard(Self);
  if SQLTransaction.Active then
    SQLTransaction.Rollback;
end;

procedure TCardWindow.CancelBtnClick(Sender: TObject);
begin
  SQLTransaction.Rollback;
  Close;
  //UDB.DB.SQLTransaction.CommitRetaining;
end;

procedure TCardWindow.Setup(ATable: TTable; AID: Integer);
begin
  Table := ATable;
  ID := AID;
end;

end.

