unit UCardWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbCtrls, ExtCtrls, ComCtrls, UMetadata, UQuery, UDB, StdCtrls, UNotification;

type

  TCardMode = (cmNew, cmEdit);

  TCardNotFilledException = class(Exception);

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
    FQuery: TCardQuery;
    FComboboxes: array of TDBLookupComboBox;
    FMode: TCardMode;
    procedure CreateLabel(ACol: TCol);
    procedure AddEdit(ACol: TCol);
    procedure AddCB(ACol: TCol);
    procedure GetID;
    procedure PrepareQuery;
    procedure Check;
    procedure Finish;
  public
    Table: TTable;
    ID: Integer;
    procedure Setup(ATable: TTable; AID: Integer = -1; AMode: TCardMode = cmNew);
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
  PrepareQuery;
  for i := 0 to High(Table.Cols) do
    AddEdit(Table.Cols[i]);
  for i := 0 to High(Table.ForeignKeys) do
    AddCB(Table.ForeignKeys[i]);
end;

procedure TCardWindow.OKBtnClick(Sender: TObject);
begin
  try
    Finish;
  except
    on E: Exception do
    begin
      MessageDlg('Error', E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
  Close;
end;

procedure TCardWindow.SaveBtnClick(Sender: TObject);
begin
  try
    Finish;
  except
    on E: Exception do
    begin
      MessageDlg('Error', E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
  if FMode = cmNew then
    GetID;
end;

procedure TCardWindow.CreateLabel(ACol: TCol);
var
  tl: TLabel;
begin
  tl := TLabel.Create(ScrollBox);
  with tl do
  begin
    Caption := ACol.DisplayName;
    Left := 20;
    Top := 20 + 50 * ScrollBox.Tag;
    Parent := ScrollBox;
  end;
end;

procedure TCardWindow.AddEdit(ACol: TCol);
var
  te: TDBEdit;
begin
  CreateLabel(ACol);
  te := TDBEdit.Create(ScrollBox);
  with te do
  begin
    DataSource := Self.DataSource;
    DataField := ACol.SQLName;
    Width := 400;
    Left := 150;
    Top := 20 + 50 * ScrollBox.Tag;
    Parent := ScrollBox;
  end;
  ScrollBox.Tag := ScrollBox.Tag + 1;
end;

procedure TCardWindow.AddCB(ACol: TCol);
var
  tcb: TDBLookupComboBox;
  tds: TDataSource;
  tsq: TSQLQuery;
begin
  CreateLabel(ACol);
  tsq := TSQLQuery.Create(ScrollBox);
  tsq.DataBase := UDB.DB.IBConnection;
  tsq.Transaction := SQLTransaction;
  tsq.SQL.Text := FQuery.ComboboxQueryAsText(ACol);
  tsq.Open;
  tds := TDataSource.Create(ScrollBox);
  tds.DataSet := tsq;
  tcb := TDBLookupComboBox.Create(ScrollBox);
  with tcb do
  begin
    DataSource := Self.DataSource;
    DataField := ACol.SQLName;
    ListSource := tds;
    ListField := 'Data';
    KeyField := ACol.Reference.SQLName;
    Style := csDropDownList;
    Left := 150;
    Top := 20 + 50 * ScrollBox.Tag;
    Width := 400;
    Parent := ScrollBox;
  end;
  SetLength(FComboboxes, Length(FComboboxes) + 1);
  FComboboxes[High(FComboboxes)] := tcb;
  ScrollBox.Tag := ScrollBox.Tag + 1;
end;

procedure TCardWindow.GetID;
var tq: TSQLQuery;
begin
  tq := TSQLQuery.Create(Self);
  tq.SQLTransaction := SQLTransaction;
  tq.DataBase := UDB.DB.IBConnection;
  tq.SQL.Text := Format('SELECT gen_id(%s, 0) AS ID FROM rdb$database',
    [Table.GeneratorName]);
  tq.Open;
  ID := tq.FieldByName('ID').AsInteger;
  FMode := cmEdit;
  tq.Free;
  FQuery.Free;
  PrepareQuery;
end;

procedure TCardWindow.PrepareQuery;
begin
  FQuery := TCardQuery.Create(Table, ID);
  with SQLQuery do
  begin;
    if Active then
      Close;
    SQL.Text := FQuery.SelectQueryAsText;
    UpdateSQL.Text := FQuery.UpdateQueryAsText;
    InsertSQL.Text := FQuery.InsertQueryAsText;
    Prepare;
    Open;
  end;
end;

procedure TCardWindow.Check;
var i: Integer;
begin
  for i := 0 to High(FComboboxes) do
    if FComboboxes[i].ItemIndex = -1 then
      raise TCardNotFilledException.Create('Please fill the card');
end;

procedure TCardWindow.Finish;
begin
  Check;
  SQLQuery.Edit;
  SQLQuery.Post;
  SQLTransaction.CommitRetaining;
  UDB.DB.SQLTransaction.CommitRetaining;
  OnDataUpdate;
end;

procedure TCardWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  RemoveCard(Self);
  SQLTransaction.Rollback;
  FQuery.Free;
end;

procedure TCardWindow.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TCardWindow.Setup(ATable: TTable; AID: Integer; AMode: TCardMode);
begin
  Table := ATable;
  ID := AID;
  FMode := AMode;
end;

end.

