unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, Forms, Controls, Dialogs,
  DBGrids, ExtCtrls, PairSplitter, UMetadata, math, UCardWindow, UDB,
  UQuery, UFilters, Buttons, ComCtrls, UNotification;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    AddFilterBtn: TSpeedButton;
    ArrowIL: TImageList;
    ExecuteBtn: TSpeedButton;
    ToolBarIL: TImageList;
    PairSplitter: TPairSplitter;
    PairSplitterUpperSide: TPairSplitterSide;
    PairSplitterLowerSide: TPairSplitterSide;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FilterSB: TScrollBox;
    FilterControlPanel: TPanel;
    NavigatorPanel: TPanel;
    SQLQuery: TSQLQuery;
    NavigatorTB: TToolBar;
    AddBtn: TToolButton;
    EditBtn: TToolButton;
    RemoveBtn: TToolButton;
    procedure AddBtnClick(Sender: TObject);
    procedure AddFilterBtnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure EditBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
  private
    FFilters: array of TFilter;
    FQuery: TDirectoryQuery;
    FSortedColumn: TColumn;
    FHasHiddenFilters: Boolean;
    procedure UpdateStatus;
    procedure UpdateData;
    procedure RemoveFilter(Sender: TObject);
    procedure SetColWidth;
    procedure ExecuteQuery;
    procedure UpdateSortOrder(ATitle: String; ATag: Integer);
    procedure StartEdit;
  public
    CurrentTable: TTable;
    constructor Create(TheOwner: TComponent; ATable: TTable);
    procedure SetHiddenFilters(AFilters: array of TFilter);
  end;

implementation

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.FormShow(Sender: TObject);
begin
  RegisterDataUpdateListener(@UpdateData);
  Caption := CurrentTable.DisplayName;
  FHasHiddenFilters := Length(FFilters) > 0;
  ExecuteQuery;
  SetColWidth;
end;

procedure TDirectoryForm.RemoveBtnClick(Sender: TObject);
begin
  if MessageDlg('Delete?', 'Do you really want to delete this record?',
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    SQLQuery.Delete;
    UDB.DB.SQLTransaction.CommitRetaining;
    OnDataUpdate;
  end;
end;

procedure TDirectoryForm.UpdateStatus;
begin
  ExecuteBtn.Enabled := True;
end;

procedure TDirectoryForm.UpdateData;
var id: Integer;
begin
  id := SQLQuery.FieldValues[FQuery.BaseTable.PrimaryKey.DisplayName];
  SQLQuery.Refresh;
  SetColWidth;
  SQLQuery.First;
  while (SQLQuery.FieldValues[FQuery.BaseTable.PrimaryKey.DisplayName] <> id) and
    not SQLQuery.EOF do
    SQLQuery.Next;
  if SQLQuery.EOF then
    SQLQuery.First;
end;

procedure TDirectoryForm.RemoveFilter(Sender: TObject);
var i, j: Integer;
begin
  for i := 0 to High(FFilters) do
  begin
    if FFilters[i] = Sender then
    begin
      FilterSB.Tag := i;
      for j := i to High(FFilters) - 1 do
      begin
        FFilters[j] := FFilters[j + 1];
        FFilters[j].Draw(FilterSB);
      end;
      SetLength(FFilters, Length(FFilters) - 1);
      break;
    end;
  end;
end;

procedure TDirectoryForm.SetColWidth;
var i: Integer;
begin
  DBGrid.Columns.Items[0].Width := FQuery.BaseTable.PrimaryKey.Width;
  for i := 0 to High(FQuery.Cols) do
    DBGrid.Columns.Items[i + 1].Width := FQuery.Cols[i].Width;
end;

procedure TDirectoryForm.ExecuteQuery; //make this more readable
var
  i, t: Integer;
  s: String;
  sorted: Boolean;
begin
  sorted := FSortedColumn <> nil;
  if sorted then
  begin
    t := FSortedColumn.Tag;
    s := FSortedColumn.FieldName;
  end;
  ExecuteBtn.Enabled := False;
  SQLQuery.Close;
  FQuery.Free;
  FQuery := TDirectoryQuery.Create(CurrentTable, FFilters,
    TDirectoryQuery.GetFullColList(CurrentTable, True));
  try
    if sorted then
      SQLQuery.SQL.Text := FQuery.SelectQueryAsText(s, SortDirection[t])
    else
      SQLQuery.SQL.Text := FQuery.SelectQueryAsText;
    SQLQuery.DeleteSQL.Text := FQuery.DeleteQueryAsText;
    SQLQuery.Prepare;
    for i := 0 to SQLQuery.Params.Count - 1 do
      SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
    SQLQuery.Open;
  except
    on E: Exception do
    begin
      MessageDlg('Error', 'An exception was raised:'#13#10 + E.Message + #13#10 +
        'Probably an unacceptable value was entered or an impossible action was' +
        ' selected.', mtError, [mbOK], 0);
      SQLQuery.Close;
      Exit;
    end;
  end;
  if sorted then
    UpdateSortOrder(s, t);
  SetColWidth;
end;

procedure TDirectoryForm.UpdateSortOrder(ATitle: String; ATag: Integer);
const
  ImgArrUp = 0;
  ImgArrDown = 1;
var i: Integer;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
  begin
    if DBGrid.Columns.Items[i].FieldName = ATitle then
    begin
      FSortedColumn := DBGrid.Columns.Items[i];
      FSortedColumn.Tag := ATag;
      if Boolean(ATag) then
        FSortedColumn.Title.ImageIndex := ImgArrUp
      else
        FSortedColumn.Title.ImageIndex := ImgArrDown;
      Break;
    end;
  end;
end;

procedure TDirectoryForm.StartEdit;
var
  t: TCardWindow;
  id: Integer;
begin
  id := SQLQuery.FieldValues[FQuery.BaseTable.PrimaryKey.DisplayName];
  t := CheckCardExistence(CurrentTable, id);
  if t <> nil then
  begin
    t.BringToFront;
    Exit;
  end;
  t := TCardWindow.Create(Application);
  t.Setup(CurrentTable, id, cmEdit);
  RegisterCard(t);
  t.Show;
end;

constructor TDirectoryForm.Create(TheOwner: TComponent; ATable: TTable);
begin
  inherited Create(TheOwner);
  CurrentTable := ATable;
end;

procedure TDirectoryForm.SetHiddenFilters(AFilters: array of TFilter);
var i: Integer;
begin
  SetLength(FFilters, Length(AFilters));
  for i := 0 to High(AFilters) do
    FFilters[i] := AFilters[i];
end;

procedure TDirectoryForm.AddFilterBtnClick(Sender: TObject);
begin
  ExecuteBtn.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, CurrentTable, FQuery.Cols);
  FFilters[High(FFilters)].OnFilterUpdate := @UpdateStatus;
  FFilters[High(FFilters)].OnFilterRemove := @RemoveFilter;
end;

procedure TDirectoryForm.DBGridDblClick(Sender: TObject);
begin
  StartEdit;
end;

procedure TDirectoryForm.AddBtnClick(Sender: TObject);
var
  t: TCardWindow;
begin
  t := TCardWindow.Create(Application);
  t.Setup(CurrentTable);
  RegisterCard(t);
  t.Show;
end;

procedure TDirectoryForm.DBGridTitleClick(Column: TColumn);
begin
  Column.Tag := ifthen(Column.Tag = 0, 1, 0);
  if (FSortedColumn <> nil) and (FSortedColumn <> Column) then
    FSortedColumn.Title.ImageIndex := -1;
  FSortedColumn := Column;
  FSortedColumn.FieldName := Column.FieldName;
  ExecuteQuery;
end;

procedure TDirectoryForm.EditBtnClick(Sender: TObject);
begin
  StartEdit;
end;

procedure TDirectoryForm.ExecuteBtnClick(Sender: TObject);
begin
  ExecuteQuery;
end;

procedure TDirectoryForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  RemoveDataUpdateListener(@UpdateData);
end;

procedure TDirectoryForm.FormDestroy(Sender: TObject);
var i: Integer;
begin
  FQuery.Free;
  for i := 0 to High(FFilters) do
    FFilters[i].Free;
end;

end.

