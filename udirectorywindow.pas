unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, PairSplitter, UMetadata, math, UCardWindow,
  UQuery, UFilters, Grids, Buttons, DbCtrls, ComCtrls;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    AddFilterBtn: TBitBtn;
    ExecuteBtn: TBitBtn;
    ArrowIL: TImageList;
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
    procedure DBGridTitleClick(Column: TColumn);
    procedure EditBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
  private
    FFilters: array of TFilter;
    FQuery: TQuery;
    FSortedColumn: TColumn;
    procedure UpdateStatus;
    procedure RemoveFilter(Sender: TObject);
    procedure SetColWidth;
    procedure ExecuteQuery;
    procedure UpdateSortOrder(ATitle: String; ATag: Integer);
  public
    CurrentTable: TTable;
  end;

implementation

var
  AddGlyph, ApplyGlyph: TBitmap;

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.FormShow(Sender: TObject);
begin
  Caption := CurrentTable.DisplayName;
  AddFilterBtn.Glyph := AddGlyph;
  ExecuteBtn.Glyph := ApplyGlyph;
  SQLQuery.Close;
  FQuery := TQuery.Create(CurrentTable, nil);
  SQLQuery.SQL.Text := FQuery.QueryAsText;
  SQLQuery.Open;
  SetColWidth;
end;

procedure TDirectoryForm.RemoveBtnClick(Sender: TObject);
begin

end;

procedure TDirectoryForm.UpdateStatus;
begin
  ExecuteBtn.Enabled := True;
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
  for i := 1 to High(FQuery.Cols) do
    DBGrid.Columns.Items[i].Width := FQuery.Cols[i - 1].Width;
end;

procedure TDirectoryForm.ExecuteQuery;
const SortDirection: array[0..1] of String = ('DESC', 'ASC');
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
  FQuery := TQuery.Create(CurrentTable, FFilters);
  try
    SQLQuery.SQL.Text := FQuery.QueryAsText;
    if sorted then
      SQLQuery.SQL.Text := SQLQuery.SQL.Text + ' ORDER BY "' +
        s + '" ' + SortDirection[t] + ';';
    SQLQuery.Prepare;
    for i := 0 to SQLQuery.Params.Count - 1 do
      SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
    SQLQuery.Open;
    if sorted then
      UpdateSortOrder(s, t);
    SetColWidth;
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
      if Boolean(FSortedColumn.Tag) then
        FSortedColumn.Title.ImageIndex := ImgArrUp
      else
        FSortedColumn.Title.ImageIndex := ImgArrDown;
      Break;
    end;
  end;
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

procedure TDirectoryForm.AddBtnClick(Sender: TObject);
var
  t: TCardWindow;
begin
  t := TCardWindow.Create(Application);
  t.Setup(CurrentTable, -1);
  t.Caption := 'Add - ' + CurrentTable.DisplayName;
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
var
  t: TCardWindow;
  id: Integer;
begin
  id := DBGrid.DataSource.DataSet.FieldValues[FQuery.BaseTable.PrimaryKey.DisplayName];
  t := CheckCardExistence(CurrentTable, id);
  if t <> nil then
  begin
    t.BringToFront;
    Exit;
  end;
  t := TCardWindow.Create(Application);
  t.Setup(CurrentTable, id);
  t.Caption := 'Edit - ' + CurrentTable.DisplayName;
  RegisterCard(t);
  t.Show;
end;

procedure TDirectoryForm.ExecuteBtnClick(Sender: TObject);
begin
  ExecuteQuery;
end;

procedure TDirectoryForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDirectoryForm.FormDestroy(Sender: TObject);
var i: Integer;
begin
  FQuery.Free;
  for i := 0 to High(FFilters) do
    FFilters[i].Free;
end;

initialization
  AddGlyph := TBitmap.Create;
  AddGlyph.LoadFromFile('icons/Add.bmp');
  AddGlyph.TransparentColor := clWhite;
  AddGlyph.Transparent := True;
  ApplyGlyph := TBitmap.Create;
  ApplyGlyph.LoadFromFile('icons/Apply.bmp');
  ApplyGlyph.TransparentColor := clWhite;
  ApplyGlyph.Transparent := True;
end.

