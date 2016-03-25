unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, PairSplitter, UMetadata,
  UQuery, UFilters, Grids, Buttons;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    AddFilterBtn: TBitBtn;
    ExecuteBtn: TBitBtn;
    ImageList: TImageList;
    PairSplitter: TPairSplitter;
    PairSplitterUpperSide: TPairSplitterSide;
    PairSplitterLowerSide: TPairSplitterSide;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FilterSB: TScrollBox;
    FilterControlPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFilters: array of TFilter;
    FQuery: TQuery;
    FLastColumnSorted: TColumn;
    procedure UpdateStatus;
    procedure RemoveFilter(Sender: TObject);
    procedure SetColWidth;
  public
    CurrentTable: Integer;
  end;

implementation

var
  AddGlyph, ApplyGlyph: TBitmap;

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.FormShow(Sender: TObject);
begin
  Caption := Metadata.Tables[CurrentTable].DisplayName;
  AddFilterBtn.Glyph := AddGlyph;
  ExecuteBtn.Glyph := ApplyGlyph;
  SQLQuery.Close;
  FQuery := TQuery.Create(CurrentTable, nil);
  SQLQuery.SQL.Text := FQuery.QueryAsText;
  SQLQuery.Open;
  SetColWidth;
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
  for i := 0 to High(FQuery.Cols) do
    DBGrid.Columns.Items[i].Width := FQuery.Cols[i].Width;
end;

procedure TDirectoryForm.AddFilterBtnClick(Sender: TObject);
begin
  ExecuteBtn.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, Metadata.Tables[CurrentTable], FQuery.Cols);
  FFilters[High(FFilters)].OnFilterUpdate := @UpdateStatus;
  FFilters[High(FFilters)].OnFilterRemove := @RemoveFilter;
end;

procedure TDirectoryForm.DBGridTitleClick(Column: TColumn);
const
  ImgArrUp = 0;
  ImgArrDown = 1;
var
  ASC_IndexName, DESC_IndexName: String;

  procedure UpdateIndexes;
  begin
    SQLQuery.IndexDefs.Updated := false;
    SQLQuery.IndexDefs.Update;
  end;

begin
  ASC_IndexName := 'ASC_' + Column.FieldName;
  DESC_IndexName := 'DESC_' + Column.FieldName;
  if SQLQuery.IndexDefs.IndexOf(ASC_IndexName) = -1 then
  begin
    SQLQuery.AddIndex(ASC_IndexName, Column.FieldName, []);
    UpdateIndexes;
  end;
  if SQLQuery.IndexDefs.IndexOf(DESC_IndexName) = -1 then
  begin
    SQLQuery.AddIndex(DESC_IndexName, Column.FieldName, [ixDescending]);
    UpdateIndexes;
  end;
  Column.Tag := not Column.Tag;
  if Boolean(Column.Tag) then
  begin
    Column.Title.ImageIndex := ImgArrUp;
    SQLQuery.IndexName := ASC_IndexName;
  end
  else
  begin
    Column.Title.ImageIndex := ImgArrDown;
    SQLQuery.IndexName := DESC_IndexName;
  end;
  if (FLastColumnSorted <> nil) and (FLastColumnSorted <> Column) then
    FLastColumnSorted.Title.ImageIndex := -1;
  FLastColumnSorted := Column;
  SetColWidth;
end;

procedure TDirectoryForm.ExecuteBtnClick(Sender: TObject);
var i: Integer;
begin
  ExecuteBtn.Enabled := False;
  SQLQuery.Close;
  FQuery.Free;
  FQuery := TQuery.Create(CurrentTable, FFilters);
  SQLQuery.SQL.Text := FQuery.QueryAsText;
  SQLQuery.Prepare;
  for i := 0 to SQLQuery.Params.Count - 1 do
    SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
  try
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
  SetColWidth;
  if FLastColumnSorted <> nil then
    FLastColumnSorted.Title.ImageIndex := FLastColumnSorted.Tag;
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
  ApplyGlyph := TBitmap.Create;
  ApplyGlyph.LoadFromFile('icons/Apply.bmp');
end.

