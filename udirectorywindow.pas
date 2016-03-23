unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, PairSplitter, UMetadata,
  UQuery, UFilters, Grids;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    AddFilterBtn: TButton;
    ExecuteBtn: TButton;
    PairSplitter: TPairSplitter;
    PairSplitterUpperSide: TPairSplitterSide;
    PairSplitterLowerSide: TPairSplitterSide;
    RemoveFilterBtn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FilterSB: TScrollBox;
    FilterControlPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveFilterBtnClick(Sender: TObject);
  private
    FFilters: array of TFilter;
    FQuery: TQuery;
    procedure UpdateStatus(Sender: TObject);
  public
    CurrentTable: Integer;
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.FormShow(Sender: TObject);
var i: Integer;
begin
  Caption := Metadata.Tables[CurrentTable].DisplayName;
  SQLQuery.Close;
  DBGrid.Tag := 0;
  FQuery := TQuery.Create(CurrentTable, nil);
  SQLQuery.SQL.Text := Format('SELECT %s FROM %s', [
    FQuery.ColsAsText, FQuery.TablesAsText]);
  SQLQuery.Open;
end;

procedure TDirectoryForm.RemoveFilterBtnClick(Sender: TObject);
begin
  FreeAndNil(FFilters[High(FFilters)]);
  SetLength(FFilters, Length(FFilters) - 1);
  FilterSB.Tag := FilterSB.Tag - 1;
  if Length(FFilters) = 0 then
    RemoveFilterBtn.Enabled := False;
  ExecuteBtn.Enabled := True;
end;

procedure TDirectoryForm.UpdateStatus(Sender: TObject);
begin
  ExecuteBtn.Enabled := True;
end;

procedure TDirectoryForm.AddFilterBtnClick(Sender: TObject);
begin
  RemoveFilterBtn.Enabled := True;
  ExecuteBtn.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, Metadata.Tables[CurrentTable], FQuery.Cols);
  FFilters[High(FFilters)].OnFilterUpdate := @UpdateStatus;
end;

procedure TDirectoryForm.DBGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if Column.Tag = 1 then
    exit;
  Column.Tag := 1;
  Column.Width := FQuery.Cols[DBGrid.Tag].Width;
  DBGrid.Tag := DBGrid.Tag + 1;
end;

procedure TDirectoryForm.ExecuteBtnClick(Sender: TObject);
var i: Integer;
begin
  DBGrid.Tag := 0;
  ExecuteBtn.Enabled := False;
  SQLQuery.Close;
  FQuery.Free;
  FQuery := TQuery.Create(CurrentTable, FFilters);
  if Length(FFilters) = 0 then
    SQLQuery.SQL.Text := Format('SELECT %s FROM %s', [
      FQuery.ColsAsText, FQuery.TablesAsText])
  else
    SQLQuery.SQL.Text := Format('SELECT %s FROM %s WHERE %s', [
      FQuery.ColsAsText, FQuery.TablesAsText, FQuery.FiltersAsText]);
  SQLQuery.Prepare;
  for i := 0 to SQLQuery.Params.Count - 1 do
    SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
  try
    SQLQuery.Open;
  except
    on E: Exception do
    begin
      ShowMessage('An exception was raised:'#13#10 + E.Message + #13#10 +
        'Probably an unacceptable value was entered or an impossible action was' +
        ' selected.');
      SQLQuery.Close;
    end;
  end;
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

end.

