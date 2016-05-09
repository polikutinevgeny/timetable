unit UConflictTreeWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, UConflicts, UMetadata, UQuery, sqldb, UNotification,
  UCardWindow, UDirectoryWindow, UFilters;

type

  { TConflictTreeWindow }

  TConflictTreeWindow = class(TForm)
    ImageList: TImageList;
    ButtonPnl: TPanel;
    RemoveMarksBtn: TSpeedButton;
    RefreshBtn: TSpeedButton;
    TreeView: TTreeView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure RemoveMarksBtnClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  private
    procedure Refresh;
  public
    procedure Mark(AConflictList: array of TConflict);
  end;

var
  ConflictTreeWindow: TConflictTreeWindow;

implementation

{$R *.lfm}

{ TConflictTreeWindow }

procedure TConflictTreeWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TConflictTreeWindow.FormCreate(Sender: TObject);
begin
  Refresh;
end;

procedure TConflictTreeWindow.RefreshBtnClick(Sender: TObject);
begin
  Refresh;
end;

procedure TConflictTreeWindow.RemoveMarksBtnClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to TreeView.Items.Count - 1 do
  begin
    TreeView.Items[i].ImageIndex := -1;
    TreeView.Items[i].SelectedIndex := -1;
  end;
end;

procedure TConflictTreeWindow.TreeViewDblClick(Sender: TObject);
var
  t: TCardWindow;
  f: TFilter;
  df: TDirectoryForm;
begin
  if TreeView.Selected <> nil then
  begin
    if TreeView.Selected.Level = 2 then
    begin
      t := CheckCardExistence(Metadata.TimetableTable, Integer(TreeView.Selected.Data));
      if t <> nil then
      begin
        t.BringToFront;
        Exit;
      end;
      t := TCardWindow.Create(Application);
      t.Setup(Metadata.TimetableTable, Integer(TreeView.Selected.Data), cmEdit);
      RegisterCard(t);
      t.Show;
    end
    else if TreeView.Selected.Level = 1 then
    begin
      df := TDirectoryForm.Create(Application, Metadata.TimetableTable);
      f := TFilter.Create(df.FilterSB, Metadata.TimetableTable, [
        Metadata.TimetableTable.PrimaryKey], False);
      f.SetupHiddenFilter(TConflict(TreeView.Selected.Data).ConflictedIDs);
      df.SetHiddenFilters([f]);
      df.Show;
    end;
  end;
end;

procedure TConflictTreeWindow.Refresh;
var i: Integer;
begin
  ConflictTypesContainer.RefreshConflicts;
  TreeView.Items.Clear;
  for i := 0 to High(ConflictTypesContainer.ConflictTypes) do
    ConflictTypesContainer.ConflictTypes[i].Display(TreeView, []);
end;

procedure TConflictTreeWindow.Mark(AConflictList: array of TConflict);
var i: Integer;
begin
  TreeView.Items.Clear;
  for i := 0 to High(ConflictTypesContainer.ConflictTypes) do
    ConflictTypesContainer.ConflictTypes[i].Display(TreeView, AConflictList);
end;

end.

