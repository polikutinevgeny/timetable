unit UTimetableWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  LCLIntf, LCLType, ExtCtrls, StdCtrls, Buttons, PairSplitter, CheckLst, UQuery,
  UMetadata, sqldb, UFilters, math, UDirectoryWindow, UCardWindow, UDB, UNotification;

type

  TBooleanArray = array of Boolean;

  { TTimetableWindow }

  TTimetableWindow = class(TForm)
    ApplyBtn: TSpeedButton;
    DisplayedFieldsCLB: TCheckListBox;
    DisplayedNamesCLB: TCheckListBox;
    HideEmptyCB: TCheckBox;
    HorizontalLbl: TLabel;
    FieldSelectionLbl: TLabel;
    NameSelelctionLbl: TLabel;
    UtilitySQLQuery: TSQLQuery;
    VerticalLbl: TLabel;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    FilterControlPanel: TPanel;
    RowColControlPanel: TPanel;
    FilterSB: TScrollBox;
    AddFilterBtn: TSpeedButton;
    SQLQuery: TSQLQuery;
    VerticalCB: TComboBox;
    HorizontalCB: TComboBox;
    OptionsPanel: TPanel;
    TimetableDG: TDrawGrid;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure DisplayedFieldsCLBClickCheck(Sender: TObject);
    procedure DisplayedNamesCLBClickCheck(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HideEmptyCBChange(Sender: TObject);
    procedure HorizontalCBChange(Sender: TObject);
    procedure TimetableDGDblClick(Sender: TObject);
    procedure TimetableDGDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TimetableDGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TimetableDGEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TimetableDGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimetableDGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimetableDGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimetableDGStartDrag(Sender: TObject; var DragObject: TDragObject
      );
    procedure VerticalCBChange(Sender: TObject);
  private
    FQuery: TTimetableQuery;
    FColList: TColArray;
    FRealColList: TColArray;
    FCols: TStringArray;
    FRows: TStringArray;
    FColIDs: TStringArray;
    FRowIDs: TStringArray;
    FData: array of array of array of array of String;
    FIDs: array of array of array of String;
    FShowAllButton: TRect;
    FAddButton: TRect;
    FDeleteBtns: array of TRect;
    FEditBtns: array of TRect;
    FDragBtns: array of TRect;
    FExpandTriangles: array of array of HRGN;
    FFilters: array of TFilter;
    FRecordHeight: Integer;
    FOpenInNewWindowImage: TPortableNetworkGraphic;
    FAddImage: TPortableNetworkGraphic;
    FDeleteImage: TPortableNetworkGraphic;
    FEditImage: TPortableNetworkGraphic;
    FDragImage: TPortableNetworkGraphic;
    FTextStyle: TTextStyle;
    FCurrentCell: TPoint;
    FDraggedID: String;
    procedure AssignData(var fc: TBooleanArray; var fr: TBooleanArray);
    procedure DeleteEmpty(const fc: TBooleanArray; const fr: TBooleanArray);
    procedure DrawNewWindowBtn(ALeft: Integer; ATop: Integer);
    procedure DrawAddBtn(ALeft: Integer; ATop: Integer);
    procedure DrawTriangle(aRect: TRect; aRow: Integer; aCol: Integer);
    procedure DrawDragBtn(ALeft: Integer; ATop: Integer);
    procedure DrawEditBtn(ALeft: Integer; ATop: Integer);
    procedure DrawRemoveBtn(ALeft: Integer; ATop: Integer);
    procedure Expand(ARow: Integer; ACol: Integer);
    procedure OpenAsDirectory(ARow: Integer; ACol: Integer);
    procedure AddRecord(ARow: Integer; ACol: Integer);
    procedure EditRecord(ARow: Integer; ACol: Integer; AIndex: Integer);
    procedure DeleteRecord(ARow: Integer; ACol: Integer; AIndex: Integer);
    procedure SetupCBs;
    procedure UpdateStatus;
    procedure RemoveFilter(Sender: TObject);
    procedure SetupData(GetData: Boolean = True);
    procedure SetupFixed(var ANames: TStringArray; ACombobox: TComboBox;
      var AIDs: TStringArray);
    procedure SetupGrid;
    procedure SelectData;
    procedure UpdateData;
  public
  end;

var
  TimetableWindow: TTimetableWindow;

implementation

const
  BorderMargin = 2;
  RightMargin = 25;
  ButtonSize = 25;

{$R *.lfm}

{ TTimetableWindow }

procedure TTimetableWindow.TimetableDGDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  i, j, w: Integer;
  full: Boolean;
begin
  Canvas.FillRect(aRect);
  DeleteObject(FExpandTriangles[aRow][aCol]);
  FExpandTriangles[aRow][aCol] := NullRegion;
  if (FCurrentCell.x = aCol) and (FCurrentCell.y = aRow) then
  begin
    SetLength(FDragBtns, 0);
    SetLength(FDeleteBtns, 0);
    SetLength(FEditBtns, 0);
  end;
  with TimetableDG do
    if (aCol <> 0) and (aRow = 0) then
      Canvas.TextRect(aRect, aRect.Left + BorderMargin, aRect.Top + BorderMargin,
        FCols[aCol - 1], FTextStyle)
    else if (aCol = 0) and (aRow <> 0) then
    begin
      FTextStyle.Wordbreak := True;
      Canvas.TextRect(aRect, aRect.Left + BorderMargin, aRect.Top + BorderMargin,
        FRows[aRow - 1], FTextStyle);
      FTextStyle.Wordbreak := False;
    end
    else if (aCol <> 0) and (aRow <> 0) and
      (Length(FData[aRow - 1][aCol - 1]) <> 0) then
    begin
      full := False;
      aRect.Right := aRect.Right - ButtonSize;
      aRect.Left := aRect.Left + ButtonSize;
      for i := 0 to High(FData[aRow - 1][aCol - 1]) do
      begin
        if full then
          break;
        w := 0;
        for j := 0 to High(FData[aRow - 1][aCol - 1][i]) do
        begin
          if FRecordHeight * i + Canvas.TextHeight('Hlg') * (j + 1) >
            RowHeights[aRow]
          then
          begin
            full := True;
            break;
          end;
          Canvas.TextRect(aRect, aRect.Left,
            aRect.Top + BorderMargin + FRecordHeight * i +
            Canvas.TextHeight('Hlg') * j, FData[aRow - 1][aCol - 1][i][j],
            FTextStyle);
          w := max(w, Canvas.TextWidth(FData[aRow - 1][aCol - 1][i][j]));
        end;
        Canvas.Pen.Color := clBlack;
        if (i <> 0) and (FRecordHeight <> 0) then
          Canvas.Line(aRect.Left - ButtonSize,
            aRect.Top + BorderMargin + FRecordHeight * i,
            aRect.Right + ButtonSize,
            aRect.Top + BorderMargin + FRecordHeight * i);
        if (FCurrentCell.x = aCol) and (FCurrentCell.y = aRow) then
          begin
            DrawDragBtn(aRect.Left - ButtonSize, aRect.Top + FRecordHeight * i);
            DrawEditBtn(aRect.Left - ButtonSize,
              aRect.Top + FRecordHeight * i + ButtonSize);
            DrawRemoveBtn(aRect.Left - ButtonSize,
              aRect.Top + FRecordHeight * i + ButtonSize * 2);
          end;
      end;
      if (FCurrentCell.x = aCol) and (FCurrentCell.y = aRow) then
      begin
        DrawNewWindowBtn(aRect.Right, aRect.Top);
        DrawAddBtn(aRect.Right, aRect.Top + ButtonSize);
      end;
      if (w > ColWidths[aCol] - ButtonSize * 2 - BorderMargin) or
        (Length(FData[aRow - 1][aCol - 1]) * FRecordHeight > RowHeights[aRow] - BorderMargin)
      then
        DrawTriangle(aRect, aRow, aCol);
    end
end;

procedure TTimetableWindow.TimetableDGEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var
  row, col: Integer;
begin
  TimetableDG.MouseToCell(X, Y, col, row);
  UtilitySQLQuery.SQL.Text := TTimetableQuery.UpdateQueryAsText(
    Metadata.TimetableTable, FRealColList[VerticalCB.ItemIndex],
    FRealColList[HorizontalCB.ItemIndex]);
  UtilitySQLQuery.Prepare;
  UtilitySQLQuery.ParamByName('ID').AsString := FDraggedID;
  UtilitySQLQuery.ParamByName('Vert').AsString := FRowIDs[row - 1];
  UtilitySQLQuery.ParamByName('Horiz').AsString := FColIDs[col - 1];
  UtilitySQLQuery.ExecSQL;
  UDB.DB.SQLTransaction.CommitRetaining;
  OnDataUpdate;
end;

procedure TTimetableWindow.TimetableDGMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, col, row: Integer;
begin
  TimetableDG.MouseToCell(X, Y, col, row);
  for i := 0 to High(FDragBtns) do
    if PtInRect(FDragBtns[i], Point(X, Y)) then
    begin
      FDraggedID := FIDs[row - 1][col - 1][i];
      TimetableDG.BeginDrag(True);
      break;
    end;
end;

procedure TTimetableWindow.TimetableDGMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var col, row: Integer;
begin
  TimetableDG.MouseToCell(X, Y, col, row);
  if (col <> FCurrentCell.x) or (row <> FCurrentCell.y) then
  begin
    TimetableDG.InvalidateCell(FCurrentCell.x, FCurrentCell.y);
    FCurrentCell.x := col;
    FCurrentCell.y := row;
    TimetableDG.InvalidateCell(FCurrentCell.x, FCurrentCell.y);
  end;
end;

procedure TTimetableWindow.TimetableDGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col, row, i: Integer;
  p: TPoint;
begin
  TimetableDG.MouseToCell(X, Y, col, row);
  p := Point(X, Y);
  if PtInRegion(FExpandTriangles[row][col], X, Y) then
    Expand(row, col)
  else if PtInRect(FShowAllButton, p) then
    OpenAsDirectory(row, col)
  else if PtInRect(FAddButton, p) then
    AddRecord(row, col)
  else
    for i := 0 to High(FEditBtns) do
      if PtInRect(FEditBtns[i], p) then
      begin
        EditRecord(row, col, i);
        break;
      end
      else if PtInRect(FDeleteBtns[i], p) then
      begin
        DeleteRecord(row, col, i);
        break;
      end;
end;

procedure TTimetableWindow.TimetableDGStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

procedure TTimetableWindow.VerticalCBChange(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TTimetableWindow.UpdateStatus;
begin
  ApplyBtn.Enabled := True;
end;

procedure TTimetableWindow.DeleteEmpty(const fc: TBooleanArray;
  const fr: TBooleanArray);
var
  k, i, j: Integer;
begin
  k := 0;
  for i := 0 to High(FRows) do
  begin
    if not fr[i] then
      k += 1
    else
    begin
      FRows[i - k] := FRows[i];
      FData[i - k] := FData[i];
    end;
  end;
  SetLength(FRows, Length(FRows) - k);
  SetLength(FData, Length(FData) - k);
  k := 0;
  for i := 0 to High(FCols) do
  begin
    if not fc[i] then
      k += 1
    else
    begin
      FCols[i - k] := FCols[i];
      for j := 0 to High(FRows) do
        FData[j][i - k] := FData[j][i];
    end;
  end;
  SetLength(FCols, Length(FCols) - k);
  if Length(FData) > 0 then
    SetLength(FData, Length(FData), Length(FData[0]) - k);
end;

procedure TTimetableWindow.AssignData(var fc: TBooleanArray; var fr: TBooleanArray);
var
  i, j, k, c: Integer;
  t : ^String;
begin
  SetLength(FData, 0, 0, 0);
  SetLength(FIDs, 0, 0, 0);
  SetLength(FData, Length(FRows), Length(FCols));
  SetLength(FIDs, Length(FRows), Length(FCols));
  SetLength(fr, Length(FRows));
  for i := 0 to High(fr) do
    fr[i] := False;
  SetLength(fc, Length(FCols));
  for i := 0 to High(fc) do
    fc[i] := False;
  SQLQuery.First;
  for i := 0 to High(FRows) do
    for j := 0 to High(FCols) do
      while (not SQLQuery.EOF) and
        (FRowIDs[i] = SQLQuery.FieldByName('VerticalID').AsString) and
        (FColIDs[j] = SQLQuery.FieldByName('HorizontalID').AsString) do
      begin
        fr[i] := True;
        fc[j] := True;
        SetLength(FData[i][j], Length(FData[i][j]) + 1);
        SetLength(FIDs[i][j], Length(FIDs[i][j]) + 1);
        FIDs[i][j][High(FIDs[i][j])] := SQLQuery.Fields[0].AsString;
        for k := 3 to SQLQuery.FieldCount - 1 do
          if DisplayedFieldsCLB.Checked[k - 3] then
          begin
            SetLength(FData[i][j][High(FData[i][j])],
              Length(FData[i][j][High(FData[i][j])]) + 1);
            {Get pointer to the string we will put data into}
            t := @FData[i][j][High(FData[i][j])][High(FData[i][j][High(FData[i][j])])];
            t^ := '';
            if DisplayedNamesCLB.Checked[k - 3] then
              t^ += SQLQuery.Fields[k].DisplayName + ': ';
            t^ += SQLQuery.Fields[k].AsString;
          end;
        SQLQuery.Next;
      end;
  c := 0;
  for k := 3 to SQLQuery.FieldCount - 1 do
    if DisplayedFieldsCLB.Checked[k - 3] then
      c += 1;
  FRecordHeight := TimetableDG.Canvas.TextHeight('Hlg') * c;
end;

procedure TTimetableWindow.DrawNewWindowBtn(ALeft: Integer; ATop: Integer);
begin
  TimetableDG.Canvas.Draw(ALeft, ATop, FOpenInNewWindowImage);
  FShowAllButton := Rect(ALeft, ATop, ALeft + ButtonSize, ATop + ButtonSize);
end;

procedure TTimetableWindow.DrawAddBtn(ALeft: Integer; ATop: Integer);
begin
  TimetableDG.Canvas.Draw(ALeft, ATop, FAddImage);
  FAddButton := Rect(ALeft, ATop, ALeft + ButtonSize, ATop + ButtonSize);
end;

procedure TTimetableWindow.DrawTriangle(aRect: TRect; aRow: Integer;
  aCol: Integer);
var trpts: array[0 .. 2] of TPoint;
begin
  with TimetableDG do
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.Polygon([Point(aRect.Right + RightMargin, aRect.Bottom),
      Point(aRect.Right, aRect.Bottom), Point(aRect.Right + RightMargin,
      aRect.Bottom - RightMargin)]);
    trpts[0] := Point(aRect.Right + RightMargin, aRect.Bottom);
    trpts[1] := Point(aRect.Right, aRect.Bottom);
    trpts[2] := Point(aRect.Right + RightMargin, aRect.Bottom - RightMargin);
    FExpandTriangles[aRow][aCol] := CreatePolygonRgn(trpts, 3, 1);
  end;
end;

procedure TTimetableWindow.DrawDragBtn(ALeft: Integer; ATop: Integer);
begin
  TimetableDG.Canvas.Draw(ALeft, ATop, FDragImage);
  SetLength(FDragBtns, Length(FDragBtns) + 1);
  FDragBtns[High(FDragBtns)] := Rect(ALeft, ATop,
    ALeft + ButtonSize, ATop + ButtonSize);
end;

procedure TTimetableWindow.DrawEditBtn(ALeft: Integer; ATop: Integer);
begin
  TimetableDG.Canvas.Draw(ALeft, ATop, FEditImage);
  SetLength(FEditBtns, Length(FEditBtns) + 1);
  FEditBtns[High(FEditBtns)] := Rect(ALeft, ATop,
    ALeft + ButtonSize, ATop + ButtonSize);
end;

procedure TTimetableWindow.DrawRemoveBtn(ALeft: Integer; ATop: Integer);
begin
  TimetableDG.Canvas.Draw(ALeft, ATop, FDeleteImage);
  SetLength(FDeleteBtns, Length(FDeleteBtns) + 1);
  FDeleteBtns[High(FDeleteBtns)] := Rect(ALeft, ATop,
    ALeft + ButtonSize, ATop + ButtonSize);
end;

procedure TTimetableWindow.Expand(ARow: Integer; ACol: Integer);
var
  w, j, i: Integer;
begin
  w := 0;
  for i := 0 to High(FData[ARow - 1][ACol - 1]) do
    for j := 0 to High(FData[ARow - 1][ACol - 1][i]) do
      w := Max(w, Canvas.TextWidth(FData[ARow - 1][ACol - 1][i][j]) +
        ButtonSize * 2 + BorderMargin);
  TimetableDG.ColWidths[ACol] := Max(w, TimetableDG.ColWidths[ACol]);
  TimetableDG.RowHeights[ARow] := Max(
    Length(FData[ARow - 1][ACol - 1]) *
    FRecordHeight + BorderMargin, TimetableDG.RowHeights[ARow]);
end;

procedure TTimetableWindow.OpenAsDirectory(ARow: Integer; ACol: Integer);
var
  cf: TFilter;
  rf: TFilter;
  df: TDirectoryForm;
begin
  cf := TFilter.Create(nil, Metadata.TimetableTable, [
    (VerticalCB.Items.Objects[VerticalCB.ItemIndex] as TCol).Table.PrimaryKey],
    False);
  cf.SetupHiddenFilter(FRowIDs[ARow - 1]);
  rf := TFilter.Create(nil, Metadata.TimetableTable, [
    (HorizontalCB.Items.Objects[HorizontalCB.ItemIndex] as TCol).Table.PrimaryKey],
    False);
  rf.SetupHiddenFilter(FColIDs[ACol - 1]);
  df := TDirectoryForm.Create(Application, Metadata.TimetableTable);
  df.SetHiddenFilters([rf, cf]);
  df.Show;
end;

procedure TTimetableWindow.AddRecord(ARow: Integer; ACol: Integer);
var t: TCardWindow;
begin
  t := TCardWindow.Create(Application);
  t.Setup(Metadata.TimetableTable, -1, cmTTNew,
    FRealColList[VerticalCB.ItemIndex], FRealColList[HorizontalCB.ItemIndex],
    FRowIDs[ARow - 1], FColIDs[ACol - 1]);
  RegisterCard(t);
  t.Show;
end;

procedure TTimetableWindow.EditRecord(ARow: Integer; ACol: Integer;
  AIndex: Integer);
var t: TCardWindow;
begin
  t := CheckCardExistence(Metadata.TimetableTable,
    StrToInt(FIDs[ARow - 1][ACol - 1][AIndex]));
  if t <> nil then
  begin
    t.BringToFront;
    Exit;
  end;
  t := TCardWindow.Create(Application);
  t.Setup(Metadata.TimetableTable, StrToInt(FIDs[ARow - 1][ACol - 1][AIndex]),
    cmTTEdit, FRealColList[VerticalCB.ItemIndex],
    FRealColList[HorizontalCB.ItemIndex]);
  RegisterCard(t);
  t.Show;
end;

procedure TTimetableWindow.DeleteRecord(ARow: Integer; ACol: Integer;
  AIndex: Integer);
begin
  if MessageDlg('Delete?', 'Do you really want to delete this record?',
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    UtilitySQLQuery.SQL.Text := TTimetableQuery.DeleteQueryAsText(
      Metadata.TimetableTable);
    UtilitySQLQuery.Prepare;
    UtilitySQLQuery.ParamByName('ID').AsString := FIDs[ARow - 1][ACol - 1][AIndex];
    UtilitySQLQuery.ExecSQL;
    UDB.DB.SQLTransaction.CommitRetaining;
  end;
  UtilitySQLQuery.Close;
  OnDataUpdate;
end;

procedure TTimetableWindow.SetupCBs;
var
  i: Integer;
begin
  for i := 0 to High(FColList) do
  begin
    VerticalCB.AddItem(FColList[i].DisplayName, FColList[i]);
    HorizontalCB.AddItem(FColList[i].DisplayName, FColList[i]);
    DisplayedFieldsCLB.AddItem(FColList[i].DisplayName, FColList[i]);
    DisplayedNamesCLB.AddItem(FColList[i].DisplayName, FColList[i]);
  end;
  DisplayedFieldsCLB.CheckAll(cbChecked);
  DisplayedNamesCLB.CheckAll(cbChecked);
  VerticalCB.ItemIndex := 0;
  HorizontalCB.ItemIndex := 1;
end;

procedure TTimetableWindow.RemoveFilter(Sender: TObject);
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

procedure TTimetableWindow.SetupFixed(var ANames: TStringArray;
  ACombobox: TComboBox; var AIDs: TStringArray);
begin
  UtilitySQLQuery.SQL.Text :=
    TTimetableQuery.GetValuesListQuery(
    ACombobox.Items.Objects[ACombobox.ItemIndex] as TCol);
  UtilitySQLQuery.Open;
  SetLength(ANames, 0);
  SetLength(AIDs, 0);
  while not UtilitySQLQuery.EOF do
  begin
    SetLength(ANames, Length(ANames) + 1);
    SetLength(AIDs, Length(AIDs) + 1);
    ANames[High(ANames)] := UtilitySQLQuery.FieldByName('Data').AsString;
    AIDs[High(AIDs)] := UtilitySQLQuery.FieldByName('ID').AsString;
    UtilitySQLQuery.Next;
  end;
  UtilitySQLQuery.Close;
end;

procedure TTimetableWindow.SetupGrid;
const
  FixedRowHeight = 32;
  FixedColWidth = 170;
  StandartColWidth = 320;
  MinRowHeight = 75;
var i: Integer;
begin
  SetupFixed(FRows, VerticalCB, FRowIDs);
  SetupFixed(FCols, HorizontalCB, FColIDs);
  SetupData;
  TimetableDG.RowCount := Length(FRows) + 1;
  TimetableDG.ColCount := Length(FCols) + 1;
  TimetableDG.RowHeights[0] := FixedRowHeight;
  TimetableDG.ColWidths[0] := FixedColWidth;
  for i := 1 to TimetableDG.RowCount - 1 do
    TimetableDG.RowHeights[i] := Max(FRecordHeight + BorderMargin * 2, MinRowHeight);
  for i := 1 to TimetableDG.ColCount - 1 do
    TimetableDG.ColWidths[i] := Max(StandartColWidth, TimetableDG.ColWidths[i]);
  TimetableDG.Invalidate;
end;

procedure TTimetableWindow.SelectData;
var i: Integer;
begin
  FQuery.Free;
  try
    FQuery := TTimetableQuery.Create(Metadata.TimetableTable, FFilters, FColList);
    SQLQuery.Close;
    SQLQuery.SQL.Text := FQuery.SelectQueryAsText(
      VerticalCB.Items.Objects[VerticalCB.ItemIndex] as TCol,
      HorizontalCB.Items.Objects[HorizontalCB.ItemIndex] as TCol);
    SQLQuery.Prepare;
    for i := 0 to SQLQuery.Params.Count - 1 do
      SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
    SQLQuery.Open;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      exit;
    end;
  end;
end;

procedure TTimetableWindow.UpdateData;
//var col, row: Integer;
begin
  //row := TimetableDG.Selection.Top;
  //col := TimetableDG.Selection.Left;
  SQLQuery.Refresh;
  SetupData(False);
end;

procedure TTimetableWindow.SetupData(GetData: Boolean);
var fr, fc: array of Boolean;
begin
  if GetData then
    SelectData;
  AssignData(fc, fr);
  if HideEmptyCB.Checked then
    DeleteEmpty(fc, fr);
  SetLength(FExpandTriangles, Length(FRows) + 1, Length(FCols) + 1);
end;

procedure TTimetableWindow.ApplyBtnClick(Sender: TObject);
begin
  ApplyBtn.Enabled := False;
  SetupGrid;
end;

procedure TTimetableWindow.DisplayedFieldsCLBClickCheck(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TTimetableWindow.DisplayedNamesCLBClickCheck(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TTimetableWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TTimetableWindow.FormCreate(Sender: TObject);
begin
  RegisterDataUpdateListener(@UpdateData);
  FOpenInNewWindowImage := TPortableNetworkGraphic.Create;
  FOpenInNewWindowImage.LoadFromFile('icons/NewWindow.png');
  FAddImage := TPortableNetworkGraphic.Create;
  FAddImage.LoadFromFile('icons/TTAdd.png');
  FDeleteImage := TPortableNetworkGraphic.Create;
  FDeleteImage.LoadFromFile('icons/TTDelete.png');
  FEditImage := TPortableNetworkGraphic.Create;
  FEditImage.LoadFromFile('icons/TTEdit.png');
  FDragImage := TPortableNetworkGraphic.Create;
  FDragImage.LoadFromFile('icons/TTDrag.png');
  FCurrentCell.x := 0;
  FCurrentCell.y := 0;
  with FTextStyle do
  begin
    Alignment := taLeftJustify;
    Layout := tlTop;
    WordBreak := False;
    SingleLine := False;
    Clipping := True;
    ShowPrefix := False;
    Opaque := False;
    EndEllipsis := True;
  end;
  FColList := TTimetableQuery.GetFullColList(Metadata.TimetableTable);
  FRealColList := TTimetableQuery.GetRealColList(Metadata.TimetableTable);
  SetupCBs;
  SetupGrid;
end;

procedure TTimetableWindow.HideEmptyCBChange(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TTimetableWindow.HorizontalCBChange(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TTimetableWindow.TimetableDGDblClick(Sender: TObject);
const StandartColWidth = 320;
begin
  with TimetableDG do
  begin;
    ColWidths[Selection.Left] := StandartColWidth;
    RowHeights[Selection.Top] := FRecordHeight;
  end;
end;

procedure TTimetableWindow.TimetableDGDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TTimetableWindow.AddFilterBtnClick(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, Metadata.TimetableTable,
    TDirectoryQuery.GetFullColList(Metadata.TimetableTable));
  FFilters[High(FFilters)].OnFilterUpdate := @UpdateStatus;
  FFilters[High(FFilters)].OnFilterRemove := @RemoveFilter;
end;

end.

