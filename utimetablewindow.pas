unit UTimetableWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  LCLIntf, LCLType, ExtCtrls, StdCtrls, Buttons, PairSplitter, CheckLst, UQuery,
  UMetadata, sqldb, UFilters, math, UDirectoryWindow;

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
    procedure TimetableDGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TimetableDGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VerticalCBChange(Sender: TObject);
  private
    FQuery: TTimetableQuery;
    FColList: TColArray;
    FCols: TStringArray;
    FRows: TStringArray;
    FData: array of array of array of array of String;
    FShowAllButtons: array of array of HRGN;
    FExpandTriangles: array of array of HRGN;
    FFilters: array of TFilter;
    FRecordHeight: Integer;
    FOpenInNewWindowImage: TPortableNetworkGraphic;
    FTextStyle: TTextStyle;
    procedure AssignData(fc: TBooleanArray; fr: TBooleanArray);
    procedure DeleteEmpty(const fc: TBooleanArray; const fr: TBooleanArray);
    procedure DrawNewWindowBtn(aRect: TRect; aRow: Integer; aCol: Integer);
    procedure DrawTriangle(aRect: TRect; aRow: Integer; aCol: Integer);
    procedure UpdateStatus;
    procedure RemoveFilter(Sender: TObject);
    procedure SetupData;
    procedure SetupFixed(var ANames: TStringArray; ACombobox: TComboBox);
    procedure SetupGrid;
    procedure SelectData;
  public
  end;

var
  TimetableWindow: TTimetableWindow;

implementation

const
  BorderMargin = 2;
  RightMargin = 25;

{$R *.lfm}

{ TTimetableWindow }

procedure TTimetableWindow.TimetableDGDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var i, j, w: Integer;
begin
  Canvas.FillRect(aRect);
  aRect.Right := aRect.Right - RightMargin;
  DeleteObject(FExpandTriangles[aRow][aCol]);
  FExpandTriangles[aRow][aCol] := NullRegion;
  FShowAllButtons[aRow][aCol] := NullRegion;
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
      for i := 0 to High(FData[aRow - 1][aCol - 1]) do
      begin
        w := 0;
        for j := 0 to High(FData[aRow - 1][aCol - 1][i]) do
        begin
          Canvas.TextRect(aRect, aRect.Left + BorderMargin,
            aRect.Top + BorderMargin + FRecordHeight * i +
            Canvas.TextHeight('Hlg') * j, FData[aRow - 1][aCol - 1][i][j],
            FTextStyle);
          w := max(w, Canvas.TextWidth(FData[aRow - 1][aCol - 1][i][j]));
        end;
        Canvas.Pen.Color := clBlack;
        if i <> 0 then
          Canvas.Line(
            aRect.Left ,aRect.Top + BorderMargin + FRecordHeight * i,
            aRect.Right + RightMargin,
            aRect.Top + BorderMargin + FRecordHeight * i);
      end;
      DrawNewWindowBtn(aRect, aRow, aCol);
      if (w > ColWidths[aCol] - RightMargin) or
        (Length(FData[aRow - 1][aCol - 1]) * FRecordHeight > RowHeights[aRow])
      then
        DrawTriangle(aRect, aRow, aCol);
    end
end;

procedure TTimetableWindow.TimetableDGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col, row, i, j, w: Integer;
  df: TDirectoryForm;
  rf, cf: TFilter;
begin
  TimetableDG.MouseToCell(X, Y, col, row);
  if PtInRegion(FExpandTriangles[row][col], X, Y) then
  begin
    w := 0;
    for i := 0 to High(FData[row - 1][col - 1]) do
      for j := 0 to High(FData[row - 1][col - 1][i]) do
        w := max(w, Canvas.TextWidth(FData[row - 1][col - 1][i][j]) + 25);
    TimetableDG.ColWidths[col] := Max(w, TimetableDG.ColWidths[col]);
    TimetableDG.RowHeights[row] := Max(
      Length(FData[row - 1][col - 1]) *
      FRecordHeight, TimetableDG.RowHeights[row]);
  end
  else if PtInRegion(FShowAllButtons[row][col], X, Y) then
  begin
    cf := TFilter.Create(nil, Metadata.TimetableTable, FColList, False);
    cf.SetupHiddenFilter(VerticalCB.ItemIndex, FRows[row - 1]);
    rf := TFilter.Create(nil, Metadata.TimetableTable, FColList, False);
    rf.SetupHiddenFilter(HorizontalCB.ItemIndex, FCols[col - 1]);
    df := TDirectoryForm.Create(Application, Metadata.TimetableTable);
    df.SetHiddenFilters([rf, cf]);
    df.Show;
  end;
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

procedure TTimetableWindow.AssignData(fc: TBooleanArray; fr: TBooleanArray);
var i, j, k: Integer;
begin
  FRecordHeight := TimetableDG.Canvas.TextHeight('Hlg') *
    (SQLQuery.FieldCount - 1);
  SetLength(FData, 0, 0, 0);
  SetLength(FData, Length(FRows), Length(FCols));
  SetLength(fr, Length(FRows));
  for i := 0 to High(fr) do
    fr[i] := False;
  SetLength(fc, Length(FCols));
  for i := 0 to High(fc) do
    fc[i] := False;
  for i := 0 to High(FRows) do
    for j := 0 to High(FCols) do
      while (not SQLQuery.EOF) and
        (FRows[i] = SQLQuery.FieldByName(VerticalCB.Items[
        VerticalCB.ItemIndex]).AsString) and
        (FCols[j] = SQLQuery.FieldByName(HorizontalCB.Items[
        HorizontalCB.ItemIndex]).AsString) do
      begin
        fr[i] := True;
        fc[j] := True;
        SetLength(FData[i][j], Length(FData[i][j]) + 1);
        SetLength(FData[i][j][High(FData[i][j])], SQLQuery.FieldCount - 1);
        for k := 1 to SQLQuery.FieldCount - 1 do
          if DisplayedFieldsCLB.Checked[k - 1] then
          begin
            FData[i][j][High(FData[i][j])][k - 1] := '';
            if DisplayedNamesCLB.Checked[k - 1] then
              FData[i][j][High(FData[i][j])][k - 1] +=
                SQLQuery.Fields[k].DisplayName + ': ';
            FData[i][j][High(FData[i][j])][k - 1] += SQLQuery.Fields[k].AsString;
          end;
        SQLQuery.Next;
      end;
end;

procedure TTimetableWindow.DrawNewWindowBtn(aRect: TRect; aRow: Integer;
  aCol: Integer);
begin
  TimetableDG.Canvas.Draw(aRect.Right, aRect.Top, FOpenInNewWindowImage);
  FShowAllButtons[aRow][aCol] := CreateRectRgn(aRect.Right, aRect.Top,
    aRect.Right + RightMargin, aRect.Top + RightMargin);
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
    trpts[2] := Point(aRect.Right + RightMargin, aRect.Bottom -
      RightMargin);
    FExpandTriangles[aRow][aCol] := CreatePolygonRgn(trpts, 3, 1);
  end;
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
  ACombobox: TComboBox);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text :=
    TTimetableQuery.GetValuesListQuery(Metadata.TimetableTable,
    ACombobox.Items[ACombobox.ItemIndex]);
  SQLQuery.Open;
  SetLength(ANames, 0);
  while not SQLQuery.EOF do
  begin
    SetLength(ANames, Length(ANames) + 1);
    ANames[High(ANames)] := SQLQuery.FieldByName('Data').AsString;
    SQLQuery.Next;
  end;
end;

procedure TTimetableWindow.SetupGrid;
const
  FixedRowHeight = 32;
  FixedColWidth = 170;
  StandartColWidth = 320;
var i: Integer;
begin
  SetupFixed(FRows, VerticalCB);
  SetupFixed(FCols, HorizontalCB);
  SetupData;
  TimetableDG.RowCount := Length(FRows) + 1;
  TimetableDG.ColCount := Length(FCols) + 1;
  TimetableDG.RowHeights[0] := FixedRowHeight;
  TimetableDG.ColWidths[0] := FixedColWidth;
  for i := 1 to TimetableDG.RowCount - 1 do
    TimetableDG.RowHeights[i] := FRecordHeight;
  for i := 1 to TimetableDG.ColCount - 1 do
    TimetableDG.ColWidths[i] := StandartColWidth;
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
      VerticalCB.Items[VerticalCB.ItemIndex],
      HorizontalCB.Items[HorizontalCB.ItemIndex]);
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

procedure TTimetableWindow.SetupData;
var fr, fc: array of Boolean;
begin
  SelectData;
  AssignData(fc, fr);
  if HideEmptyCB.Checked then
    DeleteEmpty(fc, fr);
  SetLength(FShowAllButtons, Length(FRows) + 1, Length(FCols) + 1);
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
var i: Integer;
begin
  FOpenInNewWindowImage := TPortableNetworkGraphic.Create;
  FOpenInNewWindowImage.LoadFromFile('icons/NewWindow.png');
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

