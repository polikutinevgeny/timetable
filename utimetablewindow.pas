unit UTimetableWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  LCLIntf, LCLType, ExtCtrls, StdCtrls, Buttons, PairSplitter, CheckLst, UQuery,
  UMetadata, db, sqldb, UFilters;

type

  { TTimetableWindow }

  TTimetableWindow = class(TForm)
    ApplyBtn: TSpeedButton;
    CheckListBox2: TCheckListBox;
    CheckListBox3: TCheckListBox;
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
    procedure FormShow(Sender: TObject);
    procedure TimetableDGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    FQuery: TTimetableQuery;
    FColList: TStringArray;
    FCols: TStringArray;
    FRows: TStringArray;
    FData: array of array of array of String;
    FFilters: array of TFilter;
    procedure UpdateStatus;
    procedure RemoveFilter(Sender: TObject);
    procedure SetupData;
    procedure SetupFixed(var ANames: TStringArray; ACombobox: TComboBox);
    { private declarations }
  public
    { public declarations }
  end;

var
  TimetableWindow: TTimetableWindow;

implementation

{$R *.lfm}

{ TTimetableWindow }

procedure TTimetableWindow.TimetableDGDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  s: string;
  style: TTextStyle;
  i: Integer;
begin
  with style do
  begin
    Alignment := taLeftJustify;
    Layout := tlTop;
    WordBreak := False;
    SingleLine := False;
    Clipping := False;
    ShowPrefix := False;
    Opaque := False;
    EndEllipsis := False;
  end;
  with TimetableDG do
  begin
    if (aCol <> 0) and (aRow = 0) then
      Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, FCols[aCol - 1], style)
    else if (aCol = 0) and (aRow <> 0) then
    begin
      style.Wordbreak := True;
      Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, FRows[aRow - 1], style);
    end
    else if (aCol <> 0) and (aRow <> 0) and
      (Length(FData[aRow - 1][aCol - 1]) <> 0) then
      for i := 0 to High(FData[aRow - 1][aCol - 1]) do
        Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2 + 300 * i,
          FData[aRow - 1][aCol - 1][i], style);
  end;
end;

procedure TTimetableWindow.UpdateStatus;
begin
  ApplyBtn.Enabled := True;
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

procedure TTimetableWindow.SetupData;
var
  i, j, k: Integer;
  fr, fc: array of Boolean;
begin
  FQuery.Free;
  FQuery := TTimetableQuery.Create(Metadata.TimetableTable, FFilters);
  SQLQuery.Close;
  SQLQuery.SQL.Text := FQuery.TimetableQueryAsText(
    VerticalCB.Items[VerticalCB.ItemIndex],
    HorizontalCB.Items[HorizontalCB.ItemIndex]);
  SQLQuery.Prepare;
  for i := 0 to SQLQuery.Params.Count - 1 do
    SQLQuery.Params.Items[i].AsString := FFilters[i].Value;
  ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.Open;
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
    begin
      while (not SQLQuery.EOF) and
        (FRows[i] = SQLQuery.FieldByName(VerticalCB.Items[
          VerticalCB.ItemIndex]).AsString) and
        (FCols[j] = SQLQuery.FieldByName(HorizontalCB.Items[
          HorizontalCB.ItemIndex]).AsString) do
      begin
        fr[i] := True;
        fc[j] := True;
        SetLength(FData[i][j], Length(FData[i][j]) + 1);
        FData[i][j][High(FData[i][j])] := '';
        for k := 0 to SQLQuery.FieldCount - 1 do
        begin
          FData[i][j][High(FData[i][j])] +=
            SQLQuery.Fields[k].DisplayName + ': ' + SQLQuery.Fields[k].AsString +
            #10#13;
        end;
        SQLQuery.Next;
      end;
    end;
  if HideEmptyCB.Checked then
  begin;
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
    SetLength(FData, Length(FData), Length(FData[0]) - k);
  end;
end;

procedure TTimetableWindow.FormShow(Sender: TObject);
var i: Integer;
begin
  FColList := TTimetableQuery.GetFullColList(Metadata.TimetableTable);
  for i := 0 to High(FColList) do
  begin
    VerticalCB.AddItem(FColList[i], nil);
    HorizontalCB.AddItem(FColList[i], nil);
  end;
  VerticalCB.ItemIndex := 0;
  HorizontalCB.ItemIndex := 1;
  SetupFixed(FRows, VerticalCB);
  SetupFixed(FCols, HorizontalCB);
  SetupData;
  TimetableDG.RowCount := Length(FRows) + 1;
  TimetableDG.ColCount := Length(FCols) + 1;
  TimetableDG.RowHeights[0] := 32;
  TimetableDG.ColWidths[0] := 160;
end;

procedure TTimetableWindow.ApplyBtnClick(Sender: TObject);
begin
  SetupFixed(FRows, VerticalCB);
  SetupFixed(FCols, HorizontalCB);
  SetupData;
  TimetableDG.RowCount := 1;
  TimetableDG.ColCount := 1;
  TimetableDG.RowCount := Length(FRows) + 1;
  TimetableDG.ColCount := Length(FCols) + 1;
end;

procedure TTimetableWindow.AddFilterBtnClick(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, Metadata.TimetableTable, TDirectoryQuery.GetFullColList(Metadata.TimetableTable));
  FFilters[High(FFilters)].OnFilterUpdate := @UpdateStatus;
  FFilters[High(FFilters)].OnFilterRemove := @RemoveFilter;
end;

end.

