unit UTimetableWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  LCLIntf, LCLType, ExtCtrls, StdCtrls, Buttons, PairSplitter, UQuery,
  UMetadata, db, sqldb, UFilters;

type

  { TTimetableWindow }

  TTimetableWindow = class(TForm)
    ApplyBtn: TSpeedButton;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    SQLQuery: TSQLQuery;
    VerticalCB: TComboBox;
    HorizontalCB: TComboBox;
    OptionsPanel: TPanel;
    TimetableDG: TDrawGrid;
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
var i, j, k: Integer;
begin
  FQuery.Free;
  FQuery := TTimetableQuery.Create(Metadata.TimetableTable, nil);
  SQLQuery.Close;
  SQLQuery.SQL.Text := FQuery.TimetableQueryAsText(
    VerticalCB.Items[VerticalCB.ItemIndex],
    HorizontalCB.Items[HorizontalCB.ItemIndex]);
  SQLQuery.Open;
  SetLength(FData, 0, 0, 0);
  SetLength(FData, Length(FRows), Length(FCols));
  for i := 0 to High(FRows) do
    for j := 0 to High(FCols) do
    begin
      while (not SQLQuery.EOF) and
        (FRows[i] = SQLQuery.FieldByName(VerticalCB.Items[
          VerticalCB.ItemIndex]).AsString) and
        (FCols[j] = SQLQuery.FieldByName(HorizontalCB.Items[
          HorizontalCB.ItemIndex]).AsString) do
      begin
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
  TimetableDG.RowCount := Length(FRows) + 1;
  SetupFixed(FCols, HorizontalCB);
  TimetableDG.ColCount := Length(FCols) + 1;
  SetupData;
  TimetableDG.RowHeights[0] := 32;
  TimetableDG.ColWidths[0] := 150;
end;

procedure TTimetableWindow.ApplyBtnClick(Sender: TObject);
begin
  SetupFixed(FRows, VerticalCB);
  SetupFixed(FCols, HorizontalCB);
  SetupData;
  TimetableDG.RowCount := Length(FRows) + 1;
  TimetableDG.ColCount := Length(FCols) + 1;
end;

end.

