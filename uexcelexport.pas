unit UExcelExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, comobj, LCLProc, UExcelConstants, UFilters, math, Dialogs;

type TData = array of array of array of array of String;

procedure ExportTo(AFilename: WideString; ACols: array of String;
  ARows: array of String; AData: TData; AFilters: array of TFilter; AFileType: Integer;
  AStartDate, AEndDate: TDateTime);

implementation

procedure ExportTo(AFilename: WideString; ACols: array of String;
  ARows: array of String; AData: TData; AFilters: array of TFilter;
  AFileType: Integer; AStartDate, AEndDate: TDateTime);
const
  ColumnWidth = 30;
  FixedColumnWidth = 20;
  FixedCellsColorIndex = 15;
  FixedRowHeight = 15;
var
  xlapp, xlworksheet, temp: OleVariant;
  i, j, k, l, currow, height: Integer;
  rec: WideString;
begin
  try
    xlapp := CreateOleObject('Excel.Application');
    xlapp.DisplayAlerts := False;
    xlapp.Visible := False;
    xlapp.ScreenUpdating := False;
    xlapp.Workbooks.Add;
    xlworksheet := xlapp.WorkBooks[1].WorkSheets[1];
    xlworksheet.Name := 'Timetable';
    xlworksheet.Cells[1, 1].Interior.ColorIndex := FixedCellsColorIndex;
    xlworksheet.Rows.Rows[1].RowHeight := FixedRowHeight;
    xlworksheet.Columns.Columns[1].ColumnWidth := FixedColumnWidth;
    xlworksheet.Rows.Rows[1].WrapText := True;
    xlworksheet.Columns.Columns[1].WrapText := True;
    for i := 2 to Length(ACols) + 1 do
    begin
      xlworksheet.Columns.Columns[i].ColumnWidth := ColumnWidth;
      xlworksheet.Cells[1, i].Value := WideString(ACols[i - 2]);
      xlworksheet.Cells[1, i].BorderAround(xlContinuous, xlThick, vbBlack);
      xlworksheet.Cells[1, i].Interior.ColorIndex := FixedCellsColorIndex;
      xlworksheet.Cells[1, i].Font.Bold := True;
    end;
    xlapp.ActiveWindow.SplitRow := 1;
    xlapp.ActiveWindow.SplitColumn := 1;
    xlapp.ActiveWindow.FreezePanes := True;
    currow := 2;
    for i := 0 to High(AData) do
    begin
      height := 1;
      for j := 0 to High(AData[i]) do
      begin
        height := Max(height, Length(AData[i][j]));
        for k := 0 to High(AData[i][j]) do
        begin
          rec := '';
          for l := 0 to High(AData[i][j][k]) do
          begin
            rec += WideString(AData[i][j][k][l]);
            if l < High(AData[i][j][k]) then
              rec += #10;
          end;
          xlworksheet.Cells[currow + k, j + 2].Value := rec;
          xlworksheet.Cells[currow + k, j + 2].BorderAround(
          xlContinuous, xlThin, vbBlack);
        end;
      end;
      temp := xlworksheet.Range(
        xlworksheet.Cells[currow, 2],
        xlworksheet.Cells[currow + height - 1, j + 2]);
      temp.BorderAround(xlContinuous, xlThin, vbBlack);
      temp.VerticalAlignment := xlTop;
      temp := xlworksheet.Range(
        xlworksheet.Cells[currow, 1],
        xlworksheet.Cells[currow + height - 1, 1]);
      temp.Merge;
      temp.VerticalAlignment := xlTop;
      temp.BorderAround(xlContinuous, xlThick, vbBlack);
      xlworksheet.Cells[currow, 1].Value := WideString(ARows[i]);
      xlworksheet.Cells[currow, 1].Font.Bold := True;
      xlworksheet.Cells[currow, 1].Interior.ColorIndex := FixedCellsColorIndex;
      currow += height;
    end;
    currow += 1;
    if Length(AFilters) > 0 then
    begin
      xlworksheet.Cells[currow, 1].Value := 'Applied filters:';
      for i := 0 to High(AFilters) do
      begin
        xlworksheet.Cells[currow + i, 2].Value := WideString(
          IntToStr(i + 1) + '). ' + AFilters[i].Column.DisplayName + ' '  +
          AFilters[i].Action + ' ' + AFilters[i].Value);
        xlworksheet.Cells[currow + i, 2].BorderAround(
          xlContinuous, xlThin, vbBlack);
      end;
      temp := xlworksheet.Range(
        xlworksheet.Cells[currow, 1],
        xlworksheet.Cells[currow + High(AFilters), 1]);
      temp.Merge;
      temp.BorderAround(xlContinuous, xlThick, vbBlack);
      temp.VerticalAlignment := xlTop;
      xlworksheet.Range(
        xlworksheet.Cells[currow, 2],
        xlworksheet.Cells[currow + High(AFilters), 2]).BorderAround(
          xlContinuous, xlThick, vbBlack);
    end;
    xlworksheet.Cells[currow - 1, 4].Value := WideString(Format('From %s to %s', [
      DateToStr(AStartDate), DateToStr(AEndDate)]));
    xlapp.Workbooks[1].SaveAs(AFilename, AFileType);
    xlapp.Workbooks[1].Close;
  except
    on E: Exception do
      MessageDlg('Error', E.Message, mtError, [mbOK], 0);
  end;
  xlapp.Quit;
  xlapp := Unassigned;
end;

end.

