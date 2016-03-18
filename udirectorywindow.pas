unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, StdCtrls, UDB, UMetadata, USQLQueryConstructor, UFilters,
  math;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    AddFilterBtn: TButton;
    RemoveFilterBtn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FilterSB: TScrollBox;
    FilterControlPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveFilterBtnClick(Sender: TObject);
  private
    FFilters: array of TFilter;
    { private declarations }
  public
    CurrentTable: Integer;
    { public declarations }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.FormShow(Sender: TObject);
begin
  SQLQuery.Active := False;
  SQLQuery.SQL.Text := SQLQueryConstructor.CreateQuery(CurrentTable, []);
  SQLQuery.Active := True;
end;

procedure TDirectoryForm.RemoveFilterBtnClick(Sender: TObject);
begin
  FreeAndNil(FFilters[High(FFilters)]);
  SetLength(FFilters, max(Length(FFilters) - 1, 0));
  FilterSB.Tag := max(FilterSB.Tag - 1, 0);
end;

procedure TDirectoryForm.AddFilterBtnClick(Sender: TObject);
begin
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(
    FilterSB, Metadata.Tables[CurrentTable]);
end;

end.

