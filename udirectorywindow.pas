unit UDirectoryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, UDB, UMetadata;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FilterSB: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure FormShow(Sender: TObject);
  private
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

  SQLQuery.Active := True;
end;

end.

