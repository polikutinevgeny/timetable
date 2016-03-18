unit UShowingWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids;

type

  { TShowingForm }

  TShowingForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ShowingForm: TShowingForm;

implementation

{$R *.lfm}

end.

