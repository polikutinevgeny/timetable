unit UDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil;

type

  { TDB }

  TDB = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  end;

var
  DB: TDB;

implementation

{$R *.lfm}

end.

