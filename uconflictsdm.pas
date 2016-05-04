unit UConflictsDM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil;

type

  { TConflictsDM }

  TConflictsDM = class(TDataModule)
    SQLQuery: TSQLQuery;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConflictsDM: TConflictsDM;

implementation

{$R *.lfm}

end.

