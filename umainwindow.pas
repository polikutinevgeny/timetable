unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, Menus, StdCtrls, UShowingWindow;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    IBConnection: TIBConnection;
    AboutMI: TMenuItem;
    StartupLabel: TLabel;
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    ExitMI: TMenuItem;
    DirectoriesMI: TMenuItem;
    SQLTransaction: TSQLTransaction;
    procedure AboutMIClick(Sender: TObject);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DirectoryMIClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

var
  TableNames: array of String;
  ReadableNames: array of String;

procedure RegisterTable(ATableName: String; AReadableName: String);
begin
  SetLength(TableNames, Length(TableNames) + 1);
  SetLength(ReadableNames, Length(ReadableNames) + 1);
  TableNames[High(TableNames)] := ATableName;
  ReadableNames[High(ReadableNames)] := AReadableName;
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  t: TMenuItem;
  i: Integer;
begin
  for i := 0 to High(TableNames) do
  begin
    t := TMenuItem.Create(DirectoriesMI);
    t.Caption := ReadableNames[i];
    t.OnClick := @DirectoryMIClick;
    t.Tag := i;
    DirectoriesMI.Add(t);
  end;
end;

procedure TMainWindow.DirectoryMIClick(Sender: TObject);
var
  f: TShowingForm;
begin
  f := TShowingForm.Create(nil);
  f.SQLQuery.Active := False;
  f.SQLQuery.SQL.Text := 'SELECT * FROM ' +
    TableNames[(Sender as TMenuItem).Tag];
  f.SQLQuery.Active := True;
  f.Caption := ReadableNames[(Sender as TMenuItem).Tag];
  f.Show;
end;

procedure TMainWindow.ExitMIClick(Sender: TObject);
begin
  Close();
end;

procedure TMainWindow.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Timetable by Polikutin Evgeny'#13#10'B8103a, 2015');
end;

initialization
  RegisterTable('Groups', 'Groups');
  RegisterTable('Teachers', 'Teachers');
  RegisterTable('Lessons', 'Lessons');
  RegisterTable('Classrooms', 'Classrooms');
  RegisterTable('Weekdays', 'Weekdays');
  RegisterTable('Lessons_Times', 'Lesson times');
  RegisterTable('Lessons_Types', 'Lesson types');
  RegisterTable('Timetable', 'Timetable');
  RegisterTable('FullTimetable', 'Full timetable');
end.

