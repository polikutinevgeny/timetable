unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, UTimetableWindow,
  Dialogs, Menus, StdCtrls, UDirectoryWindow, UMetadata;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    AboutMI: TMenuItem;
    TimetableMI: TMenuItem;
    StartupLabel: TLabel;
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    ExitMI: TMenuItem;
    DirectoriesMI: TMenuItem;
    procedure AboutMIClick(Sender: TObject);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DirectoryMIClick(Sender: TObject);
    procedure TimetableMIClick(Sender: TObject);
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
begin
  for i := 0 to High(Metadata.Tables) do
  begin
    m := TMenuItem.Create(MainMenu);
    m.Caption := Metadata.Tables[i].DisplayName;
    m.Tag := i;
    m.OnClick := @DirectoryMIClick;
    DirectoriesMI.Add(m);
  end;
end;

procedure TMainWindow.DirectoryMIClick(Sender: TObject);
var f: TDirectoryForm;
begin
  f := TDirectoryForm.Create(Application);
  f.CurrentTable := Metadata.Tables[(Sender as TMenuItem).Tag];
  f.Show;
end;

procedure TMainWindow.TimetableMIClick(Sender: TObject);
begin
  TimetableWindow.Show;
end;

procedure TMainWindow.ExitMIClick(Sender: TObject);
begin
  Close();
end;

procedure TMainWindow.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Timetable by Polikutin Evgeny'#13#10'B8103a, 2015');
end;

end.

