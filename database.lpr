program database;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainWindow, UDirectoryWindow, UMetadata, UDB, UFilters,
  UQuery, UCardWindow, UNotification, UTimetableWindow, UConflicts, UConflictsDM,
  UConflictTreeWindow, UExcelConstants, UExcelExport;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TDB, DB);
  Application.CreateForm(TConflictsDM, ConflictsDM);
  Application.CreateForm(TConflictTreeWindow, ConflictTreeWindow);
  Application.CreateForm(TTimetableWindow, TimetableWindow);
  Application.Run;
end.

