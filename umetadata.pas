unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTable = class;
  TCol = class;

  { TCol }

  TCol = class
    public
      Table: TTable;
      SQLName: String;
      DisplayName: String;
      Reference: TCol;
      Width: Integer;
      PrimaryKey: Boolean;
      Real: Boolean;
      constructor Create(ASQLName, ADisplayName: String; AWidth: Integer;
        APrimaryKey: Boolean = False; AReal: Boolean = True; AReference: TCol = nil);
  end;

  TColArray = array of TCol;

  { TTable }

  TTable = class
    public
      Cols: TColArray;
      ForeignKeys: TColArray;
      PrimaryKey: TCol;
      SortKey: TCol;
      SQLName: String;
      DisplayName: String;
      GeneratorName: String;
      constructor Create(ASQLName, ADisplayName: String);
      procedure AddCol(ACol: TCol);
  end;

  TTableArray = array of TTable;

  { TMetadata }

  TMetadata = class
    public
      Tables: array of TTable;
      TimetableTable: TTable;
      procedure RegisterTable(
        ASQLName, ADisplayName: String; ACols: array of TCol;
        AGenerator: String; IsTimeTable: Boolean = False; SortBy: String = '');
      function GetReference(ATableName: String): TCol;
      function GetColByName(ATableName: String; AColName: String): TCol;
  end;

var
  Metadata: TMetadata;

implementation

{ TMetadata }

procedure TMetadata.RegisterTable(ASQLName, ADisplayName: String;
  ACols: array of TCol; AGenerator: String; IsTimeTable: Boolean; SortBy: String
  );
var
  i: Integer;
  nt: TTable;
begin
  nt := TTable.Create(ASQLName, ADisplayName);
  for i := 0 to High(ACols) do
  begin
    if ACols[i].SQLName = SortBy then
    begin
      nt.SortKey := ACols[i];
      nt.SortKey.Table := nt;
      Continue;
    end;
    nt.AddCol(ACols[i]);
    ACols[i].Table := nt;
  end;
  nt.GeneratorName := AGenerator;
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := nt;
  if IsTimeTable then
    TimetableTable := nt
end;

function TMetadata.GetReference(ATableName: String): TCol;
var
  t: TTable;
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].SQLName = ATableName then
    begin
      t := Tables[i];
      break;
    end;
  Result := t.PrimaryKey;
end;

function TMetadata.GetColByName(ATableName: String; AColName: String): TCol;
var
  t: TTable;
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].SQLName = ATableName then
    begin
      t := Tables[i];
      break;
    end;
  for i := 0 to High(t.Cols) do
    if t.Cols[i].SQLName = AColName then
    begin
      Result := t.Cols[i];
      Exit;
    end;
  for i := 0 to High(t.ForeignKeys) do
    if t.ForeignKeys[i].SQLName = AColName then
    begin
      Result := t.ForeignKeys[i];
      Exit;
    end;
end;

{ TTable }

constructor TTable.Create(ASQLName, ADisplayName: String);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
end;

procedure TTable.AddCol(ACol: TCol);
begin
  if ACol.PrimaryKey then
    PrimaryKey := ACol
  else
  if ACol.Reference <> nil then
  begin
    SetLength(ForeignKeys, Length(ForeignKeys) + 1);
    ForeignKeys[High(ForeignKeys)] := ACol;
  end
  else
  begin
    SetLength(Cols, Length(Cols) + 1);
    Cols[High(Cols)] := ACol;
  end;
end;

{ TCol }

constructor TCol.Create(ASQLName, ADisplayName: String; AWidth: Integer;
  APrimaryKey: Boolean; AReal: Boolean; AReference: TCol);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
  Reference := AReference;
  Width := AWidth;
  PrimaryKey := APrimaryKey;
  Real := AReal;
end;

initialization
  Metadata := TMetadata.Create;
  //Initializing metadata
  Metadata.RegisterTable('Groups', 'Groups', [
    TCol.Create('id', 'Group ID', 100, True),
    TCol.Create('name', 'Group', 120),
    TCol.Create('number', 'Number of students', 100)], 'GroupsIdGenerator');
  Metadata.RegisterTable('Lessons', 'Lessons', [
    TCol.Create('id', 'Lesson ID', 100, True),
    TCol.Create('name', 'Lesson', 450)], 'LessonsIdGenerator');
  Metadata.RegisterTable('Teachers', 'Teachers', [
    TCol.Create('id', 'Teacher ID', 100, True),
    TCol.Create('last_name', 'Last name', 200),
    TCol.Create('first_name', 'First name', 200),
    TCol.Create('middle_name', 'Middle name', 200)], 'TeachersIdGenerator');
  Metadata.RegisterTable('Classrooms', 'Classrooms', [
    TCol.Create('id', 'Classroom ID', 120, True),
    TCol.Create('name', 'Classroom', 110),
    TCol.Create('capacity', 'Capacity', 100)], 'ClassroomsIdGenerator');
  Metadata.RegisterTable('Lessons_Times', 'Lesson Times', [
    TCol.Create('id', 'Lesson time ID', 130, True),
    TCol.Create('begin_', 'Starts at', 100),
    TCol.Create('end_', 'Ends at', 100),
    TCol.Create('number', 'Lesson number', 10)],
    'LessonsTimesIdGenerator', False, 'number');
  Metadata.RegisterTable('Weekdays', 'Weekdays', [
    TCol.Create('id', 'Weekday ID', 100, True),
    TCol.Create('name', 'Weekday', 120),
    TCol.Create('number', 'Weekday number', 10)],
    'WeekdaysIdGenerator', False, 'number');
  Metadata.RegisterTable('Lessons_Types', 'Lesson types', [
    TCol.Create('id', 'Lesson type ID', 130, True),
    TCol.Create('name', 'Type', 200)], 'LessonsTypesIdGenerator');
  Metadata.RegisterTable('Timetable', 'Timetable', [
    TCol.Create('id', 'ID', 40, True),
    TCol.Create('lesson_id', 'Lesson',
      100, False, True, Metadata.GetReference('Lessons')),
    TCol.Create('lesson_type_id', 'Lesson type',
      110, False, True, Metadata.GetReference('Lessons_Types')),
    TCol.Create('teacher_id', 'Teacher',
      100, False, True, Metadata.GetReference('Teachers')),
    TCol.Create('group_id', 'Group',
      100, False, True, Metadata.GetReference('Groups')),
    TCol.Create('classroom_id', 'Classroom',
      120, False, True, Metadata.GetReference('Classrooms')),
    TCol.Create('weekday_id', 'Weekday',
      100, False, True, Metadata.GetReference('Weekdays')),
    TCol.Create('lesson_time_id', 'Lesson time',
      100, False, True, Metadata.GetReference('Lessons_Times'))],
    'TimetableIdGenerator', True);
end.

