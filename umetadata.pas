unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTable = class;
  TCol = class;

  TDataType = (dtInteger, dtString); //Is it really needed?

  { TCol }

  TCol = class
    public
      Table: TTable;
      SQLName: String;
      DisplayName: String;
      DataType: TDataType;
      Reference: TCol;
      Width: Integer;
      PrimaryKey: Boolean;
      constructor Create(ASQLName, ADisplayName: String; ADataType: TDataType;
        AWidth: Integer; APrimaryKey: Boolean = False; AReference: TCol = nil);
  end;

  TColArray = array of TCol;

  { TTable }

  TTable = class
    public
      Cols: TColArray;
      ForeignKeys: TColArray;
      PrimaryKey: TCol;
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
        AGenerator: String; IsTimeTable: Boolean = False);
      function GetReference(ATableName: String): TCol;
  end;

var
  Metadata: TMetadata;

implementation

{ TMetadata }

procedure TMetadata.RegisterTable(ASQLName, ADisplayName: String;
  ACols: array of TCol; AGenerator: String; IsTimeTable: Boolean);
var
  i: Integer;
  nt: TTable;
begin
  nt := TTable.Create(ASQLName, ADisplayName);
  for i := 0 to High(ACols) do
  begin
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

constructor TCol.Create(ASQLName, ADisplayName: String; ADataType: TDataType;
  AWidth: Integer; APrimaryKey: Boolean = False; AReference: TCol = nil);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
  DataType := ADataType;
  Reference := AReference;
  Width := AWidth;
  PrimaryKey := APrimaryKey;
end;

initialization
  Metadata := TMetadata.Create;
  //Initializing metadata
  Metadata.RegisterTable('Groups', 'Groups', [
    TCol.Create('id', 'Group ID', dtInteger, 100, True),
    TCol.Create('name', 'Group', dtString, 120)],
    'GroupsIdGenerator');
  Metadata.RegisterTable('Lessons', 'Lessons', [
    TCol.Create('id', 'Lesson ID', dtInteger, 100, True),
    TCol.Create('name', 'Lesson', dtString, 450)],
    'LessonsIdGenerator');
  Metadata.RegisterTable('Teachers', 'Teachers', [
    TCol.Create('id', 'Teacher ID', dtInteger, 100, True),
    TCol.Create('last_name', 'Last name', dtString, 200),
    TCol.Create('first_name', 'First name', dtString, 200),
    TCol.Create('middle_name', 'Middle name', dtString, 200)],
    'TeachersIdGenerator');
  Metadata.RegisterTable('Classrooms', 'Classrooms', [
    TCol.Create('id', 'Classroom ID', dtInteger, 120, True),
    TCol.Create('name', 'Classroom', dtString, 110)],
    'ClassroomsIdGenerator');
  Metadata.RegisterTable('Lessons_Times', 'Lesson Times', [
    TCol.Create('id', 'Lesson time ID', dtInteger, 130, True),
    TCol.Create('begin_', 'Starts at', dtString, 100),
    TCol.Create('end_', 'Ends at', dtString, 100)],
    'LessonsTimesIdGenerator');
  Metadata.RegisterTable('Weekdays', 'Weekdays', [
    TCol.Create('id', 'Weekday ID', dtInteger, 100, True),
    TCol.Create('name', 'Weekday', dtString, 120)],
    'WeekdaysIdGenerator');
  Metadata.RegisterTable('Lessons_Types', 'Lesson types', [
    TCol.Create('id', 'Lesson type ID', dtInteger, 130, True),
    TCol.Create('name', 'Type', dtString, 200)],
    'LessonsTypesIdGenerator');
  Metadata.RegisterTable('Timetable', 'Timetable', [
    TCol.Create('id', 'ID', dtInteger, 40, True),
    TCol.Create('lesson_id', 'Lesson', dtInteger,
      100, False, Metadata.GetReference('Lessons')),
    TCol.Create('lesson_type_id', 'Lesson type', dtInteger,
      110, False, Metadata.GetReference('Lessons_Types')),
    TCol.Create('teacher_id', 'Teacher', dtInteger,
      100, False, Metadata.GetReference('Teachers')),
    TCol.Create('group_id', 'Group', dtInteger,
      100, False, Metadata.GetReference('Groups')),
    TCol.Create('classroom_id', 'Classroom', dtInteger,
      120, False, Metadata.GetReference('Classrooms')),
    TCol.Create('weekday_id', 'Weekday', dtInteger,
      100, False, Metadata.GetReference('Weekdays')),
    TCol.Create('lesson_time_id', 'Lesson time', dtInteger,
      100, False, Metadata.GetReference('Lessons_Times'))],
    'TimetableIdGenerator', True);
end.

