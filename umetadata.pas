unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TColDataType = (cdtNull, cdtInteger, cdtDate, cdtTime, cdtString);

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
      Hidden: Boolean;
      DataType: TColDataType;
      constructor Create(ADataType: TColDataType; AHidden: Boolean; ASQLName,
        ADisplayName: String; AWidth: Integer; APrimaryKey: Boolean = False;
        AReal: Boolean = True; AReference: TCol = nil);
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
      PeriodStartCol: TCol;
      PeriodEndCol: TCol;
      WeekdayCol: TCol;
      RepeatPeriod: TCol;
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

constructor TCol.Create(ADataType: TColDataType; AHidden: Boolean; ASQLName,
  ADisplayName: String; AWidth: Integer; APrimaryKey: Boolean; AReal: Boolean;
  AReference: TCol);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
  Reference := AReference;
  Width := AWidth;
  PrimaryKey := APrimaryKey;
  Real := AReal;
  Hidden := AHidden;
  DataType := ADataType;
end;

initialization
  Metadata := TMetadata.Create;
  //Initializing metadata
  Metadata.RegisterTable('Groups', 'Groups', [
    TCol.Create(cdtInteger, True, 'id', 'Group ID', 100, True),
    TCol.Create(cdtString, False, 'name', 'Group', 120),
    TCol.Create(cdtInteger, True, 'number', 'Number of students', 100)], 'GroupsIdGenerator');
  Metadata.RegisterTable('Lessons', 'Lessons', [
    TCol.Create(cdtInteger, True, 'id', 'Lesson ID', 100, True),
    TCol.Create(cdtString, False, 'name', 'Lesson', 450)], 'LessonsIdGenerator');
  Metadata.RegisterTable('Teachers', 'Teachers', [
    TCol.Create(cdtInteger, True, 'id', 'Teacher ID', 100, True),
    TCol.Create(cdtString, False, 'last_name', 'Last name', 200),
    TCol.Create(cdtString, False, 'first_name', 'First name', 200),
    TCol.Create(cdtString, False, 'middle_name', 'Middle name', 200)], 'TeachersIdGenerator');
  Metadata.RegisterTable('Classrooms', 'Classrooms', [
    TCol.Create(cdtInteger, True, 'id', 'Classroom ID', 120, True),
    TCol.Create(cdtString, False, 'name', 'Classroom', 110),
    TCol.Create(cdtInteger, True, 'capacity', 'Capacity', 100)], 'ClassroomsIdGenerator');
  Metadata.RegisterTable('Lessons_Times', 'Lesson Times', [
    TCol.Create(cdtInteger, True, 'id', 'Lesson time ID', 130, True),
    TCol.Create(cdtTime, False, 'begin_', 'Starts at', 100),
    TCol.Create(cdtTime, False, 'end_', 'Ends at', 100),
    TCol.Create(cdtInteger, True, 'number', 'Lesson number', 100)],
    'LessonsTimesIdGenerator', False, 'number');
  Metadata.RegisterTable('Weekdays', 'Weekdays', [
    TCol.Create(cdtInteger, True, 'id', 'Weekday ID', 100, True),
    TCol.Create(cdtString, False, 'name', 'Weekday', 120),
    TCol.Create(cdtInteger, True, 'number', 'Weekday number', 100)],
    'WeekdaysIdGenerator', False, 'number');
  Metadata.RegisterTable('Lessons_Types', 'Lesson types', [
    TCol.Create(cdtInteger, True, 'id', 'Lesson type ID', 130, True),
    TCol.Create(cdtString, False, 'name', 'Type', 200)], 'LessonsTypesIdGenerator');
  Metadata.RegisterTable('Timetable', 'Timetable', [
    TCol.Create(cdtInteger, True, 'id', 'ID', 40, True),
    TCol.Create(cdtString, False, 'lesson_id', 'Lesson',
      100, False, True, Metadata.GetReference('Lessons')),
    TCol.Create(cdtString, False, 'lesson_type_id', 'Lesson type',
      110, False, True, Metadata.GetReference('Lessons_Types')),
    TCol.Create(cdtString, False, 'teacher_id', 'Teacher',
      100, False, True, Metadata.GetReference('Teachers')),
    TCol.Create(cdtString, False, 'group_id', 'Group',
      100, False, True, Metadata.GetReference('Groups')),
    TCol.Create(cdtString, False, 'classroom_id', 'Classroom',
      120, False, True, Metadata.GetReference('Classrooms')),
    TCol.Create(cdtString, False, 'weekday_id', 'Weekday',
      100, False, True, Metadata.GetReference('Weekdays')),
    TCol.Create(cdtString, False, 'lesson_time_id', 'Lesson time',
      100, False, True, Metadata.GetReference('Lessons_Times')),
    TCol.Create(cdtDate, True, 'actuality_period_start', 'Actuality period start', 200),
    TCol.Create(cdtDate, True, 'actuality_period_end', 'Actuality period end', 200),
    TCol.Create(cdtInteger, True, 'repeat_period', 'Repeat period', 100)],
    'TimetableIdGenerator', True);
  Metadata.PeriodStartCol := Metadata.GetColByName('Timetable', 'actuality_period_start');
  Metadata.PeriodEndCol := Metadata.GetColByName('Timetable', 'actuality_period_end');
  Metadata.WeekdayCol := Metadata.GetColByName('Weekdays', 'number');
  Metadata.RepeatPeriod := Metadata.GetColByName('Timetable', 'repeat_period');
end.

