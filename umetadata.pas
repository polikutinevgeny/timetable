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
      Relationship: TCol;
      Width: Integer;
      constructor Create(ASQLName, ADisplayName: String; ADataType: TDataType;
        ARelationship: TCol; AWidth: Integer);
  end;

  TColArray = array of TCol;

  { TTable }

  TTable = class
    public
      Cols: TColArray;
      ForeignKeys: TColArray;
      SQLName: String;
      DisplayName: String;
      constructor Create(ASQLName, ADisplayName: String);
      procedure AddCol(ACol: TCol);
  end;

  { TMetadata }

  TMetadata = class
    public
      Tables: array of TTable;
      procedure RegisterTable(
        ASQLName, ADisplayName: String; ACols: array of TCol);
      function GetRelationship(ATableName, AColName: String): TCol;
  end;

var
  Metadata: TMetadata;

implementation

{ TMetadata }

procedure TMetadata.RegisterTable(ASQLName, ADisplayName: String;
  ACols: array of TCol);
var
  i: Integer;
begin
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := TTable.Create(ASQLName, ADisplayName);
  for i := 0 to High(ACols) do
  begin
    Tables[High(Tables)].AddCol(ACols[i]);
    ACols[i].Table := Tables[High(Tables)];
  end;
end;

function TMetadata.GetRelationship(ATableName, AColName: String): TCol;
var
  t: TTable;
  c: TCol;
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
      c := t.Cols[i];
      break;
    end;
  Result := c;
end;

{ TTable }

constructor TTable.Create(ASQLName, ADisplayName: String);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
end;

procedure TTable.AddCol(ACol: TCol);
begin
  if ACol.Relationship <> nil then
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
  ARelationship: TCol; AWidth: Integer);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
  DataType := ADataType;
  Relationship := ARelationship;
  Width := AWidth;
end;

initialization
  Metadata := TMetadata.Create;
  //Initializing metadata
  Metadata.RegisterTable('Groups', 'Groups', [
    TCol.Create('id', 'Group ID', dtInteger, nil, 100),
    TCol.Create('name', 'Group', dtString, nil, 120)]);
  Metadata.RegisterTable('Lessons', 'Lessons', [
    TCol.Create('id', 'Lesson ID', dtInteger, nil, 100),
    TCol.Create('name', 'Lesson', dtString, nil, 400)]);
  Metadata.RegisterTable('Teachers', 'Teachers', [
    TCol.Create('id', 'Teacher ID', dtInteger, nil, 100),
    TCol.Create('last_name', 'Last name', dtString, nil, 200),
    TCol.Create('first_name', 'First name', dtString, nil, 200),
    TCol.Create('middle_name', 'Middle name', dtString, nil, 200)]);
  Metadata.RegisterTable('Classrooms', 'Classrooms', [
    TCol.Create('id', 'Classroom ID', dtInteger, nil, 120),
    TCol.Create('name', 'Classroom', dtString, nil, 110)]);
  Metadata.RegisterTable('Lessons_Times', 'Lesson Times', [
    TCol.Create('id', 'Lesson time ID', dtInteger, nil, 130),
    TCol.Create('begin_', 'Starts at', dtString, nil, 100),
    TCol.Create('end_', 'Ends at', dtString, nil, 100)]);
  Metadata.RegisterTable('Weekdays', 'Weekdays', [
    TCol.Create('id', 'Weekday ID', dtInteger, nil, 100),
    TCol.Create('name', 'Weekday', dtString, nil, 120)]);
  Metadata.RegisterTable('Lessons_Types', 'Lesson types', [
    TCol.Create('id', 'Lesson type ID', dtInteger, nil, 130),
    TCol.Create('name', 'Type', dtString, nil, 200)]);
  Metadata.RegisterTable('Timetable', 'Timetable', [
    TCol.Create('id', 'ID', dtInteger, nil, 40),
    TCol.Create('lesson_id', 'Lesson ID', dtInteger,
      Metadata.GetRelationship('Lessons', 'id'), 100),
    TCol.Create('lesson_type_id', 'Lesson type ID', dtInteger,
      Metadata.GetRelationship('Lessons_Types', 'id'), 110),
    TCol.Create('teacher_id', 'Teacher ID', dtInteger,
      Metadata.GetRelationship('Teachers', 'id'), 100),
    TCol.Create('group_id', 'Group ID', dtInteger,
      Metadata.GetRelationship('Groups', 'id'), 100),
    TCol.Create('classroom_id', 'Classroom ID', dtInteger,
      Metadata.GetRelationship('Classrooms', 'id'), 120),
    TCol.Create('weekday_id', 'Weekday ID', dtInteger,
      Metadata.GetRelationship('Weekdays', 'id'), 100),
    TCol.Create('lesson_time_id', 'Lesson time ID', dtInteger,
      Metadata.GetRelationship('Lessons_Times', 'id'), 100)]);
end.

