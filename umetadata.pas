unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTable = class;
  TCol = class;

  TDataType = (dtInteger, dtString);

  { TRelationship }

  TRelationship = class
  public
    Table: TTable;
    Col: TCol;
    constructor Create(ATable: TTable; ACol: TCol);
  end;

  { TCol }

  TCol = class
    public
      SQLName: String;
      DisplayName: String;
      DataType: TDataType;
      Relationship: TRelationship;
      constructor Create(ASQLName, ADisplayName: String; ADataType: TDataType;
        ARelationship: TRelationship);
  end;

  { TTable }
  TStringArray = array of string;

  TTable = class
    private
      function GetColNames: TStringArray;
    public
      Cols: array of TCol;
      SQLName: String;
      DisplayName: String;
      constructor Create(ASQLName, ADisplayName: String);
      procedure AddCol(ACol: TCol);
      property ColNames: TStringArray read GetColNames;
  end;

  { TMetadata }

  TMetadata = class
    public
      Tables: array of TTable;
      procedure RegisterTable(
        ASQLName, ADisplayName: String; ACols: array of TCol);
      function GetRelationship(ATableName, AColName: String): TRelationship;
  end;

var
  Metadata: TMetadata;

implementation

{ TRelationship }

constructor TRelationship.Create(ATable: TTable; ACol: TCol);
begin
  Table := ATable;
  Col := ACol;
end;

{ TMetadata }

procedure TMetadata.RegisterTable(ASQLName, ADisplayName: String;
  ACols: array of TCol);
var
  i: Integer;
begin
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := TTable.Create(ASQLName, ADisplayName);
  SetLength(Tables[High(Tables)].Cols, Length(ACols));
  for i := 0 to High(ACols) do
  begin
    Tables[High(Tables)].Cols[i] := ACols[i];
  end;
end;

function TMetadata.GetRelationship(ATableName, AColName: String): TRelationship;
var
  t: TTable;
  c: TCol;
  i: Integer;
begin
  for i := 0 to High(Tables) do
  begin
    if Tables[i].SQLName = ATableName then
    begin
      t := Tables[i];
      break;
    end;
  end;
  for i := 0 to High(t.Cols) do
  begin
    if t.Cols[i].SQLName = AColName then
    begin
      c := t.Cols[i];
      break;
    end;
  end;
  Result := TRelationship.Create(t, c);
end;

{ TTable }

function TTable.GetColNames: TStringArray;
var i: Integer;
begin
  SetLength(Result, Length(Cols));
  for i := 0 to High(Cols) do
  begin
    Result[i] := Cols[i].DisplayName;
  end;
end;

constructor TTable.Create(ASQLName, ADisplayName: String);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
end;

procedure TTable.AddCol(ACol: TCol);
begin
  SetLength(Cols, Length(Cols) + 1);
  Cols[High(Cols)] := ACol;
end;

{ TCol }

constructor TCol.Create(ASQLName, ADisplayName: String; ADataType: TDataType;
  ARelationship: TRelationship);
begin
  SQLName := ASQLName;
  DisplayName := ADisplayName;
  DataType := ADataType;
  Relationship := ARelationship;
end;

initialization
  Metadata := TMetadata.Create;
  //Initializing metadata
  Metadata.RegisterTable('Groups', 'Groups', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('name', 'Group', dtString, nil)]);
  Metadata.RegisterTable('Lessons', 'Lessons', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('name', 'Lesson', dtString, nil)]);
  Metadata.RegisterTable('Teachers', 'Teachers', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('last_name', 'Last name', dtString, nil),
    TCol.Create('first_name', 'First name', dtString, nil),
    TCol.Create('middle_name', 'Middle name', dtString, nil)]);
  Metadata.RegisterTable('Classrooms', 'Classrooms', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('name', 'Classroom', dtString, nil)]);
  Metadata.RegisterTable('Lessons_Times', 'Lesson Times', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('begin_', 'Starts at', dtInteger, nil),
    TCol.Create('end_', 'Ends at', dtInteger, nil)]);
  Metadata.RegisterTable('Weekdays', 'Weekdays', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('name', 'Weekday', dtString, nil)]);
  Metadata.RegisterTable('Lessons_Types', 'Lesson types', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('name', 'Type', dtString, nil)]);
  Metadata.RegisterTable('Timetable', 'Timetable', [
    TCol.Create('id', 'ID', dtInteger, nil),
    TCol.Create('lesson_id', 'Lesson ID', dtInteger,
      Metadata.GetRelationship('Lessons', 'id')),
    TCol.Create('lesson_type_id', 'Lesson type ID', dtInteger,
      Metadata.GetRelationship('Lessons_Types', 'id')),
    TCol.Create('teacher_id', 'Teacher ID', dtInteger,
      Metadata.GetRelationship('Teachers', 'id')),
    TCol.Create('group_id', 'Group ID', dtInteger,
      Metadata.GetRelationship('Groups', 'id')),
    TCol.Create('classroom_id', 'Classroom ID', dtInteger,
      Metadata.GetRelationship('Classrooms', 'id')),
    TCol.Create('weekday_id', 'Weekday ID', dtInteger,
      Metadata.GetRelationship('Weekdays', 'id')),
    TCol.Create('lesson_time_id', 'Lesson time ID', dtInteger,
      Metadata.GetRelationship('Lessons_Times', 'id'))]);
end.

