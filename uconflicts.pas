unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UQuery, UConflictsDM, Dialogs, ComCtrls, UNotification;

type

  TBaseConflictType = class;

  { TConflict }

  TConflict = class
    public
      ConflictedIDs: TStringArray;
      Parent: TBaseConflictType;
      constructor Create(AConflictedIDs: TStringArray; AParent: TBaseConflictType);
  end;

  { TBaseConflictType }

  TBaseConflictType = class abstract
    private
      FQuery: TBaseConflictQuery;
      FIDs: TStringArray;
    public
      EqualFields: TColArray;
      Name: String;
      Conflicts: array of TConflict;
      procedure RefreshConflicts; virtual; abstract;
      procedure Display(ATreeView: TTreeView; AMarkedConflicts: array of TConflict);
  end;

  { TValueConflictType }

  TValueConflictType = class(TBaseConflictType)
    public
      NotEqualFields: TColArray;
      constructor Create(AEqualFields: array of TCol;
        ANotEqualFields: array of TCol; AName: String);
      procedure RefreshConflicts; override;
  end;

  { TCapacityConflictType }

  TCapacityConflictType = class(TBaseConflictType)
    public
      CapacityField: TCol;
      CapacityFieldRef: TCol;
      CapacityConsumingField: TCol;
      CapacityConsumingFieldRef: TCol;
      constructor Create(AEqualFields: array of TCol; ACapacityField: TCol;
        ACapacityFieldRef: TCol; ACapacityConsumingField: TCol;
        ACapacityConsumingFieldRef: TCol; AName: String);
      procedure RefreshConflicts; override;
  end;

  TConflictArray = array of TConflict;

  { TConflictTypesContainer }

  TConflictTypesContainer = class
    public
      ConflictTypes: array of TBaseConflictType;
      procedure RegisterConflict(AConflict: TBaseConflictType);
      procedure RefreshConflicts;
      function GetConflicts(AID: String): TConflictArray;
      constructor Create;
  end;

var
  ConflictTypesContainer: TConflictTypesContainer;

implementation

{ TBaseConflictType }

procedure TBaseConflictType.Display(ATreeView: TTreeView;
  AMarkedConflicts: array of TConflict);
var
  j, k: Integer;
  t1, t2, t3: TTreeNode;
  s: String;
begin
  SetLength(FIDs, 0);
  t1 := ATreeView.Items.Add(nil, Name);
  for j := 0 to High(Conflicts) do
  begin
    ConflictsDM.SQLQuery.Close;
    ConflictsDM.SQLQuery.SQL.Text := FQuery.DisplayQueryAsText(Conflicts[j].ConflictedIDs);
    ConflictsDM.SQLQuery.Open;
    s := '';
    for k := 0 to High(EqualFields) do
    begin
      s += ConflictsDM.SQLQuery.FieldByName(EqualFields[k].DisplayName).AsString + ', ';
      ConflictsDM.SQLQuery.FieldByName(EqualFields[k].DisplayName).Tag := 1;
    end;
    t2 := ATreeView.Items.AddChild(t1, s);
    t2.Data := Conflicts[j];
    for k := 0 to High(AMarkedConflicts) do
      if Conflicts[j] = AMarkedConflicts[k] then
      begin
        t2.ImageIndex := 0;
        t2.Parent.ImageIndex := 0;
        Break;
      end;
    while not ConflictsDM.SQLQuery.EOF do
    begin
      s := '';
      for k := 1 to ConflictsDM.SQLQuery.Fields.Count - 1 do
        if ConflictsDM.SQLQuery.Fields[k].Tag = 0 then
          s += ConflictsDM.SQLQuery.Fields[k].AsString + ', ';
      t3 := ATreeView.Items.AddChild(t2, s);
      SetLength(FIDs, Length(FIDs) + 1);
      FIDs[High(FIDs)] := ConflictsDM.SQLQuery.Fields[0].AsString;
      t3.Data := @FIDs[High(FIDs)];
      ConflictsDM.SQLQuery.Next;
    end;
  end;
end;

{ TConflictTypesContainer }

procedure TConflictTypesContainer.RegisterConflict(AConflict: TBaseConflictType
  );
begin
  SetLength(ConflictTypes, Length(ConflictTypes) + 1);
  ConflictTypes[High(ConflictTypes)] := AConflict;
end;

procedure TConflictTypesContainer.RefreshConflicts;
var i: Integer;
begin
  for i := 0 to High(ConflictTypes) do
    ConflictTypes[i].RefreshConflicts;
end;

function TConflictTypesContainer.GetConflicts(AID: String): TConflictArray;
var
  i, j, k: Integer;
  found: Boolean;
begin
  SetLength(Result, 0);
  for i := 0 to High(ConflictTypes) do
    for j := 0 to High(ConflictTypes[i].Conflicts) do
    begin
      found := False;
      for k := 0 to High(ConflictTypes[i].Conflicts[j].ConflictedIDs) do
        if ConflictTypes[i].Conflicts[j].ConflictedIDs[k] = AID then
        begin
          found := True;
          break;
        end;
      if found then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := ConflictTypes[i].Conflicts[j];
      end;
    end;
end;

constructor TConflictTypesContainer.Create;
begin
  SetLength(ConflictTypes, 0);
  RegisterDataUpdateListener(@RefreshConflicts);
end;

{ TCapacityConflictType }

constructor TCapacityConflictType.Create(AEqualFields: array of TCol;
  ACapacityField: TCol; ACapacityFieldRef: TCol; ACapacityConsumingField: TCol;
  ACapacityConsumingFieldRef: TCol; AName: String);
var i: Integer;
begin
  SetLength(EqualFields, Length(AEqualFields));
  for i := 0 to High(AEqualFields) do
    EqualFields[i] := AEqualFields[i];
  Name := AName;
  CapacityField := ACapacityField;
  CapacityConsumingField := ACapacityConsumingField;
  CapacityFieldRef := ACapacityFieldRef;
  CapacityConsumingFieldRef := ACapacityConsumingFieldRef;
  FQuery := TCapacityConflictQuery.Create(Metadata.TimetableTable, nil,
    TTimetableQuery.GetFullColList(Metadata.TimetableTable));
end;

procedure TCapacityConflictType.RefreshConflicts;
var
  i, total, curcap: Integer;
  cvs: TStringArray;
  ccids: TStringArray;
  eqok: Boolean;
begin
  SetLength(Conflicts, 0);
  with ConflictsDM.SQLQuery do
  begin;
    Close;
    SQL.Text := TCapacityConflictQuery.GetSelectQuery(
      Metadata.TimetableTable, EqualFields, CapacityField, CapacityFieldRef,
      CapacityConsumingField, CapacityConsumingFieldRef);
    Open;
    First;
    SetLength(cvs, Length(EqualFields));
    SetLength(ccids, 1);
    for i := 0 to High(EqualFields) do
      cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
    ccids[0] := FieldByName('ID').AsString;
    total := FieldByName(CapacityConsumingField.DisplayName).AsInteger;
    curcap := FieldByName(CapacityField.DisplayName).AsInteger;
    Next;
    eqok := True;
    while True do
    begin
      for i := 0 to High(EqualFields) do
        if not (cvs[i] = FieldByName(EqualFields[i].DisplayName).AsString) then
        begin
          eqok := False;
          Break;
        end;
      if eqok then
      begin
        SetLength(ccids, Length(ccids) + 1);
        ccids[High(ccids)] := FieldByName('ID').AsString;
        total += FieldByName(CapacityConsumingField.DisplayName).AsInteger;
        if EOF then
        begin
          if total > curcap then
          begin
            SetLength(Conflicts, Length(Conflicts) + 1);
            Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
          end;
          break;
        end;
      end
      else
      begin
        if total > curcap then
        begin
          SetLength(Conflicts, Length(Conflicts) + 1);
          Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
        end;
        curcap := FieldByName(CapacityField.DisplayName).AsInteger;
        total := FieldByName(CapacityConsumingField.DisplayName).AsInteger;
        SetLength(ccids, 1);
        ccids[0] := FieldByName('ID').AsString;
        for i := 0 to High(EqualFields) do
          cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
        eqok := True;
      end;
      Next;
    end;
  end;
end;

{ TConflict }

constructor TConflict.Create(AConflictedIDs: TStringArray;
  AParent: TBaseConflictType);
var i: Integer;
begin
  SetLength(ConflictedIDs, Length(AConflictedIDs));
  for i := 0 to High(AConflictedIDs) do
    ConflictedIDs := AConflictedIDs;
  Parent := AParent;
end;

{ TValueConflictType }

constructor TValueConflictType.Create(AEqualFields: array of TCol;
  ANotEqualFields: array of TCol; AName: String);
var i: Integer;
begin
  SetLength(EqualFields, Length(AEqualFields));
  SetLength(NotEqualFields, Length(ANotEqualFields));
  for i := 0 to High(AEqualFields) do
    EqualFields[i] := AEqualFields[i];
  for i := 0 to High(ANotEqualFields) do
    NotEqualFields[i] := ANotEqualFields[i];
  Name := AName;
  FQuery := TCapacityConflictQuery.Create(Metadata.TimetableTable, nil,
    TTimetableQuery.GetFullColList(Metadata.TimetableTable));
end;

procedure TValueConflictType.RefreshConflicts;
var
  i: Integer;
  cvs: TStringArray;
  ccids: TStringArray;
  eqok, neqok: Boolean;
begin
  SetLength(Conflicts, 0);
  with ConflictsDM.SQLQuery do
  begin;
    Close;
    SQL.Text := TValueConflictQuery.GetSelectQuery(
      Metadata.TimetableTable, EqualFields, NotEqualFields);
    Open;
    First;
    SetLength(cvs, Length(EqualFields) + Length(NotEqualFields));
    SetLength(ccids, 1);
    for i := 0 to High(EqualFields) do
      cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
    for i := 0 to High(NotEqualFields) do
      cvs[i + Length(EqualFields)] :=
        FieldByName(NotEqualFields[i].DisplayName).AsString;
    ccids[0] := FieldByName('ID').AsString;
    Next;
    eqok := True;
    neqok := False;
    while True do
    begin
      for i := 0 to High(EqualFields) do
        if not (cvs[i] = FieldByName(EqualFields[i].DisplayName).AsString) then
        begin
          eqok := False;
          Break;
        end;
      if eqok then
      begin
        for i := 0 to High(NotEqualFields) do
          if not (cvs[i + Length(EqualFields)] =
            FieldByName(NotEqualFields[i].DisplayName).AsString)
          then
          begin
            neqok := True;
            Break;
          end;
        SetLength(ccids, Length(ccids) + 1);
        ccids[High(ccids)] := FieldByName('ID').AsString;
        if EOF then
        begin
          if (neqok) and (Length(ccids) > 1) then
          begin
            SetLength(Conflicts, Length(Conflicts) + 1);
            Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
          end;
          Break;
        end;
      end
      else
      begin
        if (neqok) and (Length(ccids) > 1) then
        begin
          SetLength(Conflicts, Length(Conflicts) + 1);
          Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
        end;
        SetLength(ccids, 1);
        ccids[0] := FieldByName('ID').AsString;
        for i := 0 to High(EqualFields) do
          cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
        for i := 0 to High(NotEqualFields) do
          cvs[i + Length(EqualFields)] :=
            FieldByName(NotEqualFields[i].DisplayName).AsString;
        eqok := True;
        neqok := False;
      end;
      Next;
    end;
  end;
end;

initialization
  ConflictTypesContainer := TConflictTypesContainer.Create;
  ConflictTypesContainer.RegisterConflict(TValueConflictType.Create([
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'weekday_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_time_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'teacher_id')], [
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'classroom_id')],
    'Teacher is in two or more classrooms at the same time'));
  ConflictTypesContainer.RegisterConflict(TValueConflictType.Create([
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'weekday_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_time_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'group_id')], [
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'classroom_id')],
    'Group is in two or more classrooms at the same time'));
  ConflictTypesContainer.RegisterConflict(TValueConflictType.Create([
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'weekday_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_time_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'group_id')], [
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_id')],
    'Group has two or more different lessons at the same time'));
  ConflictTypesContainer.RegisterConflict(TValueConflictType.Create([
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'weekday_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_time_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'classroom_id')], [
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'teacher_id')],
    'Classroom has two or more teachers in it at the same time'));
  ConflictTypesContainer.RegisterConflict(TCapacityConflictType.Create([
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'weekday_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'lesson_time_id'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'classroom_id')],
    Metadata.GetColByName('Classrooms', 'capacity'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'classroom_id'),
    Metadata.GetColByName('Groups', 'number'),
    Metadata.GetColByName(Metadata.TimetableTable.SQLName, 'group_id'),
    'Classroom doesn''''t have enough space'));
end.

