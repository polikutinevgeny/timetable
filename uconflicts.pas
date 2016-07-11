unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, UQuery, UConflictsDM, Dialogs, ComCtrls, UNotification,
  UBinaryHeap;

type

  TBaseConflictType = class;

  { TConflict }

  TConflict = class
    public
      ConflictedIDs: array of Integer;
      Parent: TBaseConflictType;
      constructor Create(AConflictedIDs: array of Integer; AParent: TBaseConflictType);
  end;

  { TBaseConflictType }

  TBaseConflictType = class abstract
    private
      FQuery: TBaseConflictQuery;
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
      function GetConflicts(AID: Integer): TConflictArray;
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
  t1 := ATreeView.Items.Add(nil, Name);
  t1.SelectedIndex := t1.ImageIndex;
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
    while not ConflictsDM.SQLQuery.EOF do
    begin
      s := '';
      for k := 1 to ConflictsDM.SQLQuery.Fields.Count - 1 do
        if ConflictsDM.SQLQuery.Fields[k].Tag = 0 then
          s += ConflictsDM.SQLQuery.Fields[k].AsString + ', ';
      t3 := ATreeView.Items.AddChild(t2, s);
      t3.Data := Pointer(ConflictsDM.SQLQuery.Fields[0].AsInteger);
      ConflictsDM.SQLQuery.Next;
    end;
    for k := 0 to High(AMarkedConflicts) do
      if Conflicts[j] = AMarkedConflicts[k] then
      begin
        t2.ImageIndex := 0;
        t2.Parent.ImageIndex := 0;
        t2.Parent.Expand(False);
        t2.Expand(False);
        Break;
      end;
    t2.Parent.SelectedIndex := t2.Parent.ImageIndex;
    t2.SelectedIndex := t2.ImageIndex;
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

function TConflictTypesContainer.GetConflicts(AID: Integer): TConflictArray;
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
    TTimetableQuery.GetFullColList(Metadata.TimetableTable, True));
end;

procedure TCapacityConflictType.RefreshConflicts;
var
  total, curcap: Integer;
  heap: TIDDateCapacityBinaryHeap;

  procedure NewConflict;
  var
    j: Integer;
    ccids: array of Integer;
  begin
    if total > curcap then
    begin
      SetLength(ccids, Length(heap.Values));
      for j := 0 to High(heap.Values) do
        ccids[j] := heap.Values[j].ID;
      SetLength(Conflicts, Length(Conflicts) + 1);
      Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
    end;
  end;

var
  cvs: TStringArray;
  eqok: Boolean;
  i: Integer;
begin
  heap := TIDDateCapacityBinaryHeap.Create;
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
    for i := 0 to High(EqualFields) do
      cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
    heap.Push(IDDateCapacity(FieldByName('ID').AsInteger,
      FieldByName(Metadata.PeriodEndCol.DisplayName).AsDateTime,
      FieldByName(CapacityConsumingField.DisplayName).AsInteger));
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
        if (FieldByName(Metadata.PeriodStartCol.DisplayName).AsDateTime > heap.Values[0].EndDate) then
        begin
          NewConflict;
          while (Length(heap.Values) > 0) and
            (FieldByName(Metadata.PeriodStartCol.DisplayName).AsDateTime < heap.Values[0].EndDate)
          do
          begin
            total -= heap.Values[0].Capacity;
            heap.Pop;
          end;
        end;
        total += FieldByName(CapacityConsumingField.DisplayName).AsInteger;
        if EOF then
        begin
          NewConflict;
          break;
        end;
      end
      else
      begin
        NewConflict;
        while (Length(heap.Values) > 0) do
          heap.Pop;
        curcap := FieldByName(CapacityField.DisplayName).AsInteger;
        total := FieldByName(CapacityConsumingField.DisplayName).AsInteger;
        for i := 0 to High(EqualFields) do
          cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
        eqok := True;
      end;
      heap.Push(IDDateCapacity(FieldByName('ID').AsInteger,
        FieldByName(Metadata.PeriodEndCol.DisplayName).AsDateTime,
        FieldByName(CapacityConsumingField.DisplayName).AsInteger));
      Next;
    end;
  end;
end;

{ TConflict }

constructor TConflict.Create(AConflictedIDs: array of Integer;
  AParent: TBaseConflictType);
var i: Integer;
begin
  SetLength(ConflictedIDs, Length(AConflictedIDs));
  for i := 0 to High(AConflictedIDs) do
    ConflictedIDs[i] := AConflictedIDs[i];
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
    TTimetableQuery.GetFullColList(Metadata.TimetableTable, True));
end;

procedure TValueConflictType.RefreshConflicts;
var
  i: Integer;
  cvs, temp: TStringArray;
  ccids: array of Integer;
  eqok, neqok: Boolean;
  heap: TIDDateBinaryHeap;

  procedure NewConflict;
  var j : Integer;
  begin
    if (neqok) and (Length(heap.Values) > 1) then
    begin
      SetLength(ccids, Length(heap.Values));
      for j := 0 to High(heap.Values) do
        ccids[j] := heap.Values[j].ID;
      SetLength(Conflicts, Length(Conflicts) + 1);
      Conflicts[High(Conflicts)] := TConflict.Create(ccids, Self);
    end;
  end;

  procedure CheckHeap;
  var j, k: Integer;
  begin
    neqok := False;
    if Length(heap.Values) < 1 then
      Exit;
    for j := Length(EqualFields) to High(cvs) do
      cvs[j] := heap.Values[0].Data[j - Length(EqualFields)];
    for j := 0 to High(heap.Values) do
    begin
      for k := Length(EqualFields) to High(cvs) do
        if cvs[k] <> heap.Values[j].Data[k - Length(EqualFields)] then
        begin
          neqok := True;
          Break;
        end;
      if neqok then
        Break;
    end;
  end;

begin
  heap := TIDDateBinaryHeap.Create();
  SetLength(Conflicts, 0);
  with ConflictsDM.SQLQuery do
  begin;
    Close;
    SQL.Text := TValueConflictQuery.GetSelectQuery(
      Metadata.TimetableTable, EqualFields, NotEqualFields);
    Open;
    First;
    SetLength(cvs, Length(EqualFields) + Length(NotEqualFields));
    SetLength(temp, Length(NotEqualFields));
    SetLength(ccids, 1);
    for i := 0 to High(EqualFields) do
      cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
    for i := 0 to High(NotEqualFields) do
      cvs[i + Length(EqualFields)] :=
        FieldByName(NotEqualFields[i].DisplayName).AsString;
    for i := 0 to High(NotEqualFields) do
      temp[i] := FieldByName(NotEqualFields[i].DisplayName).AsString;
    heap.Push(IDDateData(FieldByName('ID').AsInteger,
      FieldByName(Metadata.PeriodEndCol.DisplayName).AsDateTime, temp));
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
        if (FieldByName(Metadata.PeriodStartCol.DisplayName).AsDateTime > heap.Values[0].EndDate) then
        begin
          NewConflict;
          while (Length(heap.Values) > 0) and
            (FieldByName(Metadata.PeriodStartCol.DisplayName).AsDateTime > heap.Values[0].EndDate)
          do
            heap.Pop;
          CheckHeap;
        end;
        if EOF then
        begin
          NewConflict;
          Break;
        end;
      end
      else
      begin
        if (neqok) and (Length(heap.Values) > 1) then
          NewConflict;
        while (Length(heap.Values) > 0) do
          heap.Pop;
        for i := 0 to High(EqualFields) do
          cvs[i] := FieldByName(EqualFields[i].DisplayName).AsString;
        for i := 0 to High(NotEqualFields) do
          cvs[i + Length(EqualFields)] :=
            FieldByName(NotEqualFields[i].DisplayName).AsString;
        eqok := True;
        neqok := False;
      end;
      for i := 0 to High(NotEqualFields) do
        temp[i] := FieldByName(NotEqualFields[i].DisplayName).AsString;
      heap.Push(IDDateData(FieldByName('ID').AsInteger,
        FieldByName(Metadata.PeriodEndCol.DisplayName).AsDateTime, temp));
      Next;
    end;
  end;
  heap.Free;
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

