unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, Forms, StdCtrls, EditBtn, DateTimeCtrls, DateTimePicker,
  Buttons, Graphics, dateutils, Dialogs;

type
  { TRemoveButton }

  TRemoveButton = class(TSpeedButton)
    procedure Click; override;
  end;

  TFilterUpdateEvent = procedure of object;
  TFilterRemoveEvent = procedure(Sender: TObject) of object;

  { TFilter }

  TFilter = class
    private
      FOnFilterUpdate: TFilterUpdateEvent;
      FOnFilterRemove: TFilterRemoveEvent;
      FColumnCB: TComboBox;
      FActionCB: TComboBox;
      FStringTE: TEdit;
      FDateDE: TDateEdit;
      FIntegerTE: TEdit;
      FTimeDTP: TDateTimePicker;
      FRemoveBtn: TRemoveButton;
      FTable: TTable;
      FCols: TColArray;
      FEnabled: Boolean;
      FLastType: TColDataType;
      function GetCol: TCol;
      function GetAction: String;
      function GetValue: Variant;
      procedure FilterUpdate(Sender: TObject);
      procedure FilterRemove(Sender: TObject);
      procedure CreateColumnCB(AScrollbox: TScrollBox; ACols: array of TCol);
      procedure CreateActionCB(AScrollbox: TScrollBox);
      procedure CreateStringTE(AScrollbox: TScrollBox);
      procedure CreateIntegerTE(AScrollbox: TScrollBox);
      procedure CreateDateTE(AScrollbox: TScrollBox);
      procedure CreateTimeTE(AScrollbox: TScrollBox);
      procedure CreateRemoveBtn(AScrollbox: TScrollBox);
      procedure SelectEditor;
    public
      constructor Create(AScrollbox: TScrollBox; ATable: TTable;
        ACols: array of TCol; AEnabled: Boolean = True);
      procedure Draw(AScrollbox: TScrollBox);
      procedure SetupHiddenFilter(AValue: String; AAction: String = '=');
      procedure SetupHiddenFilter(AValues: array of Integer);
      function Copy(AScrollbox: TScrollBox): TFilter;
      property Visible: Boolean read FEnabled;
      property Column: TCol read GetCol;
      property Action: String read GetAction;
      property Value: Variant read GetValue;
      property OnFilterUpdate: TFilterUpdateEvent read FOnFilterUpdate
        write FOnFilterUpdate;
      property OnFilterRemove: TFilterRemoveEvent read FOnFilterRemove
        write FOnFilterRemove;
      destructor Destroy; override;
  end;

  TFilterArray = array of TFilter;

  TColumnException = class(Exception);
  TActionException = class(Exception);

implementation

const
  Actions: array[0..6] of String = ('=', '>', '<', '>=', '<=', 'LIKE', 'IN');
  Interval: Integer = 40;
  UpperPadding: Integer = 20;

var RemoveGlyph: TBitmap;

{ TRemoveButton }

procedure TRemoveButton.Click;
begin
  inherited Click;
  Free;
end;

{ TFilter }

function TFilter.GetCol: TCol;
begin
  if FColumnCB.ItemIndex = -1 then
    raise TColumnException.Create('Please, select column');
  Result := FCols[FColumnCB.ItemIndex];
end;

function TFilter.GetAction: String;
begin
  if FColumnCB.ItemIndex = -1 then
    raise TActionException.Create('Please, select action');
  Result := Actions[FActionCB.ItemIndex];
end;

function TFilter.GetValue: Variant;
begin
  if FColumnCB.ItemIndex = -1 then
    raise TActionException.Create('Please, select column');
  case FCols[FColumnCB.ItemIndex].DataType of
    cdtDate: Result := FDateDE.Date;
    cdtInteger: Result := FIntegerTE.Text;
    cdtString: Result := FStringTE.Text;
    cdtTime: Result := FTimeDTP.Time;
  end;
end;

procedure TFilter.FilterUpdate(Sender: TObject);
begin
  SelectEditor;
  if FEnabled then
    OnFilterUpdate;
end;

procedure TFilter.FilterRemove(Sender: TObject);
begin
  FRemoveBtn := nil; //Button will remove itself after click
  OnFilterUpdate;
  OnFilterRemove(Self);
  Free;
end;

procedure TFilter.CreateColumnCB(AScrollbox: TScrollBox; ACols: array of TCol);
var i: Integer;
begin
  FColumnCB := TComboBox.Create(AScrollbox);
  SetLength(FCols, Length(ACols));
  for i := 0 to High(ACols) do
  begin
    FColumnCB.Items.Add(ACols[i].DisplayName);
    FCols[i] := ACols[i];
  end;
  with FColumnCB do
  begin
    Visible := False;
    Width := 150;
    ReadOnly := True;
    OnChange := @FilterUpdate;
    Enabled := FEnabled;
    Parent := AScrollbox;
  end;
end;

procedure TFilter.CreateActionCB(AScrollbox: TScrollBox);
begin
  FActionCB := TComboBox.Create(AScrollbox);
  with FActionCB do
  begin
    Visible := False;
    Items.AddStrings(Actions);
    Width := 80;
    ReadOnly := True;
    OnChange := @FilterUpdate;
    Enabled := FEnabled;
    Parent := AScrollbox;
  end;
end;

procedure TFilter.CreateStringTE(AScrollbox: TScrollBox);
begin
  FStringTE := TEdit.Create(AScrollbox);
  with FStringTE do
  begin
    Visible := False;
    Text := '';
    Width := 300;
    OnChange := @FilterUpdate;
    Enabled := FEnabled;
    Parent := AScrollbox;
  end;
end;

procedure TFilter.CreateIntegerTE(AScrollbox: TScrollBox);
begin
  FIntegerTE := TEdit.Create(AScrollbox);
  with FIntegerTE do
  begin
    Visible := False;
    Text := '';
    Width := 300;
    OnChange := @FilterUpdate;
    Enabled := FEnabled;
    Parent := AScrollbox;
    NumbersOnly := True;
  end;
end;

procedure TFilter.CreateDateTE(AScrollbox: TScrollBox);
begin
  FDateDE := TDateEdit.Create(AScrollbox);
  with FDateDE do
  begin
    Visible := False;
    Date := Today;
    Width := 300;
    OnChange := @FilterUpdate;
    Enabled := FEnabled;
    Parent := AScrollbox;
  end;
end;

procedure TFilter.CreateTimeTE(AScrollbox: TScrollBox);
begin
  FTimeDTP := TDateTimePicker.Create(AScrollbox);
  with FTimeDTP do
  begin
    Visible := False;
    Kind := dtkTime;
    DateMode := dmUpDown;
    DateSeparator := '.';
    Time := StrToTime('00:00');
    Parent := AScrollbox;
    Enabled := FEnabled;
    OnChange := @FilterUpdate;
  end;
end;

procedure TFilter.CreateRemoveBtn(AScrollbox: TScrollBox);
begin
  FRemoveBtn := TRemoveButton.Create(AScrollbox);
  with FRemoveBtn do
  begin
    Visible := False;
    Width := 34;
    Height := 34;
    Glyph := RemoveGlyph;
    Hint := 'Remove filter';
    ShowHint := True;
    Flat := True;
    Parent := AScrollbox;
    Enabled := FEnabled;
    OnClick := @FilterRemove;
  end;
end;

procedure TFilter.SelectEditor;
begin
  if FColumnCB.ItemIndex = -1 then
    Exit;
  if FCols[FColumnCB.ItemIndex].DataType = FLastType then
    Exit;
  FDateDE.Visible := False;
  FIntegerTE.Visible := False;
  FStringTE.Visible := False;
  FTimeDTP.Visible := False;
  case FCols[FColumnCB.ItemIndex].DataType of
    cdtDate: FDateDE.Visible := True;
    cdtInteger: FIntegerTE.Visible := True;
    cdtString: FStringTE.Visible := True;
    cdtTime: FTimeDTP.Visible := True;
  end;
  FLastType := FCols[FColumnCB.ItemIndex].DataType;
end;

constructor TFilter.Create(AScrollbox: TScrollBox; ATable: TTable;
  ACols: array of TCol; AEnabled: Boolean);
begin
  FEnabled := AEnabled;
  FLastType := cdtNull;
  CreateColumnCB(AScrollbox, ACols);
  CreateActionCB(AScrollbox);
  CreateStringTE(AScrollbox);
  CreateDateTE(AScrollbox);
  CreateIntegerTE(AScrollbox);
  CreateTimeTE(AScrollbox);
  CreateRemoveBtn(AScrollbox);
  Draw(AScrollbox);
  FTable := ATable;
end;

procedure TFilter.Draw(AScrollbox: TScrollBox);
begin
  FColumnCB.Top := UpperPadding + AScrollbox.Tag * Interval;
  FColumnCB.Left := 20;
  FActionCB.Top := UpperPadding + AScrollbox.Tag * Interval;
  FActionCB.Left := 180;
  FRemoveBtn.Top := UpperPadding + AScrollbox.Tag * Interval;
  FRemoveBtn.Left := 590;
  FStringTE.Top := UpperPadding + AScrollbox.Tag * Interval;
  FStringTE.Left := 270;
  FTimeDTP.Top := UpperPadding + AScrollbox.Tag * Interval;
  FTimeDTP.Left := 270;
  FDateDE.Top := UpperPadding + AScrollbox.Tag * Interval;
  FDateDE.Left := 270;
  FIntegerTE.Top := UpperPadding + AScrollbox.Tag * Interval;
  FIntegerTE.Left := 270;
  FColumnCB.Visible := True;
  FActionCB.Visible := True;
  FRemoveBtn.Visible := True;
  SelectEditor;
  AScrollbox.Tag := AScrollbox.Tag + 1;
end;

procedure TFilter.SetupHiddenFilter(AValue: String; AAction: String);
begin
  FActionCB.ItemIndex := FActionCB.Items.IndexOf(AAction);
  FColumnCB.ItemIndex := 0;
  case FCols[FColumnCB.ItemIndex].DataType of
    cdtDate: FDateDE.Date := StrToDate(AValue);
    cdtInteger: FIntegerTE.Text := AValue;
    cdtString: FStringTE.Text := AValue;
    cdtTime: FTimeDTP.Time := StrToTime(AValue);
  end;
end;

procedure TFilter.SetupHiddenFilter(AValues: array of Integer);
var i: Integer;
begin
  FActionCB.ItemIndex := FActionCB.Items.IndexOf('IN');
  FColumnCB.ItemIndex := 0;
  FIntegerTE.Text := IntToStr(AValues[0]);
  for i := 1 to High(AValues) do
    FIntegerTE.Text := FIntegerTE.Text + ',' + IntToStr(AValues[i]);
end;

function TFilter.Copy(AScrollbox: TScrollBox): TFilter;
begin
  Result := TFilter.Create(AScrollbox, FTable, FCols);
  Result.FActionCB.ItemIndex := FActionCB.ItemIndex;
  Result.FColumnCB.ItemIndex := FColumnCB.ItemIndex;
  Result.FStringTE.Text := FStringTE.Text;
  Result.FIntegerTE.Text := FIntegerTE.Text;
  Result.FDateDE.Date := FDateDE.Date;
  Result.FTimeDTP.Time := FTimeDTP.Time;
end;

destructor TFilter.Destroy;
begin
  FActionCB.Free;
  FColumnCB.Free;
  FStringTE.Free;
  FRemoveBtn.Free;
  FDateDE.Free;
  FIntegerTE.Free;
  FTimeDTP.Free;
end;

initialization
  RemoveGlyph := TBitmap.Create;
  RemoveGlyph.LoadFromFile('icons/Remove.bmp');
  RemoveGlyph.TransparentColor := clWhite;
  RemoveGlyph.Transparent := True;
end.

