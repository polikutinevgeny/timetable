unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, Forms, StdCtrls,
  Buttons, Graphics;

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
      FValueTE: TEdit;
      FRemoveBtn: TRemoveButton;
      FTable: TTable;
      FCols: TColArray;
      function GetCol: TCol;
      function GetAction: String;
      function GetValue: String;
      procedure FilterUpdate(Sender: TObject);
      procedure FilterRemove(Sender: TObject);
      procedure CreateColumnCB(AScrollbox: TScrollBox; ACols: TColArray);
      procedure CreateActionCB(AScrollbox: TScrollBox);
      procedure CreateValueTE(AScrollbox: TScrollBox);
      procedure CreateRemoveBtn(AScrollbox: TScrollBox);
    public
      constructor Create(AScrollbox: TScrollBox; ATable: TTable;
        ACols: TColArray);
      procedure Draw(AScrollbox: TScrollBox);
      property Column: TCol read GetCol;
      property Action: String read GetAction;
      property Value: String read GetValue;
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
  Actions: array[0..5] of String = ('=', '>', '<', '>=', '<=', 'LIKE');
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

function TFilter.GetValue: String;
begin
  Result := FValueTE.Text;
end;

procedure TFilter.FilterUpdate(Sender: TObject);
begin
  OnFilterUpdate;
end;

procedure TFilter.FilterRemove(Sender: TObject);
begin
  FRemoveBtn := nil; //Button will remove itself after click
  OnFilterUpdate;
  OnFilterRemove(Self);
  Free;
end;

procedure TFilter.CreateColumnCB(AScrollbox: TScrollBox; ACols: TColArray);
var i: Integer;
begin
  FColumnCB := TComboBox.Create(AScrollbox);
  for i := 0 to High(ACols) do
  begin
    FColumnCB.Items.Add(ACols[i].DisplayName);
    SetLength(FCols, Length(ACols));
    FCols[i] := ACols[i];
  end;
  with FColumnCB do
  begin
    Visible := False;
    Width := 150;
    ReadOnly := True;
    OnChange := @FilterUpdate;
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
    Parent := AScrollbox;
  end;
end;

procedure TFilter.CreateValueTE(AScrollbox: TScrollBox);
begin
  FValueTE := TEdit.Create(AScrollbox);
  with FValueTE do
  begin
    Visible := False;
    Text := '';
    Width := 300;
    OnChange := @FilterUpdate;
    Parent := AScrollbox;
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
    OnClick := @FilterRemove;
  end;
end;

constructor TFilter.Create(AScrollbox: TScrollBox; ATable: TTable;
  ACols: TColArray);
begin
  CreateColumnCB(AScrollbox, ACols);
  CreateActionCB(AScrollbox);
  CreateValueTE(AScrollbox);
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
  FValueTE.Top := UpperPadding + AScrollbox.Tag * Interval;
  FValueTE.Left := 270;
  FRemoveBtn.Top := UpperPadding + AScrollbox.Tag * Interval;
  FRemoveBtn.Left := 590;
  FColumnCB.Visible := True;
  FActionCB.Visible := True;
  FValueTE.Visible := True;
  FRemoveBtn.Visible := True;
  AScrollbox.Tag := AScrollbox.Tag + 1;
end;

destructor TFilter.Destroy;
begin
  FActionCB.Free;
  FColumnCB.Free;
  FValueTE.Free;
  FRemoveBtn.Free;
end;

initialization
  RemoveGlyph := TBitmap.Create;
  RemoveGlyph.LoadFromFile('icons/Remove.bmp');
  RemoveGlyph.TransparentColor := clWhite;
  RemoveGlyph.Transparent := True;
end.

