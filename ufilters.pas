unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, Forms, StdCtrls, LMessages, LCLIntf, messages;

type

  { TRemoveButton }

  TRemoveButton = class(TButton)
    procedure Click; override;
    procedure HandleRelease(var Msg: TMessage); message CM_RELEASE;
  end;

  TFilterUpdateEvent = procedure(Sender: TObject) of object;
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

const
  Actions: array[0..5] of String = ('=', '>', '<', '>=', '<=', 'LIKE');

implementation

{ TRemoveButton }

procedure TRemoveButton.Click;
begin
  inherited Click;
  PostMessage(Handle, CM_RELEASE, 0, 0);
end;

procedure TRemoveButton.HandleRelease(var Msg: TMessage);
begin
  Free;
end;

{ TFilter }

function TFilter.GetCol: TCol;
begin
  Result := FCols[FColumnCB.ItemIndex];
end;

function TFilter.GetAction: String;
begin
  Result := Actions[FActionCB.ItemIndex];
end;

function TFilter.GetValue: String;
begin
  Result := FValueTE.Text;
end;

procedure TFilter.FilterUpdate(Sender: TObject);
begin
  OnFilterUpdate(Sender);
end;

procedure TFilter.FilterRemove(Sender: TObject);
begin
  FRemoveBtn := nil; //Button will remove itself after click
  OnFilterUpdate(Sender);
  OnFilterRemove(Self);
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
  FColumnCB.Visible := False;
  FColumnCB.ItemIndex := 0;
  FColumnCB.Width := 150;
  FColumnCB.ReadOnly := True;
  FColumnCB.OnChange := @FilterUpdate;
  FColumnCB.Parent := AScrollbox;
end;

procedure TFilter.CreateActionCB(AScrollbox: TScrollBox);
begin
  FActionCB := TComboBox.Create(AScrollbox);
  FActionCB.Visible := False;
  FActionCB.Items.AddStrings(Actions);
  FActionCB.ItemIndex := 0;
  FActionCB.Width := 80;
  FActionCB.ReadOnly := True;
  FActionCB.OnChange := @FilterUpdate;
  FActionCB.Parent := AScrollbox;
end;

procedure TFilter.CreateValueTE(AScrollbox: TScrollBox);
begin
  FValueTE := TEdit.Create(AScrollbox);
  FValueTE.Visible := False;
  FValueTE.Text := '';
  FValueTE.Width := 300;
  FValueTE.OnChange := @FilterUpdate;
  FValueTE.Parent := AScrollbox;
end;

procedure TFilter.CreateRemoveBtn(AScrollbox: TScrollBox);
begin
  FRemoveBtn := TRemoveButton.Create(AScrollbox);
  FRemoveBtn.Visible := False;
  FRemoveBtn.Caption := '-';
  FRemoveBtn.Width := 34;
  FRemoveBtn.Height := 34;
  FRemoveBtn.Parent := AScrollbox;
  FRemoveBtn.OnClick := @FilterRemove;
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
  FColumnCB.Top := 20 + AScrollbox.Tag * 40;
  FColumnCB.Left := 20;
  FActionCB.Top := 20 + AScrollbox.Tag * 40;
  FActionCB.Left := 180;
  FValueTE.Top := 20 + AScrollbox.Tag * 40;
  FValueTE.Left := 270;
  FRemoveBtn.Top := 20 + AScrollbox.Tag * 40;
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

end.

