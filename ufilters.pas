unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, Forms, StdCtrls;

type

  TFilterUpdateEvent = procedure(Sender: TObject) of object;

  { TFilter }

  TFilter = class
    private
      FOnFilterUpdate: TFilterUpdateEvent;
      FColCB: TComboBox;
      FActCB: TComboBox;
      FValTE: TEdit;
      FTable: TTable;
      FCols: TColArray;
      function GetCol: TCol;
      function GetAction: String;
      function GetValue: String;
      procedure FilterUpdate(Sender: TObject);
    public
      constructor Create(AScrollbox: TScrollBox; ATable: TTable;
        ACols: TColArray);
      property Column: TCol read GetCol;
      property Action: String read GetAction;
      property Value: String read GetValue;
      property OnFilterUpdate: TFilterUpdateEvent read FOnFilterUpdate
        write FOnFilterUpdate;
      destructor Destroy; override;
  end;

  TFilterArray = array of TFilter;

const
  Actions: array[0..5] of String = ('=', '>', '<', '>=', '<=', 'LIKE');

implementation

{ TFilter }

function TFilter.GetCol: TCol;
begin
  Result := FCols[FColCB.ItemIndex];
end;

function TFilter.GetAction: String;
begin
  Result := Actions[FActCB.ItemIndex];
end;

function TFilter.GetValue: String;
begin
  Result := FValTE.Text;
end;

procedure TFilter.FilterUpdate(Sender: TObject);
begin
  OnFilterUpdate(Sender);
end;

constructor TFilter.Create(AScrollbox: TScrollBox; ATable: TTable;
  ACols: TColArray);
var i: Integer;
begin
  FColCB := TComboBox.Create(AScrollbox);
  for i := 0 to High(ACols) do
  begin
    FColCB.Items.Add(ACols[i].DisplayName);
    SetLength(FCols, Length(ACols));
    FCols[i] := ACols[i];
  end;
  FColCB.ItemIndex := 0;
  FColCB.Top := 10 + AScrollbox.Tag * 40;
  FColCB.Left := 20;
  FColCB.OnChange := @FilterUpdate;
  FColCB.Parent := AScrollbox;
  FActCB := TComboBox.Create(AScrollbox);
  FActCB.Items.AddStrings(Actions);
  FActCB.ItemIndex := 0;
  FActCB.Top := 10 + AScrollbox.Tag * 40;
  FActCB.Left := 120;
  FActCB.OnChange := @FilterUpdate;
  FActCB.Parent := AScrollbox;
  FValTE := TEdit.Create(AScrollbox);
  FValTE.Text := '';
  FValTE.Top := 10 + AScrollbox.Tag * 40;
  FValTE.Left := 220;
  FValTE.OnChange := @FilterUpdate;
  FValTE.Parent := AScrollbox;
  AScrollbox.Tag := AScrollbox.Tag + 1;
  FTable := ATable;
end;

destructor TFilter.Destroy;
begin
  FActCB.Free;
  FColCB.Free;
  FValTE.Free;
end;

end.

