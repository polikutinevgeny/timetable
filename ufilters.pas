unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, Forms, StdCtrls;

type

  { TFilter }

  TFilter = class
    private
      FColCB: TComboBox;
      FActCB: TComboBox;
      FValTE: TEdit;
      function GetCol: TCol;
      function GetActionIndex: Integer;
      function GetValue: String;
    public
      constructor Create(AScrollbox: TScrollBox; ATable: TTable);
      property Column: TCol read GetCol;
      property ActionIndex: Integer read GetActionIndex;
      property Value: String read GetValue;
      destructor Destroy; override;
  end;

const Actions = ['=', '>', '<', '>=', '<='];

implementation

{ TFilter }

function TFilter.GetCol: TCol;
begin

end;

function TFilter.GetAction: TFilterAction;
begin

end;

function TFilter.GetValue: String;
begin

end;

constructor TFilter.Create(AScrollbox: TScrollBox; ATable: TTable);
begin
  FColCB := TComboBox.Create(AScrollbox);
  FColCB.Parent := AScrollbox;
  FColCB.Items.AddStrings(ATable.ColNames);
  FColCB.ItemIndex := 0;
  FColCB.Top := 10 + AScrollbox.Tag * 40;
  FColCB.Left := 20;
  FActCB := TComboBox.Create(AScrollbox);
  FActCB.Parent := AScrollbox;
  FActCB.Items.AddStrings(Actions);
  FActCB.ItemIndex := 0;
  FActCB.Top := 10 + AScrollbox.Tag * 40;
  FActCB.Left := 120;
  FValTE := TEdit.Create(AScrollbox);
  FValTE.Parent := AScrollbox;
  FValTE.Text := '';
  FValTE.Top := 10 + AScrollbox.Tag * 40;
  FValTE.Left := 220;
  AScrollbox.Tag := AScrollbox.Tag + 1;
end;

destructor TFilter.Destroy;
begin
  FActCB.Free;
  FColCB.Free;
  FValTE.Free;
end;

end.

