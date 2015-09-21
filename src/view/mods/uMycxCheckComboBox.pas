unit uMycxCheckComboBox;

interface

uses
  // Delphi
  SysUtils,
  // Dev Express
  cxControls, cxEdit, cxCheckComboBox, cxLookAndFeelPainters, cxCheckBox,
  // Utils
  uStringUtils;

type
  TMycxCheckComboBox = class(TcxCheckComboBox)
  protected
    function InternalIndexOf(AStr: string): Integer;
    function GetTextValue: string;
    procedure SetTextValue(const TextValue: string);
  public
    procedure CheckAll;
  published
    property TextValue: string read GetTextValue write SetTextValue;
  end;

implementation

{ TMycxCheckComboBox }

function TMycxCheckComboBox.InternalIndexOf(AStr: string): Integer;
var
  _Index, _Count: Integer;
  _Found: Boolean;
begin
  Result := -1;

  _Index := 0;
  _Found := False;
  _Count := Properties.Items.Count;

  while (_Index < _Count) and not _Found do
  begin
    _Found := SameStr(Properties.Items.Items[_Index].Description, AStr);
    if not _Found then
      Inc(_Index);
  end;

  if _Found then
    Result := _Index;
end;

function TMycxCheckComboBox.GetTextValue: string;
begin
  Result := Text;
end;

procedure TMycxCheckComboBox.CheckAll;
var
  I: Integer;
  AStates: TcxCheckStates;
begin
  with Properties do
  begin
    SetLength(AStates, Items.Count);
    for I := 0 to Items.Count - 1 do
      AStates[I] := cbsChecked;
    Value := CalculateCheckStatesValue(AStates, Items, EditValueFormat);
  end;
end;

procedure TMycxCheckComboBox.SetTextValue(const TextValue: string);
var
  I, Index: Integer;
begin
  with SplittString(Properties.Delimiter[1], TextValue) do
    try
      for I := 0 to Count - 1 do
      begin
        Index := InternalIndexOf(Strings[I]);

        if Index <> -1 then
          States[Index] := cbsChecked;
      end;
    finally
      Free;
    end;
end;

end.
