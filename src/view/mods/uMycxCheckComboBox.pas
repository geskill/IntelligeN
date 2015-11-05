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
  LItemIndex: Integer;
  LStates: TcxCheckStates;
begin
  with Properties do
  begin
    SetLength(LStates, Items.Count);
    for LItemIndex := 0 to Items.Count - 1 do
      LStates[LItemIndex] := cbsChecked;
    Value := CalculateCheckStatesValue(LStates, Items, EditValueFormat);
  end;
end;

procedure TMycxCheckComboBox.SetTextValue(const TextValue: string);
var
  LCategoryIndex, LItemIndex: Integer;
begin
  with SplittString(Properties.Delimiter[1], TextValue) do
    try
      Value := '';
      for LCategoryIndex := 0 to Count - 1 do
      begin
        LItemIndex := InternalIndexOf(Strings[LCategoryIndex]);

        if (LItemIndex <> -1) then
          States[LItemIndex] := cbsChecked;
      end;
    finally
      Free;
    end;
end;

end.
