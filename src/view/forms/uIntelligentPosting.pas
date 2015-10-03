unit uIntelligentPosting;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, Math,
  // DevExpress
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel, cxTextEdit, StdCtrls, cxButtons, cxListBox;

type
  TIntelligentPosting = class(TForm)
    cxLSearchValue: TcxLabel;
    cxTESearchValue: TcxTextEdit;
    cxBNewSearch: TcxButton;
    cxLSearchResults: TcxLabel;
    cxLBSearchResults: TcxListBox;
    cxBAccept: TcxButton;
    cxBCancel: TcxButton;
    procedure cxLBSearchResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TIntelligentPostingClass = class
    class function IntelligentPostingHandler(var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool; safecall;
  end;

implementation

{$R *.dfm}
{ TIntelligentPostingClass }

class function TIntelligentPostingClass.IntelligentPostingHandler;
var
  _ModalResult: Integer;
begin

  with TIntelligentPosting.Create(nil) do
    try
      cxTESearchValue.Text := ASearchValue;
      cxLBSearchResults.Items.Text := ASearchResults;
      cxLBSearchResults.ItemIndex := Min(ASearchIndex, cxLBSearchResults.Count - 1);

      _ModalResult := ShowModal;

      ASearchValue := cxTESearchValue.Text;
      ASearchIndex := cxLBSearchResults.ItemIndex;
      ARedoSearch := (_ModalResult = mrRetry);
      Result := (_ModalResult = mrOk) or (_ModalResult = mrRetry);
    finally
      Free;
    end;

end;

procedure TIntelligentPosting.cxLBSearchResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  lstIndex: Integer;
  OriginalHint: string;
begin
  with cxLBSearchResults do
  begin
    OriginalHint := Hint;
    lstIndex := SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X, Y));
    if (lstIndex >= 0) and (lstIndex <= Items.Count) then
      Hint := Items[lstIndex]
    else
      Hint := '';
    if not SameStr(Hint, OriginalHint) then
      Application.CancelHint;
  end;
end;

end.
