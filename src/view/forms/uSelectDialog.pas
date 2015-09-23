unit uSelectDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,

  cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxGraphics, cxLookAndFeels;

type
  TSelectDialog = class(TForm)
    cxCOBSelect: TcxComboBox;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    Label1: TLabel;
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
  private
    FOverrideQuestion: Boolean;
    FStartSelectedItem: string;

    function GetDescription: string;
    procedure SetDescription(ADescription: string);
    function GetItems: TStrings;
    procedure SetItems(AItems: TStrings);
    function GetSelectedItem: string;
    procedure SetSelectedItem(ASelectedItem: string);
  public
    property Description: string read GetDescription write SetDescription;
    property Items: TStrings read GetItems write SetItems;
    property SelectedItem: string read GetSelectedItem write SetSelectedItem;

    function Execute(AOverrideQuestion: Boolean = True): Boolean;
  end;

implementation

{$R *.dfm}

procedure TSelectDialog.cxButton1Click(Sender: TObject);
begin
  if FOverrideQuestion and (cxCOBSelect.Properties.Items.IndexOf(SelectedItem) >= 0) then
    if (MessageDlg('Override ' + SelectedItem + ' layout?', mtConfirmation, [mbyes, mbno], 0) <> mrYes) then
    begin
      ModalResult := mrNone;
      Exit;
    end;

  CloseModal;
end;

procedure TSelectDialog.cxButton2Click(Sender: TObject);
begin
  CloseModal;
end;

function TSelectDialog.GetDescription;
begin
  Result := Label1.Caption;
end;

procedure TSelectDialog.SetDescription(ADescription: string);
begin
  Label1.Caption := ADescription;
end;

function TSelectDialog.GetItems;
begin
  Result := cxCOBSelect.Properties.Items;
end;

procedure TSelectDialog.SetItems(AItems: TStrings);
begin
  cxCOBSelect.Properties.Items := AItems;
end;

function TSelectDialog.GetSelectedItem;
begin
  Result := cxCOBSelect.Text;
end;

procedure TSelectDialog.SetSelectedItem(ASelectedItem: string);
begin
  FStartSelectedItem := ASelectedItem;
  cxCOBSelect.Text := ASelectedItem;
end;

function TSelectDialog.Execute(AOverrideQuestion: Boolean = True): Boolean;
begin
  FOverrideQuestion := AOverrideQuestion;
  ShowModal;
  Result := (ModalResult = mrOK);
end;

end.
