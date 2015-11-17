unit uSelectTemplateFileName;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  // DLLs
  uExport,
  // Utils
  uFileUtils;

type
  TSelectTemplateFileName = class(TForm)
    lTemplateFile: TLabel;
    cobTemplateFileName: TComboBox;
    bAccept: TButton;
    procedure cobTemplateFileNameChange(Sender: TObject);
    procedure bAcceptClick(Sender: TObject);
  protected
    FTemplateFileName: string;
  public
    function Execute: Boolean;
    property TemplateFileName: string read FTemplateFileName write FTemplateFileName;
  end;

implementation

{$R *.dfm}

procedure TSelectTemplateFileName.cobTemplateFileNameChange(Sender: TObject);
begin
  FTemplateFileName := cobTemplateFileName.Text;
end;

procedure TSelectTemplateFileName.bAcceptClick(Sender: TObject);
begin
  CloseModal;
end;

function TSelectTemplateFileName.Execute: Boolean;
var
  StringList: TStrings;
begin
  StringList := TStringList.Create;
  try
    GetFilesInDirectory(GetTemplatesTypeFolder, '*.xml', StringList, false, false, false, true);
    cobTemplateFileName.Items.Text := StringList.Text;
  finally
    StringList.Free;
  end;
  cobTemplateFileName.Text := FTemplateFileName;
  ShowModal;
  Result := (ModalResult = mrOK);
end;

end.
