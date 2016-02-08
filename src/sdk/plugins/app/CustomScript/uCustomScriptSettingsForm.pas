unit uCustomScriptSettingsForm;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  //
  uCustomScriptSettings;

type
  TfCustomScriptSettingsForm = class(TForm)
    eScriptFilePath: TEdit;
    sbSearchFile: TSpeedButton;
    sbCreateEmptyScriptFile: TSpeedButton;
    gbScriptFile: TGroupBox;
    lScriptFilePath: TLabel;
    gbSettings: TGroupBox;
    bSave: TButton;
    bClose: TButton;
    cbUseFoundAsValue: TCheckBox;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure sbSearchFileClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure sbCreateEmptyScriptFileClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    FCustomScriptSettings: TCustomScriptSettings;
  public
    procedure LoadSettings(const ACustomScriptSettings: TCustomScriptSettings);
    procedure SaveSettings();
  end;

var
  fCustomScriptSettingsForm: TfCustomScriptSettingsForm;

implementation

{$R *.dfm}

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

procedure TfCustomScriptSettingsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

procedure TfCustomScriptSettingsForm.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure TfCustomScriptSettingsForm.sbCreateEmptyScriptFileClick(Sender: TObject);
var
  LConfigFile: TStringList;
  LControlID: TControlID;
begin
  LConfigFile := TStringList.Create;
  try
    LConfigFile.Add('// This file has been automatically created by the custom script plugin');
    LConfigFile.Add('');
    LConfigFile.Add('');
    for LControlID := Low(TControlID) to High(TControlID) do
    begin
      LConfigFile.Add('// This function is used for the ' + ControlIDToString(LControlID) + '-control');
      LConfigFile.Add('function Get' + ControlIDToString(LControlID) + '(AProposedValues)');
      LConfigFile.Add('{');
      LConfigFile.Add('  return False;');
      LConfigFile.Add('}');
      LConfigFile.Add('');
    end;

    with TSaveDialog.Create(nil) do
      try
        Filter := 'Text files (*.txt)|*.txt';

        if Execute then
        begin
          if not(ExtractFileExt(FileName) = '.txt') then
            FileName := FileName + '.txt';
          LConfigFile.SaveToFile(FileName);
          eScriptFilePath.Text := ExtractRelativePath(GetModulePath, FileName);
        end;
      finally
        Free;
      end;
  finally
    LConfigFile.Free;
  end;
end;

procedure TfCustomScriptSettingsForm.sbSearchFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := 'Text files (*.txt)|*.txt';

      if Execute then
        eScriptFilePath.Text := ExtractRelativePath(GetModulePath, FileName);
    finally
      Free;
    end;
end;

procedure TfCustomScriptSettingsForm.bSaveClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TfCustomScriptSettingsForm.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfCustomScriptSettingsForm.LoadSettings(const ACustomScriptSettings: TCustomScriptSettings);
begin
  FCustomScriptSettings := ACustomScriptSettings;

  eScriptFilePath.Text := ACustomScriptSettings.ScriptFileName;
  cbUseFoundAsValue.Checked := ACustomScriptSettings.OverrideValue;
end;

procedure TfCustomScriptSettingsForm.SaveSettings;
begin
  FCustomScriptSettings.ScriptFileName := eScriptFilePath.Text;
  FCustomScriptSettings.OverrideValue := cbUseFoundAsValue.Checked;

  FCustomScriptSettings.SaveSettings;
end;

end.
