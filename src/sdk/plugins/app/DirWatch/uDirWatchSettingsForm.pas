unit uDirWatchSettingsForm;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, FileCtrl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Export
  uExport,
  // Utils
  uSystemUtils,
  // DirWatch
  uDirWatchSettings;

type
  TfDirWatchSettingsForm = class(TForm)
    eDirectoryPath: TEdit;
    sbSearchDirectory: TSpeedButton;
    gbDirectoryFile: TGroupBox;
    lDirectoryPath: TLabel;
    gbSettings: TGroupBox;
    cbLoadFilesOnlyOnce: TCheckBox;
    cbLoadAlreadyExistingFiles: TCheckBox;
    cbWatchSubdirectories: TCheckBox;
    cbLoadOnyXMLFiles: TCheckBox;
    cbLoadOnlyIntelligeNXML2Files: TCheckBox;
    cbRunCrawlers: TCheckBox;
    cbRunCrypters: TCheckBox;
    cbRunSave: TCheckBox;
    cbRunPublish: TCheckBox;
    cbRunPublishOnlyWithCustomCheck: TCheckBox;
    sbCreateEmptyCustomCheckScriptFile: TSpeedButton;
    ePublishCustomCheckScriptFile: TEdit;
    sbSearchCustomCheckScriptFile: TSpeedButton;
    bReset: TButton;
    bSave: TButton;
    bClose: TButton;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure sbSearchDirectoryClick(Sender: TObject);
    procedure cbLoadOnyXMLFilesClick(Sender: TObject);
    procedure cbRunPublishClick(Sender: TObject);
    procedure cbRunPublishOnlyWithCustomCheckClick(Sender: TObject);
    procedure sbCreateEmptyCustomCheckScriptFileClick(Sender: TObject);
    procedure sbSearchCustomCheckScriptFileClick(Sender: TObject);
    procedure bResetClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    FDirWatchSettings: TDirWatchSettings;
    procedure ValidateIntelligeNXML2Enabled;
    procedure ValidateRunPublishEnabled;
    procedure ValidateRunPublishOnlyWithCustomCheckEnabled;
  public
    procedure LoadSettings(const ADirWatchSettings: TDirWatchSettings);
    procedure SaveSettings();
  end;

var
  fDirWatchSettingsForm: TfDirWatchSettingsForm;

implementation

{$R *.dfm}

procedure TfDirWatchSettingsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

procedure TfDirWatchSettingsForm.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure TfDirWatchSettingsForm.sbSearchDirectoryClick(Sender: TObject);
var
  LDir: string;
begin
  LDir := GetRootDir;
  if (SelectDirectory('Select a directory to monitor', '', LDir)) then
    eDirectoryPath.Text := ExtractRelativePath(GetModulePath, LDir);
end;

procedure TfDirWatchSettingsForm.cbLoadOnyXMLFilesClick(Sender: TObject);
begin
  ValidateIntelligeNXML2Enabled;
end;

procedure TfDirWatchSettingsForm.cbRunPublishClick(Sender: TObject);
begin
  ValidateRunPublishEnabled
end;

procedure TfDirWatchSettingsForm.cbRunPublishOnlyWithCustomCheckClick(Sender: TObject);
begin
  ValidateRunPublishOnlyWithCustomCheckEnabled;
end;

procedure TfDirWatchSettingsForm.sbCreateEmptyCustomCheckScriptFileClick(Sender: TObject);
var
  LConfigFile: TStringList;
  LControlID: TControlID;
begin
  LConfigFile := TStringList.Create;
  try
    LConfigFile.Add('// This file has been automatically created by the dir watch plugin');
    LConfigFile.Add('');
    LConfigFile.Add('');
    for LControlID := Low(TControlID) to High(TControlID) do
    begin
      LConfigFile.Add('// This function is used for the ' + ControlIDToString(LControlID) + '-control check');
      LConfigFile.Add('function Validate' + ControlIDToString(LControlID) + '()');
      LConfigFile.Add('{');
      LConfigFile.Add('  return True;');
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
          ePublishCustomCheckScriptFile.Text := ExtractRelativePath(GetModulePath, FileName);
        end;
      finally
        Free;
      end;
  finally
    LConfigFile.Free;
  end;
end;

procedure TfDirWatchSettingsForm.sbSearchCustomCheckScriptFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := 'Text files (*.txt)|*.txt';

      if Execute then
        ePublishCustomCheckScriptFile.Text := ExtractRelativePath(GetModulePath, FileName);
    finally
      Free;
    end;
end;

procedure TfDirWatchSettingsForm.bResetClick(Sender: TObject);
begin
  LoadSettings(FDirWatchSettings);
end;

procedure TfDirWatchSettingsForm.bSaveClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TfDirWatchSettingsForm.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfDirWatchSettingsForm.ValidateIntelligeNXML2Enabled;
begin
  cbLoadOnlyIntelligeNXML2Files.Enabled := cbLoadOnyXMLFiles.Checked;
end;

procedure TfDirWatchSettingsForm.ValidateRunPublishEnabled;
begin
  cbRunPublishOnlyWithCustomCheck.Enabled := cbRunPublish.Checked;
  ValidateRunPublishOnlyWithCustomCheckEnabled;
end;

procedure TfDirWatchSettingsForm.ValidateRunPublishOnlyWithCustomCheckEnabled;
begin
  sbCreateEmptyCustomCheckScriptFile.Enabled := cbRunPublish.Checked and cbRunPublishOnlyWithCustomCheck.Checked;
  ePublishCustomCheckScriptFile.Enabled := cbRunPublish.Checked and cbRunPublishOnlyWithCustomCheck.Checked;
  sbSearchCustomCheckScriptFile.Enabled := cbRunPublish.Checked and cbRunPublishOnlyWithCustomCheck.Checked;
end;

procedure TfDirWatchSettingsForm.LoadSettings(const ADirWatchSettings: TDirWatchSettings);
begin
  FDirWatchSettings := ADirWatchSettings;

  eDirectoryPath.Text := ADirWatchSettings.DirWatchPath;
  cbLoadFilesOnlyOnce.Checked := ADirWatchSettings.LoadFilesOnlyOnce;
  cbLoadAlreadyExistingFiles.Checked := ADirWatchSettings.LoadAlreadyExistingFiles;
  cbWatchSubdirectories.Checked := ADirWatchSettings.WatchSubdirectories;
  cbLoadOnyXMLFiles.Checked := ADirWatchSettings.LoadOnyXMLFiles;
  cbLoadOnlyIntelligeNXML2Files.Checked := ADirWatchSettings.LoadOnlyIntelligeNXML2Files;
  cbRunCrawlers.Checked := ADirWatchSettings.RunCrawlers;
  cbRunCrypters.Checked := ADirWatchSettings.RunCrypters;
  cbRunSave.Checked := ADirWatchSettings.RunSave;
  cbRunPublish.Checked := ADirWatchSettings.RunPublish;
  cbRunPublishOnlyWithCustomCheck.Checked := ADirWatchSettings.RunPublishOnlyWithCustomCheck;
  ePublishCustomCheckScriptFile.Text := ADirWatchSettings.PublishCustomCheckScriptFile;

  ValidateIntelligeNXML2Enabled;
  ValidateRunPublishEnabled;
  ValidateRunPublishOnlyWithCustomCheckEnabled;
end;

procedure TfDirWatchSettingsForm.SaveSettings;
begin
  FDirWatchSettings.DirWatchPath := eDirectoryPath.Text;
  FDirWatchSettings.LoadFilesOnlyOnce := cbLoadFilesOnlyOnce.Checked;
  FDirWatchSettings.LoadAlreadyExistingFiles := cbLoadAlreadyExistingFiles.Checked;
  FDirWatchSettings.WatchSubdirectories := cbWatchSubdirectories.Checked;
  FDirWatchSettings.LoadOnyXMLFiles := cbLoadOnyXMLFiles.Checked;
  FDirWatchSettings.LoadOnlyIntelligeNXML2Files := cbLoadOnlyIntelligeNXML2Files.Checked;
  FDirWatchSettings.RunCrawlers := cbRunCrawlers.Checked;
  FDirWatchSettings.RunCrypters := cbRunCrypters.Checked;
  FDirWatchSettings.RunSave := cbRunSave.Checked;
  FDirWatchSettings.RunPublish := cbRunPublish.Checked;
  FDirWatchSettings.RunPublishOnlyWithCustomCheck := cbRunPublishOnlyWithCustomCheck.Checked;
  FDirWatchSettings.PublishCustomCheckScriptFile := ePublishCustomCheckScriptFile.Text;

  FDirWatchSettings.SaveSettings;
end;

end.
