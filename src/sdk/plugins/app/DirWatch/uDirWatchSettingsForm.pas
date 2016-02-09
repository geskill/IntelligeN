unit uDirWatchSettingsForm;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, FileCtrl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Export
  uExport,
  //
  uDirWatchSettings;

type
  TfDirWatchSettingsForm = class(TForm)
    eDirectoryPath: TEdit;
    sbSearchDirectory: TSpeedButton;
    gbDirectoryFile: TGroupBox;
    lDirectoryPath: TLabel;
    gbSettings: TGroupBox;
    cbWatchSubdirectories: TCheckBox;
    bSave: TButton;
    bClose: TButton;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure sbSearchDirectoryClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    FDirWatchSettings: TDirWatchSettings;
  public
    procedure LoadSettings(const ADirWatchSettings: TDirWatchSettings);
    procedure SaveSettings();
  end;

var
  fDirWatchSettingsForm: TfDirWatchSettingsForm;

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

procedure TfDirWatchSettingsForm.bSaveClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TfDirWatchSettingsForm.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfDirWatchSettingsForm.LoadSettings(const ADirWatchSettings: TDirWatchSettings);
begin
  FDirWatchSettings := ADirWatchSettings;

  eDirectoryPath.Text := ADirWatchSettings.DirWatchPath;
  cbWatchSubdirectories.Checked := ADirWatchSettings.WatchSubdirectories;
end;

procedure TfDirWatchSettingsForm.SaveSettings;
begin
  FDirWatchSettings.DirWatchPath := eDirectoryPath.Text;
  FDirWatchSettings.WatchSubdirectories := cbWatchSubdirectories.Checked;

  FDirWatchSettings.SaveSettings;
end;

end.
