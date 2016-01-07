unit uUpdate;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellAPI,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxProgressBar, cxLabel, cxTextEdit, cxRadioGroup,
  // DLLs
  uExport,
  // Api
  uApiUpdate;

type
  TUpdate = class(TForm)
    cxLMessage: TcxLabel;
    cxPBUpdateStatus: TcxProgressBar;
    bRestartProgram: TButton;
    bClose: TButton;
    cxRBUpgrade: TcxRadioButton;
    cxRBUpdate: TcxRadioButton;
    bDownload: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure cxRBClick(Sender: TObject);
    procedure bDownloadClick(Sender: TObject);
    procedure bRestartProgramClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    FUpdateController: TUpdateController;
  protected
    procedure UpdateSearching(Sender: TObject);
    procedure UpdateNoChanges(Sender: TObject);
    procedure UpdateHasChanges(Sender: TObject; UpdateVersions: TUpdateVersions);
    procedure UpdateError(Sender: TObject; ErrorMsg: string);
    procedure UpdateStartDownload(Sender: TObject);
    procedure UpdateDownloading(Sender: TObject; Position: Integer);
    procedure UpdateFinishedDownload(Sender: TObject);
  public
    property UpdateController: TUpdateController read FUpdateController;
  end;

var
  Update: TUpdate;

implementation

uses
  // Api
  uApiSettings;

{$R *.dfm}

procedure TUpdate.FormCreate(Sender: TObject);
begin
  FUpdateController := TUpdateController.Create(SettingsManager.Settings.HTTP.GetProxy(psaMain), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
  FUpdateController.OnUpdateSearching := UpdateSearching;
  FUpdateController.OnUpdateNoChanges := UpdateNoChanges;
  FUpdateController.OnUpdateHasChanges := UpdateHasChanges;
  FUpdateController.OnUpdateError := UpdateError;
  FUpdateController.OnUpdateStartDownload := UpdateStartDownload;
  FUpdateController.OnUpdateDownloading := UpdateDownloading;
  FUpdateController.OnUpdateFinishedDownload := UpdateFinishedDownload;

  if SettingsManager.Settings.CheckForUpdates then
    UpdateController.CheckForUpdates(Self);
end;

procedure TUpdate.FormDestroy(Sender: TObject);
begin
  FUpdateController.Free;
end;

procedure TUpdate.FormShow(Sender: TObject);
begin
  // UpdateController.CheckForUpdates; // check on menu item click
end;

procedure TUpdate.cxRBClick(Sender: TObject);
begin
  bDownload.Enabled := (cxRBUpgrade.Checked) or (cxRBUpdate.Checked);
end;

procedure TUpdate.bDownloadClick(Sender: TObject);
begin
  if cxRBUpgrade.Checked then
    UpdateController.DownloadUpgrade
  else
    UpdateController.DownloadUpdate;
end;

procedure TUpdate.bRestartProgramClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(GetHiddenDataDir + 'update\exec_update.bat'), nil, PChar(GetHiddenDataDir + 'update'), SW_SHOW);
  Application.Terminate;
end;

procedure TUpdate.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdate.UpdateSearching(Sender: TObject);
begin
  cxLMessage.Caption := 'searching ...';
  cxPBUpdateStatus.Visible := False;
end;

procedure TUpdate.UpdateNoChanges(Sender: TObject);
begin
  bRestartProgram.Visible := False;
  cxLMessage.Caption := 'No update is available';
  cxPBUpdateStatus.Visible := False;
end;

procedure TUpdate.UpdateHasChanges(Sender: TObject; UpdateVersions: TUpdateVersions);

  function GetUpdateTitle(AUpdateVersion: TUpdateVersion): string;
  begin
    result := AUpdateVersion.UpdateVersion.ToString + ' (' + DateTimeToStr(AUpdateVersion.UpdateCreated) + ')';

    if (AUpdateVersion.UpdateVersion.IsPreRelease) then
      result := 'PRE-RELEASE ' + result;
  end;

begin
  bRestartProgram.Visible := False;
  bDownload.Visible := True;

  cxLMessage.Caption := 'An update is available:';

  if (UpdateVersions.Upgrade.UpdateFiles.Count > 0) then
  begin
    cxRBUpgrade.Caption := GetUpdateTitle(UpdateVersions.Upgrade);
    cxRBUpgrade.Visible := True;
    cxRBUpgrade.Checked := False;

    cxRBUpdate.Top := 70;

    bDownload.Enabled := True;
  end
  else
  begin
    cxRBUpdate.Top := 52;
  end;

  if (UpdateVersions.Update.UpdateFiles.Count > 0) then
  begin
    cxRBUpdate.Caption := GetUpdateTitle(UpdateVersions.Update);
    cxRBUpdate.Checked := True;
    cxRBUpdate.Visible := True;

    bDownload.Enabled := True;

    if SettingsManager.Settings.CheckForUpdates then
      UpdateController.DownloadUpdate;
  end;
end;

procedure TUpdate.UpdateError(Sender: TObject; ErrorMsg: string);
begin
  if (ErrorMsg = '') then
    ErrorMsg := 'unknown error (maybe timeout, try again later)';
  cxLMessage.Caption := 'error: ' + ErrorMsg;
end;

procedure TUpdate.UpdateStartDownload(Sender: TObject);
begin
  bDownload.Enabled := False;
  bDownload.Visible := False;

  cxRBUpgrade.Visible := False;
  cxRBUpdate.Visible := False;

  cxLMessage.Caption := 'Downloading ...';
  cxPBUpdateStatus.Position := 0;

  cxPBUpdateStatus.Visible := True;
end;

procedure TUpdate.UpdateDownloading(Sender: TObject; Position: Integer);
begin
  cxPBUpdateStatus.Position := Position - 1;
end;

procedure TUpdate.UpdateFinishedDownload(Sender: TObject);
begin
  cxLMessage.Caption := 'Update finished';
  cxPBUpdateStatus.Position := 100;
  bRestartProgram.Visible := True;
end;

end.
