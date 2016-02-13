unit uMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, TypInfo, Generics.Collections,
  ShellAPI, ComCtrls,
  // DevExpress
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxNavigator, cxCheckBox, cxLabel, cxTextEdit, cxDropDownEdit, cxBlobEdit, cxGridCustomTableView, cxGridTableView, cxGridCustomView,
  cxClasses, cxGridLevel, cxGrid, cxMemo, cxContainer, cxMaskEdit, cxSpinEdit, dxCustomWizardControl, dxWizardControl,
  // Spring Framework
  Spring.Utils, Spring.SystemUtils,
  // Export
  uDynamicExport,
  // Api
  uApiUpdateConst, uApiUpdateInterface, uApiUpdateSettings, uApiUpdateController,
  // Utils
  uFileUtils, uPathUtils, uSetUtils, uStringUtils;

type
  TfMain = class(TForm)
    WizardControl: TdxWizardControl;
    wcpWelcomePage: TdxWizardControlPage;
    wcpLocalFiles: TdxWizardControlPage;
    rbAddNewPath: TRadioButton;
    eRootDir: TEdit;
    sbSelectRootDir: TSpeedButton;
    rbSelectExisting: TRadioButton;
    lbSelectPath: TListBox;
    cxGLocalFiles: TcxGrid;
    cxGLocalFilesLevel: TcxGridLevel;
    cxGLocalFilesTableView: TcxGridTableView;
    cxGLocalFilesTableViewColumn1: TcxGridColumn;
    cxGLocalFilesTableViewColumn2: TcxGridColumn;
    cxGLocalFilesTableViewColumn3: TcxGridColumn;
    cxGLocalFilesTableViewColumn4: TcxGridColumn;
    cxGLocalFilesTableViewColumn5: TcxGridColumn;
    cxGLocalFilesTableViewColumn6: TcxGridColumn;
    lFileSystem: TLabel;
    wcpPageServer: TdxWizardControlPage;

    wcpPublish: TdxWizardControlPage;
    wcpPageServerInfo: TdxWizardControlPage;
    rbAddNewServer: TRadioButton;
    eServerDir: TEdit;
    rbSelectExistingServer: TRadioButton;
    lbSelectServer: TListBox;
    eServerAccessToken: TEdit;
    lServerAccessToken: TLabel;
    lConnectToServer: TLabel;
    lRecivingUpdateVersions: TLabel;
    lRecivingFTPServer: TLabel;
    lRecivingUpdateFiles: TLabel;
    wcpUpdateFiles: TdxWizardControlPage;
    lServerInfoError: TLabel;
    eServerInfoError: TEdit;
    cxGUpdateFiles: TcxGrid;
    cxGUpdateFilesTableView: TcxGridTableView;
    cxGUpdateFilesTableViewColumn1: TcxGridColumn;
    cxGUpdateFilesTableViewColumn2: TcxGridColumn;
    cxGUpdateFilesTableViewColumn3: TcxGridColumn;
    cxGUpdateFilesTableViewColumn4: TcxGridColumn;
    cxGUpdateFilesTableViewColumn5: TcxGridColumn;
    cxGUpdateFilesTableViewColumn6: TcxGridColumn;
    cxGUpdateFilesLevel: TcxGridLevel;
    wcpUpdateVersion: TdxWizardControlPage;
    wcpUploadFiles: TdxWizardControlPage;
    rbAddNewVersion: TRadioButton;
    rbSelectExistingVersion: TRadioButton;
    lbSelectVersion: TListBox;
    cxSEMajorVersion: TcxSpinEdit;
    cxSEMinorVersion: TcxSpinEdit;
    cxSEMajorBuild: TcxSpinEdit;
    cxSEMinorBuild: TcxSpinEdit;
    lPreRelease: TLabel;
    cxCBLocalFilesEnableDisableAll: TcxCheckBox;
    lCompressingLocalFiles: TLabel;
    lUploadingLocalFiles: TLabel;
    pbUploadProgress: TProgressBar;
    lUploadInfoError: TLabel;
    eUploadInfoError: TEdit;
    wcpUpdateServer: TdxWizardControlPage;
    lUpdateInfoError: TLabel;
    lAddingTheNewSystems: TLabel;
    lAddVersion: TLabel;
    eUpdateInfoError: TEdit;
    lRetrieveFilesFromServer: TLabel;
    bShowHTTPLogger: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WizardControlButtonClick(Sender: TObject; AKind: TdxWizardControlButtonKind; var AHandled: Boolean);
    procedure WizardControlPageChanging(Sender: TObject; ANewPage: TdxWizardControlCustomPage; var AAllow: Boolean);
    { *************************************** STEP - 1 *************************************** }
    procedure rbSelectFileSystem(Sender: TObject);
    procedure eRootDirChange(Sender: TObject);
    procedure sbSelectRootDirClick(Sender: TObject);
    procedure lbSelectPathClick(Sender: TObject);
    { *************************************** STEP - 2 *************************************** }
    procedure rbSelectServer(Sender: TObject);
    procedure eServerDirChange(Sender: TObject);
    procedure lbSelectServerClick(Sender: TObject);
    { *************************************** STEP - 3 *************************************** }
    { *************************************** STEP - 4 *************************************** }
    procedure cxCBLocalFilesEnableDisableAllPropertiesChange(Sender: TObject);
    procedure cxGLocalFilesTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure cxGLocalFilesTableViewColumn5GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure cxGLocalFilesTableViewDataControllerDataChanged(Sender: TObject);
    procedure cxGUpdateFilesTableViewColumn5CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure lFileSystemClick(Sender: TObject);
    { *************************************** STEP - 5 *************************************** }
    procedure cxGUpdateFilesTableViewColumn6GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure rbSelectVersion(Sender: TObject);
    procedure cxSEMajorVersionPropertiesChange(Sender: TObject);
    procedure cxSEMinorVersionPropertiesChange(Sender: TObject);
    procedure cxSEMajorBuildPropertiesChange(Sender: TObject);
    procedure cxSEMinorBuildPropertiesChange(Sender: TObject);
    procedure lbSelectVersionClick(Sender: TObject);
    { *************************************** STEP - 6 *************************************** }
    { *************************************** STEP - 7 *************************************** }
    { *************************************** STEP - 8 *************************************** }
    { **************************************************************************************** }
    procedure bShowHTTPLoggerClick(Sender: TObject);
  private
  var
    FStoreUpdateFilesPath: string;
    FActiveUpdateFileCollectionItem: TUpdateFileSystemCollectionItem;
    FActiveUpdateServerCollectionItem: TUpdateServerCollectionItem;

    FActiveVersionsList: TUpdateManagerVersionsList;
    FActiveUpdateFTPServer: IFTPServer;
    FActiveSystemsList: TUpdateManagerSystemsList;

    FVersionID: Integer;
    FActiveLocalFiles: TUpdateManagerLocalFileList;
    FActiveOnlineFiles: TUpdateManagerOnlineFileList;

    function GetLocalFilesCheckAllStatus: Byte;
    procedure SetLocalFilesCheckAllStatus;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure CanContinue(AValue: Boolean);
    procedure CheckCanContinueToServer;
    procedure CheckCanContinueToServerInfo;
    procedure CheckCanContinueToUpdateFiles;
    procedure CheckCanContinueToUpdateServer;
  protected
    // procedure SetLEDStatus(AStatus: Boolean; ALED: TJvLED; AJump: Boolean = False);
    function LoadServerInfos: Boolean;
    function LoadLocalFilesList: Boolean;
    function LoadUpdateFilesList: Boolean;
    function UpdateServer: Boolean;
    function UploadFiles: Boolean;
    function ActivateVersion: Boolean;
  public

  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}
{ }
{ }
{$IFDEF DEBUG}

uses
  uHTTPLogger;
{$ENDIF}

procedure TfMain.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG}
  bShowHTTPLogger.Visible := True;
{$ENDIF}
  FStoreUpdateFilesPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'update_files\';
  ForceDirectories(FStoreUpdateFilesPath);
  cxGLocalFilesTableView.DataController.OnDataChanged := nil;
  LoadSettings;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FActiveVersionsList) then
    FActiveVersionsList.Free;

  if Assigned(FActiveUpdateFTPServer) then
    FActiveUpdateFTPServer := nil;

  if Assigned(FActiveSystemsList) then
    FActiveSystemsList.Free;

  if Assigned(FActiveLocalFiles) then
    FActiveLocalFiles.Free;

  if Assigned(FActiveOnlineFiles) then
    FActiveOnlineFiles.Free;

  SaveSettings;
end;

procedure TfMain.WizardControlButtonClick(Sender: TObject; AKind: TdxWizardControlButtonKind; var AHandled: Boolean);
var
  LFileIndex: Integer;
begin
  case AKind of
    wcbkCancel:
      begin
        Close;
      end;
    wcbkNext:
      begin
        if (WizardControl.ActivePage = wcpWelcomePage) then
        begin
          if rbAddNewPath.Checked then
          begin
            FActiveUpdateFileCollectionItem := TUpdateFileSystemCollectionItem(SettingsManager.Settings.FileSystems.Add);
            FActiveUpdateFileCollectionItem.LibraryFile := eRootDir.Text;
          end
          else if rbSelectExisting.Checked then
            FActiveUpdateFileCollectionItem := SettingsManager.Settings.FindFileSystem(lbSelectPath.Items[lbSelectPath.ItemIndex]);
        end
        else if (WizardControl.ActivePage = wcpPageServer) then
        begin
          if rbAddNewServer.Checked then
          begin
            FActiveUpdateServerCollectionItem := TUpdateServerCollectionItem(SettingsManager.Settings.UpdateServers.Add);
            FActiveUpdateServerCollectionItem.Name := eServerDir.Text;
            FActiveUpdateServerCollectionItem.AccessToken := eServerAccessToken.Text;
          end
          else if rbSelectExistingServer.Checked then
            FActiveUpdateServerCollectionItem := SettingsManager.Settings.FindServer(lbSelectServer.Items[lbSelectServer.ItemIndex]);
        end
        else if (WizardControl.ActivePage = wcpPageServerInfo) then
        begin
          // DO NOTHING
        end
        else if (WizardControl.ActivePage = wcpLocalFiles) then
        begin
          for LFileIndex := 0 to FActiveLocalFiles.Count - 1 do
            with cxGLocalFilesTableView.DataController, FActiveLocalFiles[LFileIndex].LocalFile do
            begin
              Status := Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index];
              // Action := TEnum.Parse<TUpdateAction>(VarToStr(Values[LFileIndex, cxGLocalFilesTableViewColumn5.Index]));
            end;
        end
        else if (WizardControl.ActivePage = wcpUpdateFiles) then
        begin
          for LFileIndex := 0 to FActiveLocalFiles.Count - 1 do
            with cxGUpdateFilesTableView.DataController, FActiveLocalFiles[LFileIndex].LocalFile do
            begin
              // Status := Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index];
              Action := TEnum.Parse<TUpdateAction>(VarToStr(Values[LFileIndex, cxGUpdateFilesTableViewColumn5.Index]));
            end;
        end
        else if (WizardControl.ActivePage = wcpUpdateVersion) then
        begin
          //
        end
        else if (WizardControl.ActivePage = wcpUpdateServer) then
        begin
          //
        end
        else if (WizardControl.ActivePage = wcpUploadFiles) then
        begin
          //
        end

      end;
    wcbkFinish:
      begin
        if (WizardControl.ActivePage = wcpPublish) then
        begin
          if ActivateVersion then
          begin
            ShowMessage('Update is now online and ready to use.');
            Close;
          end
          else
          begin
            ShowMessage('The was an error. Start over again.');
          end;
        end;
      end;
  end;
end;

procedure TfMain.WizardControlPageChanging(Sender: TObject; ANewPage: TdxWizardControlCustomPage; var AAllow: Boolean);
var
  LCanContinue: Boolean;
  LFileVersion: TFileVersionInfo;
  LVersionIndex: Integer;
begin
  cxGLocalFilesTableView.DataController.OnDataChanged := nil;

  if (ANewPage = wcpWelcomePage) then
  begin
    lbSelectPath.Items.Text := SettingsManager.Settings.GetLibraryFiles;
    CheckCanContinueToServer;
  end
  else if (ANewPage = wcpPageServer) then
  begin
    lbSelectServer.Items.Text := SettingsManager.Settings.GetUpdateServers;
    CheckCanContinueToServerInfo;
  end
  else if (ANewPage = wcpPageServerInfo) then
  begin
    CanContinue(LoadServerInfos);
  end
  else if (ANewPage = wcpLocalFiles) then
  begin
    LCanContinue := LoadLocalFilesList;
    SetLocalFilesCheckAllStatus;
    lFileSystem.Caption := ExtractFilePath(FActiveUpdateFileCollectionItem.LibraryFile);
    cxGLocalFilesTableView.DataController.OnDataChanged := cxGLocalFilesTableViewDataControllerDataChanged;
    CanContinue(LCanContinue);
  end
  else if (ANewPage = wcpUpdateFiles) then
  begin
    CanContinue(LoadUpdateFilesList);
  end
  else if (ANewPage = wcpUpdateVersion) then
  begin
    LFileVersion := TFileVersionInfo.GetVersionInfo(FActiveUpdateFileCollectionItem.LibraryFile);

    with LFileVersion.FileVersionNumber do
    begin
      cxSEMajorVersion.Value := Major;
      cxSEMinorVersion.Value := Minor;
      cxSEMajorBuild.Value := Build;
      cxSEMinorBuild.Value := Reversion;
    end;

    for LVersionIndex := 0 to FActiveVersionsList.Count - 1 do
      lbSelectVersion.Items.Add(FActiveVersionsList[LVersionIndex].ToString);

    CheckCanContinueToUpdateServer;
  end
  else if (ANewPage = wcpUpdateServer) then
  begin
    CanContinue(UpdateServer);
  end
  else if (ANewPage = wcpUploadFiles) then
  begin
    pbUploadProgress.Visible := False;
    CanContinue(UploadFiles);
  end


  // ;

end;

{ *************************************** STEP - 1 *************************************** }

procedure TfMain.rbSelectFileSystem(Sender: TObject);
begin
  eRootDir.Enabled := rbAddNewPath.Checked;
  sbSelectRootDir.Enabled := rbAddNewPath.Checked;

  lbSelectPath.Enabled := rbSelectExisting.Checked;

  CheckCanContinueToServer;
end;

procedure TfMain.eRootDirChange(Sender: TObject);
begin
  CheckCanContinueToServer;
end;

procedure TfMain.sbSelectRootDirClick(Sender: TObject);
var
  LDir: string;
begin
  LDir := eRootDir.Text;

  with TOpenDialog.Create(nil) do
    try
      Filter := 'IntelligeN 2009 FileSystem (' + INTELLIGEN_FILESYSTEM_LIB + ')|' + INTELLIGEN_FILESYSTEM_LIB;
      if Execute then
        eRootDir.Text := FileName;
    finally
      Free;
    end;
end;

procedure TfMain.lbSelectPathClick(Sender: TObject);
begin
  CheckCanContinueToServer;
end;

{ *************************************** STEP - 2 *************************************** }

procedure TfMain.rbSelectServer(Sender: TObject);
begin
  eServerDir.Enabled := rbAddNewServer.Checked;
  eServerAccessToken.Enabled := rbAddNewServer.Checked;

  lbSelectServer.Enabled := rbSelectExistingServer.Checked;

  CheckCanContinueToServerInfo;
end;

procedure TfMain.eServerDirChange(Sender: TObject);
begin
  CheckCanContinueToServerInfo;
end;

procedure TfMain.lbSelectServerClick(Sender: TObject);
begin
  CheckCanContinueToServerInfo;
end;

{ *************************************** STEP - 3 *************************************** }

{ *************************************** STEP - 4 *************************************** }

procedure TfMain.cxCBLocalFilesEnableDisableAllPropertiesChange(Sender: TObject);

  procedure SetStatus(AStatus: Boolean);
  var
    LFileIndex: Integer;
  begin
    with cxGLocalFilesTableView.DataController do
    begin
      OnDataChanged := nil;

      BeginUpdate;
      try
        for LFileIndex := 0 to RecordCount - 1 do
          if not(Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index] = AStatus) then
            Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index] := AStatus;
      finally
        EndUpdate;
      end;

      OnDataChanged := cxGLocalFilesTableViewDataControllerDataChanged;
      // Do here everything EXCEPT "SetLocalFilesCheckAllStatus" from "cxGLocalFilesTableViewDataControllerDataChanged"
      CheckCanContinueToUpdateFiles;
    end;
  end;

begin
  case cxCBLocalFilesEnableDisableAll.State of
    cbsUnchecked:
      SetStatus(False);
    cbsChecked:
      SetStatus(True);
  end;
end;

procedure TfMain.cxGLocalFilesTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  V: Variant;
begin
  V := AViewInfo.Value;

  if not VarIsNull(V) then
  begin
    if Pos('New', V) > 0 then
      AViewInfo.EditViewInfo.TextColor := clPurple
    else if Pos('Mis', V) > 0 then
      AViewInfo.EditViewInfo.TextColor := clRed
    else
      AViewInfo.EditViewInfo.TextColor := clBlack;

    AViewInfo.EditViewInfo.Paint(ACanvas);

    ADone := True;
  end;
end;

procedure TfMain.cxGLocalFilesTableViewColumn5GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  TcxCustomComboBoxProperties(AProperties).Items.Text := ARecord.Values[cxGLocalFilesTableViewColumn6.Index];
end;

procedure TfMain.cxGLocalFilesTableViewDataControllerDataChanged(Sender: TObject);
begin
  SetLocalFilesCheckAllStatus;
  CheckCanContinueToUpdateFiles;
end;

procedure TfMain.lFileSystemClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(lFileSystem.Caption), nil, nil, SW_SHOW);
end;

{ *************************************** STEP - 5 *************************************** }

procedure TfMain.cxGUpdateFilesTableViewColumn5CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  V: Variant;
begin
  V := AViewInfo.Value;

  if not VarIsNull(V) then
  begin
    if Pos('Add', V) > 0 then
      AViewInfo.EditViewInfo.TextColor := clPurple
    else if Pos('Edit', V) > 0 then
      AViewInfo.EditViewInfo.TextColor := clBlue
    else if Pos('Delete', V) > 0 then
      AViewInfo.EditViewInfo.TextColor := clRed
    else
      AViewInfo.EditViewInfo.TextColor := clBlack;

    AViewInfo.EditViewInfo.Paint(ACanvas);

    ADone := True;
  end;
end;

procedure TfMain.cxGUpdateFilesTableViewColumn6GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  TcxCustomComboBoxProperties(AProperties).Items.Text := ARecord.Values[cxGUpdateFilesTableViewColumn6.Index];
end;

{ *************************************** STEP - 6 *************************************** }

procedure TfMain.rbSelectVersion(Sender: TObject);
begin
  cxSEMajorVersion.Enabled := rbAddNewVersion.Checked;
  cxSEMinorVersion.Enabled := rbAddNewVersion.Checked;
  cxSEMajorBuild.Enabled := rbAddNewVersion.Checked;
  cxSEMinorBuild.Enabled := rbAddNewVersion.Checked;

  lbSelectVersion.Enabled := rbSelectExistingVersion.Checked;

  CheckCanContinueToUpdateServer;
end;

procedure TfMain.cxSEMajorVersionPropertiesChange(Sender: TObject);
begin
  CheckCanContinueToUpdateServer;
end;

procedure TfMain.cxSEMinorVersionPropertiesChange(Sender: TObject);
begin
  CheckCanContinueToUpdateServer;
end;

procedure TfMain.cxSEMajorBuildPropertiesChange(Sender: TObject);
begin
  CheckCanContinueToUpdateServer;
end;

procedure TfMain.cxSEMinorBuildPropertiesChange(Sender: TObject);
begin
  CheckCanContinueToUpdateServer;
  lPreRelease.Visible := not(cxSEMinorBuild.Value = 0);
end;

procedure TfMain.lbSelectVersionClick(Sender: TObject);
begin
  CheckCanContinueToUpdateServer;
end;

{ *************************************** STEP - 6 *************************************** }

{ *************************************** STEP - 7 *************************************** }

{ *************************************** STEP - 8 *************************************** }

{ ****************************************************************************** }

function TfMain.GetLocalFilesCheckAllStatus: Byte;
var
  LRecordCount: Integer;
  LStatus: Byte; // 0 = Unchecked, 1 = Checked, 3 = Grayed, 255 = Undefined
begin
  LStatus := 255;

  with cxGLocalFilesTableView.DataController do
    for LRecordCount := 0 to RecordCount - 1 do
    begin
      case Values[LRecordCount, cxGLocalFilesTableViewColumn1.index] of
        0:
          if (LStatus = 255) or (LStatus = 0) then
            LStatus := 0
          else
            LStatus := 2;
      else
        if (LStatus = 255) or (LStatus = 1) then
          LStatus := 1
        else
          LStatus := 2;
      end;
    end;
  if (LStatus = 255) then
    LStatus := 2;

  Result := LStatus;
end;

procedure TfMain.SetLocalFilesCheckAllStatus;
var
  LEvent: TNotifyEvent;
begin
  with cxCBLocalFilesEnableDisableAll do
  begin
    LEvent := Properties.OnChange;
    Properties.OnChange := nil;

    case GetLocalFilesCheckAllStatus of
      0:
        State := cbsUnchecked;
      1:
        State := cbsChecked;
    else
      State := cbsGrayed;
    end;

    Properties.OnChange := LEvent;
  end;
end;

procedure TfMain.LoadSettings;
begin

end;

procedure TfMain.SaveSettings;
begin

end;

{ ****************************************************************************** }

procedure TfMain.CanContinue(AValue: Boolean);
begin
  WizardControl.Buttons.Next.Enabled := AValue;
end;

procedure TfMain.CheckCanContinueToServer;
begin
  FActiveUpdateFileCollectionItem := nil;
  if rbAddNewPath.Checked then
    CanContinue(FileExists(eRootDir.Text) and (Pos(INTELLIGEN_FILESYSTEM_LIB, eRootDir.Text) > 0) and (lbSelectPath.Items.IndexOf(eRootDir.Text) = -1))
  else if rbSelectExisting.Checked then
    CanContinue((lbSelectPath.ItemIndex <> -1) and FileExists(lbSelectPath.Items[lbSelectPath.ItemIndex]));
end;

procedure TfMain.CheckCanContinueToServerInfo;
begin
  FActiveUpdateServerCollectionItem := nil;
  if rbAddNewServer.Checked then
    CanContinue((length(eServerDir.Text) > 10) and (Pos('http://', eServerDir.Text) > 0) and (lbSelectServer.Items.IndexOf(eServerDir.Text) = -1))
  else if rbSelectExistingServer.Checked then
    CanContinue((lbSelectServer.ItemIndex <> -1));
end;

procedure TfMain.CheckCanContinueToUpdateFiles;
var
  LFileIndex: Integer;
  LCanContinue: Boolean;
begin
  LCanContinue := False;
  with cxGLocalFilesTableView.DataController do
    for LFileIndex := 0 to RecordCount - 1 do
    begin
      if Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index] then
      begin
        LCanContinue := True;
        break;
      end;
    end;
  CanContinue(LCanContinue);
end;

procedure TfMain.CheckCanContinueToUpdateServer;

  function FileVersionToStr(MajorVersion, MinorVersion, MajorBuild, MinorBuild: Integer): string;
  begin
    Result := IntToStr(MajorVersion) + '.' + IntToStr(MinorVersion) + '.' + IntToStr(MajorBuild) + '.' + IntToStr(MinorBuild);
  end;

begin
  if rbAddNewVersion.Checked then
  begin
    CanContinue((lbSelectVersion.Items.IndexOf(FileVersionToStr(cxSEMajorVersion.Value, cxSEMinorVersion.Value, cxSEMajorBuild.Value, cxSEMinorBuild.Value)) = -1))
  end
  else if rbSelectExistingVersion.Checked then
    CanContinue((lbSelectVersion.ItemIndex <> -1));
end;

{ ****************************************************************************** }

{
procedure TfMain.SetLEDStatus(AStatus: Boolean; ALED: TJvLED; AJump: Boolean = False);
begin
  with ALED do
  begin
    case AStatus of
      True:
        case AJump of
          True:
            ColorOn := clBlue;
        else
          ColorOn := clLime;
        end;
    else
      ColorOn := clRed;
    end;
    Status := True;
  end;
end;
}

function TfMain.LoadServerInfos: Boolean;

  procedure SetErrorMsg(AStatus: Boolean; AMsg: string);
  begin
    with eServerInfoError do
    begin
      lServerInfoError.Visible := not AStatus;
      Visible := not AStatus;
      case AStatus of
        True:
          Text := '';
      else
        Text := AMsg;
      end;
    end;
  end;

var
  LLocalUploadController: TLocalUploadController;

  LStatus: WordBool;
  LErrorMsg: WideString;
begin
  LStatus := False;

  if Assigned(FActiveVersionsList) then
    FActiveVersionsList.Free;

  if Assigned(FActiveUpdateFTPServer) then
    FActiveUpdateFTPServer := nil;

  if Assigned(FActiveSystemsList) then
    FActiveSystemsList.Free;

  LLocalUploadController := TLocalUploadController.Create(FActiveUpdateServerCollectionItem);
  try
    LErrorMsg := '';

    LStatus := LLocalUploadController.GetVersions(FActiveVersionsList, LErrorMsg);

    // SetLEDStatus(LStatus, JvLEDConnectToServer);
    // SetLEDStatus(LStatus, JvLEDRecivingUpdateVersions);

    if not LStatus then
    begin
      SetErrorMsg(LStatus, LErrorMsg);
    end
    else
    begin
      LErrorMsg := '';

      LStatus := LLocalUploadController.GetFTPServer(FActiveUpdateFTPServer, LErrorMsg);
      // SetLEDStatus(LStatus, JvLEDRecivingFTPServer);

      if not LStatus then
      begin
        SetErrorMsg(LStatus, LErrorMsg);
      end
      else
      begin
        LErrorMsg := '';

        LStatus := LLocalUploadController.GetSystems(FActiveSystemsList, LErrorMsg);
        // SetLEDStatus(LStatus, JvLEDRecivingUpdateFiles);
      end;
    end;
  finally
    LLocalUploadController.Free;
  end;

  Result := LStatus;
end;

function TfMain.LoadLocalFilesList: Boolean;

  function ActionsToStr(AUpdateActions: TUpdateActions): string;
  begin
    with SplittString(',', SetToString(TypeInfo(TUpdateActions), AUpdateActions, False)) do
      try
        Result := Text;
      finally
        Free;
      end;
  end;

var
  LLocalUpdateController: TLocalUpdateController;

  LStatus: WordBool;
  LFileIndex: Integer;
begin
  LStatus := False;

  if Assigned(FActiveLocalFiles) then
    FActiveLocalFiles.Free;

  LLocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
  try
    LLocalUpdateController.GetLocalFiles(FActiveSystemsList, FActiveLocalFiles);
  finally
    LLocalUpdateController.Free;
  end;

  with cxGLocalFilesTableView.DataController do
  begin
    RecordCount := 0;

    BeginUpdate;
    try
      RecordCount := FActiveLocalFiles.Count;

      for LFileIndex := 0 to RecordCount - 1 do
        with FActiveLocalFiles[LFileIndex] do
        begin
          Values[LFileIndex, cxGLocalFilesTableViewColumn1.Index] := LocalFile.Status;
          if LocalFile.Status then
            LStatus := True;
          Values[LFileIndex, cxGLocalFilesTableViewColumn2.Index] := GetEnumName(TypeInfo(TUpdateCondition), Integer(LocalFile.Condition));
          Values[LFileIndex, cxGLocalFilesTableViewColumn3.Index] := ExtractRelativePath(ExtractFilePath(FActiveUpdateFileCollectionItem.LibraryFile), LocalFile.FileName);
          Values[LFileIndex, cxGLocalFilesTableViewColumn4.Index] := FileVersion.ToString;
          // Values[LFileIndex, cxGLocalFilesTableViewColumn5.Index] := GetEnumName(TypeInfo(TUpdateAction), Integer(LocalFile.Action));
          // Values[LFileIndex, cxGLocalFilesTableViewColumn6.Index] := ActionsToStr(LocalFile.Actions);
        end;
    finally
      EndUpdate;
    end;
  end;

  Result := LStatus;
end;

function TfMain.LoadUpdateFilesList: Boolean;

  function ActionsToStr(AUpdateActions: TUpdateActions): string;
  begin
    with SplittString(',', SetToString(TypeInfo(TUpdateActions), AUpdateActions, False)) do
      try
        Result := Text;
      finally
        Free;
      end;
  end;

var
  LLocalUpdateController: TLocalUpdateController;

  LFileIndex: Integer;
begin
  LLocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
  try
    LLocalUpdateController.GetPossibleActionsForLocalFiles(FActiveLocalFiles);
  finally
    LLocalUpdateController.Free;
  end;

  with cxGUpdateFilesTableView.DataController do
  begin
    RecordCount := 0;

    BeginUpdate;
    try
      RecordCount := FActiveLocalFiles.Count;

      for LFileIndex := 0 to RecordCount - 1 do
        with FActiveLocalFiles[LFileIndex] do
        begin
          Values[LFileIndex, cxGUpdateFilesTableViewColumn1.Index] := LocalFile.Status;
          Values[LFileIndex, cxGUpdateFilesTableViewColumn2.Index] := GetEnumName(TypeInfo(TUpdateCondition), Integer(LocalFile.Condition));
          Values[LFileIndex, cxGUpdateFilesTableViewColumn3.Index] := ExtractRelativePath(ExtractFilePath(FActiveUpdateFileCollectionItem.LibraryFile), LocalFile.FileName);
          Values[LFileIndex, cxGUpdateFilesTableViewColumn4.Index] := FileVersion.ToString;
          Values[LFileIndex, cxGUpdateFilesTableViewColumn5.Index] := GetEnumName(TypeInfo(TUpdateAction), Integer(LocalFile.Action));
          Values[LFileIndex, cxGUpdateFilesTableViewColumn6.Index] := ActionsToStr(LocalFile.Actions);
        end;
    finally
      EndUpdate;
    end;
  end;

  Result := True;
end;

function TfMain.UpdateServer: Boolean;

  procedure SetErrorMsg(AStatus: Boolean; AMsg: string);
  begin
    with eUpdateInfoError do
    begin
      lUpdateInfoError.Visible := not AStatus;
      Visible := not AStatus;
      case AStatus of
        True:
          Text := '';
      else
        Text := AMsg;
      end;
    end;
  end;

var
  LLocalUploadController: TLocalUploadController;
  LLocalUpdateController: TLocalUpdateController;

  LStatus: WordBool;
  LErrorMsg: WideString;

  LLocalFileIndex, LOnlineFileIndex: Integer;
  LUpdateSystemFileList: TUpdateManagerLocalFileList;
  LUpdateSystemFileBaseList: TUpdateSystemFileBaseList;

  LLocalFile: IUpdateManagerLocalSystemFile;
begin
  LStatus := False;

  LLocalUploadController := TLocalUploadController.Create(FActiveUpdateServerCollectionItem);
  try
    LErrorMsg := '';

    if (rbAddNewVersion.Checked) then
    begin
      LStatus := LLocalUploadController.AddVersion(cxSEMajorVersion.Value, cxSEMinorVersion.Value, cxSEMajorBuild.Value, cxSEMinorBuild.Value, FVersionID, LErrorMsg);
      // SetLEDStatus(LStatus, JvLEDAddVersion);
    end
    else
    begin
      FVersionID := FActiveVersionsList[lbSelectVersion.ItemIndex].ID;
      LStatus := (FVersionID > 0);
      if not LStatus then
        LErrorMsg := 'Failed to retrieve the version id in the previous step. Start over again.';
      // SetLEDStatus(LStatus, JvLEDAddVersion, True);
    end;

    if not LStatus then
    begin
      SetErrorMsg(LStatus, LErrorMsg);
    end
    else
    begin
      LErrorMsg := '';

      LUpdateSystemFileList := TUpdateManagerLocalFileList.Create;
      LUpdateSystemFileBaseList := TUpdateSystemFileBaseList.Create;
      try
        // Make list of new systems
        for LLocalFileIndex := 0 to FActiveLocalFiles.Count - 1 do
          with FActiveLocalFiles[LLocalFileIndex] do
            if (LocalFile.Action = uaAddnUpdate) then
            begin
              LUpdateSystemFileList.Add(FActiveLocalFiles[LLocalFileIndex]);
              LUpdateSystemFileBaseList.Add(FActiveLocalFiles[LLocalFileIndex].FileBase);
            end;

        if LUpdateSystemFileBaseList.Count > 0 then
        begin
          // Add new systems
          LStatus := LLocalUploadController.AddSystems(LUpdateSystemFileBaseList, LErrorMsg);

          if (LStatus) then
          begin
            LErrorMsg := '';

            if Assigned(FActiveSystemsList) then
              FActiveSystemsList.Free;

            // Retrieve ids from new systems
            LStatus := LLocalUploadController.GetSystems(FActiveSystemsList, LErrorMsg);

            if (LStatus) then
            begin
              LErrorMsg := '';

              // Combine "FActiveLocalFiles" with all SystemIDs
              LLocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
              try
                LStatus := LLocalUpdateController.MergeLocalFiles(FActiveSystemsList, LUpdateSystemFileList, LErrorMsg);
              finally
                LLocalUpdateController.Free;
              end;
            end;
          end;

          // SetLEDStatus(LStatus, JvLEDAddSystems);
        end
        else
        begin
          // SetLEDStatus(True, JvLEDAddSystems, True);
        end;

      finally
        LUpdateSystemFileBaseList.Free;
        LUpdateSystemFileList.Free;
      end;

      if not LStatus then
      begin
        SetErrorMsg(LStatus, LErrorMsg);
      end
      else
      begin
        LErrorMsg := '';

        // Clean up ignored files from local files
        for LLocalFileIndex := FActiveLocalFiles.Count - 1 downto 0 do
          with FActiveLocalFiles[LLocalFileIndex] do
            if (LocalFile.Action = uaIgnoreThisUpdate) then
            begin
              FActiveLocalFiles.Delete(LLocalFileIndex);
            end;

        if Assigned(FActiveOnlineFiles) then
          FActiveOnlineFiles.Free;

        // Retrieve already updated files
        LStatus := LLocalUploadController.GetFilesToVersion(FVersionID, FActiveOnlineFiles, LErrorMsg);

        if LStatus and (FActiveOnlineFiles.Count > 0) then
        begin

          for LOnlineFileIndex := 0 to FActiveOnlineFiles.Count - 1 do
          begin
            with FActiveOnlineFiles[LOnlineFileIndex] do
              for LLocalFileIndex := FActiveLocalFiles.Count - 1 downto 0 do
              begin
                LLocalFile := FActiveLocalFiles[LLocalFileIndex];

                if FileBase.Equals(LLocalFile.FileBase) then
                begin
                  if SameStr(FileChecksum, LLocalFile.FileChecksum) then
                    FActiveLocalFiles.Delete(LLocalFileIndex);

                  break;
                end;
              end;
            LLocalFile := nil;
          end;
        end
        else
        begin
          // SetLEDStatus(True, JvLEDRetrieveFiles, True);
        end;

        // SetLEDStatus(LStatus, JvLEDRetrieveFiles);

        if not LStatus then
        begin
          SetErrorMsg(LStatus, LErrorMsg);
        end;
      end;
    end;
  finally
    LLocalUploadController.Free;
  end;

  if (FActiveLocalFiles.Count = 0) then
  begin
    ShowMessage('All files are already online. No need to upload files.');
    Close;
  end;

  Result := LStatus;
end;

function TfMain.UploadFiles: Boolean;

  procedure SetErrorMsg(AStatus: Boolean; AMsg: string);
  begin
    with eUploadInfoError do
    begin
      lUploadInfoError.Visible := not AStatus;
      Visible := not AStatus;
      case AStatus of
        True:
          Text := '';
      else
        Text := AMsg;
      end;
    end;
  end;

var
  LLocalUploadController: TLocalUploadController;
  LLocalUpdateController: TLocalUpdateController;

  LStatus: WordBool;
  LErrorMsg: WideString;

  LLocalFileIndex: Integer;

  LUploadFiles: TUpdateManagerLocalFileList;
begin
  LStatus := False;

  LLocalUploadController := TLocalUploadController.Create(FActiveUpdateServerCollectionItem);
  try
    LErrorMsg := '';

    LUploadFiles := TUpdateManagerLocalFileList.Create;
    try
      // Make list of upload files
      for LLocalFileIndex := 0 to FActiveLocalFiles.Count - 1 do
        with FActiveLocalFiles[LLocalFileIndex] do
          if (LocalFile.Action in [uaAddnUpdate, uaEditnUpdate { , uaDelete } ]) then
          begin
            LUploadFiles.Add(FActiveLocalFiles[LLocalFileIndex]);
          end;

      // Compress all selected files
      LLocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
      try
        LLocalUpdateController.CompressLocalFiles(LUploadFiles, FStoreUpdateFilesPath);
        LStatus := True;
      finally
        LLocalUpdateController.Free;
      end;
      // SetLEDStatus(LStatus, JvLEDCompressLocalFiles);

      LErrorMsg := '';
      // Add all selected files into the DB.
      LStatus := LLocalUploadController.AddFiles(FVersionID, LUploadFiles, LErrorMsg);

      if not LStatus then
      begin
        SetErrorMsg(LStatus, LErrorMsg);
      end
      else
      begin
        LErrorMsg := '';

        pbUploadProgress.Visible := True;

        // Upload all selected files onto server.
        LLocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
        try
          LLocalUpdateController.OnUpdateUploading :=
          { } procedure(Sender: TObject; Position: Integer)
          { } begin
            { . } pbUploadProgress.Position := Position * 100;
            { } end;

          LStatus := LLocalUpdateController.UploadLocalFiles(LUploadFiles, FActiveUpdateFTPServer, FStoreUpdateFilesPath);
        finally
          LLocalUpdateController.Free;
        end;

        // SetLEDStatus(LStatus, JvLEDUploadLocalFiles);
      end;

    finally
      LUploadFiles.Free;
    end;
  finally
    LLocalUploadController.Free;
  end;

  Result := LStatus;
end;

function TfMain.ActivateVersion: Boolean;
var
  LLocalUploadController: TLocalUploadController;

  LStatus: WordBool;
  LErrorMsg: WideString;
begin
  LStatus := False;

  LLocalUploadController := TLocalUploadController.Create(FActiveUpdateServerCollectionItem);
  try
    LErrorMsg := '';

    LStatus := LLocalUploadController.ActivateVersion(FVersionID, LErrorMsg);
  finally
    LLocalUploadController.Free;
  end;

  Result := LStatus;
end;

procedure TfMain.bShowHTTPLoggerClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  HTTPLogger.Show;
{$ENDIF}
end;

end.
