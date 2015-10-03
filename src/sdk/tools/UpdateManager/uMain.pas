unit uMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, TypInfo, Generics.Collections,
  // JEDI VCL
  JvWizard, JvWizardRouteMapNodes, JvExControls, JvLED,
  // DevExpress
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxNavigator, cxCheckBox, cxLabel, cxTextEdit, cxDropDownEdit, cxBlobEdit, cxGridCustomTableView, cxGridTableView, cxGridCustomView,
  cxClasses, cxGridLevel, cxGrid, cxMemo,
  //
  IntelligeNFileSystem,
  // Api
  uApiUpdateConst, uApiUpdateInterface, uApiUpdateSettings, uApiUpdateController,
  // Utils
  uFileUtils, uPathUtils, uSetUtils, uStringUtils;

type
  TfMain = class(TForm)
    JvWizard: TJvWizard;
    JvWizardWelcomePage: TJvWizardWelcomePage;
    JvWizardInteriorPageFiles: TJvWizardInteriorPage;
    JvWizardRouteMapNodes: TJvWizardRouteMapNodes;
    rbAddNewPath: TRadioButton;
    eRootDir: TEdit;
    sbSelectRootDir: TSpeedButton;
    rbSelectExisting: TRadioButton;
    lbSelectPath: TListBox;
    cxGFiles: TcxGrid;
    cxGFilesLevel: TcxGridLevel;
    cxGFilesTableView: TcxGridTableView;
    cxGFilesTableViewColumn1: TcxGridColumn;
    cxGFilesTableViewColumn2: TcxGridColumn;
    cxGFilesTableViewColumn3: TcxGridColumn;
    cxGFilesTableViewColumn4: TcxGridColumn;
    cxGFilesTableViewColumn5: TcxGridColumn;
    cxGFilesTableViewColumn6: TcxGridColumn;
    lFileSystem: TLabel;
    JvWizardInteriorPageServer: TJvWizardInteriorPage;
    JvWizardInteriorPagePublish: TJvWizardInteriorPage;
    JvWizardInteriorPageServerInfo: TJvWizardInteriorPage;
    rbAddNewServer: TRadioButton;
    eServerDir: TEdit;
    rbSelectExistingServer: TRadioButton;
    lbSelectServer: TListBox;
    eServerAccessToken: TEdit;
    lServerAccessToken: TLabel;
    JvLEDConnectToServer: TJvLED;
    lConnectToServer: TLabel;
    JvLEDRecivingUpdateVersions: TJvLED;
    lRecivingUpdateVersions: TLabel;
    JvLEDRecivingFTPServer: TJvLED;
    lRecivingFTPServer: TLabel;
    JvLEDRecivingUpdateFiles: TJvLED;
    lRecivingUpdateFiles: TLabel;
    JvWizardInteriorPageUpdateFiles: TJvWizardInteriorPage;
    lServerInfoError: TLabel;
    eServerInfoError: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvWizardCancelButtonClick(Sender: TObject);
    { *************************************** STEP - 1 *************************************** }
    procedure JvWizardWelcomePagePage(Sender: TObject);
    procedure JvWizardWelcomePageNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure rbSelectFileSystem(Sender: TObject);
    procedure eRootDirChange(Sender: TObject);
    procedure sbSelectRootDirClick(Sender: TObject);
    procedure lbSelectPathClick(Sender: TObject);
    { *************************************** STEP - 2 *************************************** }
    procedure JvWizardInteriorPageServerPage(Sender: TObject);
    procedure JvWizardInteriorPageServerNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure rbSelectServer(Sender: TObject);
    procedure eServerDirChange(Sender: TObject);
    procedure lbSelectServerClick(Sender: TObject);
    { *************************************** STEP - 3 *************************************** }
    procedure JvWizardInteriorPageServerInfoPage(Sender: TObject);
    { *************************************** STEP - 4 *************************************** }
    procedure JvWizardInteriorPageFilesExitPage(Sender: TObject; const FromPage: TJvWizardCustomPage);
    procedure JvWizardInteriorPageFilesPage(Sender: TObject);
    procedure JvWizardInteriorPageFilesNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure cxGFilesTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure cxGFilesTableViewColumn5GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure cxGFilesTableViewDataControllerDataChanged(Sender: TObject);
    { *************************************** STEP - 5 *************************************** }
    procedure JvWizardInteriorPageUpdateFilesPage(Sender: TObject);

  private
  var
    FActiveUpdateFileCollectionItem: TUpdateFileSystemCollectionItem;
    FActiveUpdateServerCollectionItem: TUpdateServerCollectionItem;
    FActiveUpdateFTPServer: IFTPServer;
    // FActiveUpdateFiles: TUpdateLocalFileList;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure CanContinue(AValue: Boolean; APage: TJvWizardCustomPage);
    procedure CheckCanContinueToServer;
    procedure CheckCanContinueToServerInfo;
    procedure CheckCanContinueToUpdateFiles;

  protected
    function LoadServerInfos: Boolean;
    // procedure LoadFilesList;
    procedure SaveFilesList;
    procedure MakeUpdate;
  public

  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  cxGFilesTableView.DataController.OnDataChanged := nil;
  LoadSettings;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfMain.JvWizardCancelButtonClick(Sender: TObject);
begin
  Close;
end;

{ *************************************** STEP - 1 *************************************** }

procedure TfMain.JvWizardWelcomePagePage(Sender: TObject);
begin
  lbSelectPath.Items.Text := SettingsManager.Settings.GetLibraryFiles;
  CheckCanContinueToServer;
end;

procedure TfMain.JvWizardWelcomePageNextButtonClick(Sender: TObject; var Stop: Boolean);
begin
  if rbAddNewPath.Checked then
  begin
    FActiveUpdateFileCollectionItem := TUpdateFileSystemCollectionItem(SettingsManager.Settings.FileSystems.Add);
    FActiveUpdateFileCollectionItem.LibraryFile := eRootDir.Text;
  end
  else if rbSelectExisting.Checked then
    FActiveUpdateFileCollectionItem := SettingsManager.Settings.FindFileSystem(lbSelectPath.Items[lbSelectPath.ItemIndex]);
end;

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

procedure TfMain.JvWizardInteriorPageServerPage(Sender: TObject);
begin
  lbSelectServer.Items.Text := SettingsManager.Settings.GetUpdateServers;
  CheckCanContinueToServerInfo;
end;

procedure TfMain.JvWizardInteriorPageServerNextButtonClick(Sender: TObject; var Stop: Boolean);
begin
  if rbAddNewServer.Checked then
  begin
    FActiveUpdateServerCollectionItem := TUpdateServerCollectionItem(SettingsManager.Settings.UpdateServers.Add);
    FActiveUpdateServerCollectionItem.Name := eServerDir.Text;
    FActiveUpdateServerCollectionItem.AccessToken := eServerAccessToken.Text;
  end
  else if rbSelectExistingServer.Checked then
    FActiveUpdateServerCollectionItem := SettingsManager.Settings.FindServer(lbSelectServer.Items[lbSelectServer.ItemIndex]);
end;

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

procedure TfMain.JvWizardInteriorPageServerInfoPage(Sender: TObject);
var
  LCanContinue: Boolean;
begin
  LCanContinue := LoadServerInfos;
  CanContinue(LCanContinue, JvWizardInteriorPageServerInfo);
end;

{ *************************************** STEP - 4 *************************************** }

procedure TfMain.JvWizardInteriorPageFilesExitPage(Sender: TObject; const FromPage: TJvWizardCustomPage);
begin
  cxGFilesTableView.DataController.OnDataChanged := nil;
end;

procedure TfMain.JvWizardInteriorPageFilesPage(Sender: TObject);
begin
  // TODO: Load list from server
  // LoadFilesList;
  lFileSystem.Caption := ExtractFilePath(FActiveUpdateFileCollectionItem.LibraryFile);
  cxGFilesTableView.DataController.OnDataChanged := cxGFilesTableViewDataControllerDataChanged;
end;

procedure TfMain.JvWizardInteriorPageFilesNextButtonClick(Sender: TObject; var Stop: Boolean);
var
  LFileIndex: Integer;
begin
  (*
    for LFileIndex := 0 to FActiveUpdateFiles.Count - 1 do
    with cxGFilesTableView.DataController, FActiveUpdateFiles.Items[LFileIndex] do
    begin
    Status := Values[LFileIndex, cxGFilesTableViewColumn1.Index];
    Action := TUpdateAction(GetEnumValue(TypeInfo(TUpdateAction), Values[LFileIndex, cxGFilesTableViewColumn5.Index]));
    end;
    *)
end;

procedure TfMain.cxGFilesTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
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

procedure TfMain.cxGFilesTableViewColumn5GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  TcxCustomComboBoxProperties(AProperties).Items.Text := ARecord.Values[cxGFilesTableViewColumn6.Index];
end;

procedure TfMain.cxGFilesTableViewDataControllerDataChanged(Sender: TObject);
begin
  CheckCanContinueToUpdateFiles;
end;

{ *************************************** STEP - 5 *************************************** }

procedure TfMain.JvWizardInteriorPageUpdateFilesPage(Sender: TObject);
begin

end;

{ ****************************************************************************** }

procedure TfMain.LoadSettings;
begin

end;

procedure TfMain.SaveSettings;
begin

end;

{ ****************************************************************************** }

procedure TfMain.CanContinue(AValue: Boolean; APage: TJvWizardCustomPage);
begin
  with APage do
    case AValue of
      True:
        EnabledButtons := EnabledButtons + [bkNext];
    else
      EnabledButtons := EnabledButtons - [bkNext];
    end;
end;

procedure TfMain.CheckCanContinueToServer;
begin
  FActiveUpdateFileCollectionItem := nil;
  if rbAddNewPath.Checked then
    CanContinue(FileExists(eRootDir.Text) and (Pos(INTELLIGEN_FILESYSTEM_LIB, eRootDir.Text) > 0) and (lbSelectPath.Items.IndexOf(eRootDir.Text) = -1), JvWizardWelcomePage)
  else if rbSelectExisting.Checked then
    CanContinue((lbSelectPath.ItemIndex <> -1) and FileExists(lbSelectPath.Items[lbSelectPath.ItemIndex]), JvWizardWelcomePage);
end;

procedure TfMain.CheckCanContinueToServerInfo;
begin
  FActiveUpdateServerCollectionItem := nil;
  if rbAddNewServer.Checked then
    CanContinue((length(eServerDir.Text) > 10) and (Pos('http://', eServerDir.Text) > 0) and (lbSelectServer.Items.IndexOf(eServerDir.Text) = -1), JvWizardInteriorPageServer)
  else if rbSelectExistingServer.Checked then
    CanContinue((lbSelectServer.ItemIndex <> -1), JvWizardInteriorPageServer);
end;

procedure TfMain.CheckCanContinueToUpdateFiles;
var
  LFileIndex: Integer;
  LCanContinue: Boolean;
begin
  LCanContinue := False;
  with cxGFilesTableView.DataController do
    for LFileIndex := 0 to RecordCount - 1 do
    begin
      if Values[LFileIndex, cxGFilesTableViewColumn1.Index] then
      begin
        LCanContinue := True;
        break;
      end;
    end;
  CanContinue(LCanContinue, JvWizardInteriorPageFiles);
end;

{ ****************************************************************************** }

function TfMain.LoadServerInfos: Boolean;

  procedure SetLEDStatus(AStatus: Boolean; ALED: TJvLED; AJump: Boolean = False);
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

  LVersionsList: TUpdateManagerVersionsList;
  LSystemsList: TUpdateManagerSystemsList;
begin
  LStatus := False;

  LVersionsList := nil;
  LSystemsList := nil;
  try
    LLocalUploadController := TLocalUploadController.Create(FActiveUpdateServerCollectionItem);
    try
      LErrorMsg := '';

      LStatus := LLocalUploadController.GetVersions(LVersionsList, LErrorMsg);

      SetLEDStatus(LStatus, JvLEDConnectToServer);
      SetLEDStatus(LStatus, JvLEDRecivingUpdateVersions);

      if not LStatus then
      begin
        SetErrorMsg(LStatus, LErrorMsg);
      end
      else
      begin
        LErrorMsg := '';

        LStatus := LLocalUploadController.GetFTPServer(FActiveUpdateFTPServer, LErrorMsg);
        SetLEDStatus(LStatus, JvLEDRecivingFTPServer);

        if not LStatus then
        begin
          SetErrorMsg(LStatus, LErrorMsg);
        end
        else
        begin
          LErrorMsg := '';

          LStatus := LLocalUploadController.GetSystems(LSystemsList, LErrorMsg);
          SetLEDStatus(LStatus, JvLEDRecivingUpdateFiles);
        end;
      end;
    finally
      LLocalUploadController.Free;
    end;

    // TODO: Handle LVersionsList, LSystemsList storage.
  finally
    LVersionsList.Free;
    LSystemsList.Free;
  end;

  result := LStatus;
end;

(*
  // Not neccessary

  procedure TfMain.LoadFilesList;

  function ActionsToStr(AUpdateActions: TUpdateActions): string;
  begin
  with SplittString(',', SetToString(TypeInfo(TUpdateActions), AUpdateActions, False)) do
  try
  result := Text;
  finally
  Free;
  end;
  end;

  var
  LocalUpdateController: TLocalUpdateController;
  LFiles: TList<IStatusFile>;
  LFileIndex: Integer;
  begin
  LocalUpdateController := TLocalUpdateController.Create(FActiveUpdateFileCollectionItem.LibraryFile);
  try
  LFiles := FActiveUpdateFileCollectionItem.GetFiles;
  try
  LocalUpdateController.GetLocalFiles(LFiles, FActiveUpdateFiles);
  finally
  LFiles.Free;
  end;
  finally
  LocalUpdateController.Free;
  end;

  with cxGFilesTableView.DataController do
  begin
  RecordCount := 0;

  BeginUpdate;
  try
  RecordCount := FActiveUpdateFiles.Count;

  for LFileIndex := 0 to RecordCount - 1 do
  with FActiveUpdateFiles.Items[LFileIndex] do
  begin
  Values[LFileIndex, cxGFilesTableViewColumn1.Index] := Status;
  Values[LFileIndex, cxGFilesTableViewColumn2.Index] := GetEnumName(TypeInfo(TUpdateCondition), Integer(Condition));
  Values[LFileIndex, cxGFilesTableViewColumn3.Index] := ExtractRelativePath(ExtractFilePath(FActiveUpdateFileCollectionItem.LibraryFile), FileName);
  with FileVersion do
  Values[LFileIndex, cxGFilesTableViewColumn4.Index] := FileVersionToStr(MajorVersion, MinorVersion, MajorBuild, MinorBuild);
  Values[LFileIndex, cxGFilesTableViewColumn5.Index] := GetEnumName(TypeInfo(TUpdateAction), Integer(Action));
  Values[LFileIndex, cxGFilesTableViewColumn6.Index] := ActionsToStr(Actions);
  end;
  finally
  EndUpdate;
  end;
  end;
  end;
*)

procedure TfMain.SaveFilesList;
begin

end;

procedure TfMain.MakeUpdate;
begin

end;

end.
