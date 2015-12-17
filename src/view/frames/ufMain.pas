unit ufMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs, Menus,
  ExtCtrls, StdCtrls, DateUtils,
  // Dev Express
  cxGraphics, cxControls, cxContainer, cxEdit, cxProgressBar, dxStatusBar, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridTableView, cxPC, cxHint, cxCustomData, cxButtonEdit, cxPCdxBarPopupMenu, dxBar, dxBarBuiltInMenu,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent, Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiMain, uApiMultiCastEvent, uApiBackupManager, uApiControlAligner, uApiPlugins, uApiPublishController,
  uApiPublishManager, uApiCrawlerManager, uApiCrypterManager, uApiFileHosterManager, uApiImageHosterManager,
  uApiTabSheetController, uApiXml,
  // Utils
  uFileUtils;

type
  TfMain = class(TFrame, IPageController)
    tResize: TTimer;
    pcMain: TcxPageControl;
    dxStatusBar: TdxStatusBar;
    dxStatusBarContainer3: TdxStatusBarContainerControl;
    cxPBAutocompletion: TcxProgressBar;
    dxStatusBarContainer6: TdxStatusBarContainerControl;
    cxTCView: TcxTabControl;
    procedure FrameResize(Sender: TObject);
    procedure tResizeTimer(Sender: TObject);
    procedure pcMainCanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainGetTabHint(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
    procedure pcMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcMainNewTabButtonClick(Sender: TObject; var AHandled: Boolean);
    procedure cxTCViewChange(Sender: TObject);
  private
    FLockCount: Integer;
    FBackupManager: TBackupManager;
    FControlAligner: TControlAligner;
    FPublishManager: IPublishManager;
    FCrawlerManager: ICrawlerManager;
    FCrypterManager: ICrypterManager;
    FFileHosterManager: IFileHosterManager;
    FImageHosterManager: IImageHosterManager;
    FChange: INotifyEvent;
    FViewChange: IViewChangeEvent;
    function LockPageControl: Boolean;
    function UnlockPageControl: Boolean;
    procedure CrawlerGUIInteraction(const AControlController: IControlController; AStatus: TCrawlerTaskStatus; AProgressPosition: Extended; AMessage: string);
    function GetPagesAvailable: Boolean;
    procedure SetPagesAvailable(APagesAvailable: Boolean);
    function GetActiveViewType: TTabViewType;
    procedure SetActiveViewType(AViewType: TTabViewType);
    procedure CommonActiveViewTypeChange(AViewType: TTabViewType);
    function GetPublishManager: IPublishManager;
    function GetCrawlerManager: ICrawlerManager;
    function GetCrypterManager: ICrypterManager;
    function GetFileHosterManager: IFileHosterManager;
    function GetImageHosterManager: IImageHosterManager;
    function GetActiveTabSheetIndex: Integer;
    function GetActiveTabSheetController: ITabSheetController;
    function GetTabSheetController(index: Integer): ITabSheetController;
    function GetChange: INotifyEvent;
    function GetViewChange: IViewChangeEvent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PostCreate; // called after all frames are created

    procedure CallBackupManager;
    procedure CallControlAligner;
    procedure CallPublish(ATabIndex: Integer); overload;
    procedure CallPublish; overload;
    procedure CallSeriesPublish;
    procedure CallAutoCompletion;
    procedure CallSeriesAutoCompletion;
    procedure CallCrypterCrypt(ATabIndex: Integer); overload;
    procedure CallCrypterCrypt; overload;
    procedure CallSeriesCrypterCrypt;
    procedure CallCrypterCheck(ATabIndex: Integer); overload;
    procedure CallCrypterCheck; overload;
    procedure CallSeriesCrypterCheck;

    procedure SwitchDesignView(AEnabled: Boolean);

    property PagesAvailable: Boolean read GetPagesAvailable write SetPagesAvailable;
    property ActiveViewType: TTabViewType read GetActiveViewType write SetActiveViewType;

    function Add(AFileName: WideString): Integer; overload;
    function Add(AFileName: WideString; ATypeID: TTypeID; AEmpty: WordBool = False): Integer; overload;
    procedure SaveTab(ATabIndex: Integer; ASaveDialog: Boolean = False);
    procedure SaveCurrentTab;
    procedure SaveCurrentTabAs;
    procedure SaveAllTabs;
    procedure SaveAllToFolder;
    procedure OpenToNewTab(AFileName: WideString = '');
    function CanClose(ATabIndex: Integer): Boolean;
    function CanCloseCurrentTab: Boolean;
    function CanCloseAllTabs: Boolean;
    function RemoveTab(ATabIndex: Integer): Boolean;
    function RemoveCurrentTab: Boolean;
    function RemoveAllOtherTabs: Boolean;
    function RemoveAllTabs: Boolean;

    property PublishManager: IPublishManager read GetPublishManager;
    property CrawlerManager: ICrawlerManager read GetCrawlerManager;
    property CrypterManager: ICrypterManager read GetCrypterManager;
    property FileHosterManager: IFileHosterManager read GetFileHosterManager;
    property ImageHosterManager: IImageHosterManager read GetImageHosterManager;
    property ActiveTabSheetIndex: Integer read GetActiveTabSheetIndex;
    property ActiveTabSheetController: ITabSheetController read GetActiveTabSheetController;
    property TabSheetController[index: Integer]: ITabSheetController read GetTabSheetController;
    function TabSheetCount: Integer;

    property OnChange: INotifyEvent read GetChange;
    property OnViewChange: IViewChangeEvent read GetViewChange;

    destructor Destroy; override;
  end;

implementation

uses
  uMain, uSelectFolderDialog,
  // Api
  uApiSettings;

type
  TcxPageControlAccess = class(TcxPageControl)

  end;
{$R *.dfm}

procedure TfMain.FrameResize(Sender: TObject);
begin
  //OutputDebugString('CALL');
  //CallControlAligner;
  tResize.Enabled := True;
end;

procedure TfMain.tResizeTimer(Sender: TObject);
begin
  //OutputDebugString('CALL');
  CallControlAligner;
  tResize.Enabled := False;
end;

procedure TfMain.pcMainCanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
begin
  ACanClose := CanClose(ATabIndex);

  // TODO: If app is closed, this message will appear in a loop. Another problem exists for two tabs crawling
  // at the same time FActiveCrawlerControlController only stores active tab.

  // if CrawlingActive then
  // MessageDlg('You cannot close this tab because the crawler for this tab is still active! ', mtWarning, [mbOK], 0);
end;

procedure TfMain.pcMainChange(Sender: TObject);
begin
  PagesAvailable := (TabSheetCount > 0);
  Main.aCloseAllOther.Enabled := (TabSheetCount > 1);

  if (TabSheetCount > 0) then
  begin
    with ActiveTabSheetController do
      ActiveViewType := ViewType;
  end
  else
    SwitchDesignView(False);

  FChange.Invoke(pcMain);
end;

procedure TfMain.pcMainGetTabHint(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
begin
  AHint := TabSheetController[ATabIndex].ReleaseName;
end;

procedure TfMain.pcMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if SettingsManager.Settings.MoveWorkWhileHoldingLeftMouse and (TabSheetCount = 0) and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
  begin
    ReleaseCapture;
    SendMessage(Main.Handle, WM_SYSCOMMAND, SC_MOVE + HTCAPTION, 0);
  end
  else if mbRight = Button then
    with TcxPageControl(Sender) do
      if not(IndexOfTabAt(X, Y) = -1) then
      begin
        ActivePageIndex := IndexOfTabAt(X, Y);
        TcxPageControlAccess(Sender).RequestLayout;

        Main.dxBpmPageControlRightClick.Popup(ClientToScreen(Point(X, Y)).X, ClientToScreen(Point(X, Y)).Y);
      end;
end;

procedure TfMain.pcMainNewTabButtonClick(Sender: TObject; var AHandled: Boolean);
begin
  //
end;

procedure TfMain.cxTCViewChange(Sender: TObject);
begin
  ActiveTabSheetController.ViewType := ActiveViewType;

  CommonActiveViewTypeChange(ActiveViewType);
end;

function TfMain.LockPageControl: Boolean;
begin
  if (FLockCount = 0) then
  begin
    SendMessage(pcMain.Handle, WM_SETREDRAW, WPARAM(False), 0);
  end;

  Inc(FLockCount);
end;

function TfMain.UnlockPageControl: Boolean;
begin
  Dec(FLockCount);

  if (FLockCount = 0) then
  begin
    SendMessage(pcMain.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(pcMain.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TfMain.CrawlerGUIInteraction(const AControlController: IControlController; AStatus: TCrawlerTaskStatus; AProgressPosition: Extended; AMessage: string);
begin
  with cxPBAutocompletion do
  begin
    Repaint;
    case AStatus of
      ctsCREATED:
        Hint := 'Autocompletion (starting)';
      ctsWORKING:
        Hint := 'Active crawler: ' + AMessage;
      ctsFINISHED:
        Hint := 'Autocompletion (idle)';
    end;
    Position := AProgressPosition;
    Repaint;
  end;
end;

constructor TfMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLockCount := 0;
end;

function TfMain.GetPagesAvailable;
begin
  Result := not(TabSheetCount = 0);
end;

procedure TfMain.SetPagesAvailable(APagesAvailable: Boolean);
begin
  with Main do
  begin
    aClose.Enabled := APagesAvailable;
    aCloseAll.Enabled := APagesAvailable;

    aSave.Enabled := APagesAvailable;
    aSaveAs.Enabled := APagesAvailable;
    aSaveAll.Enabled := APagesAvailable;
    aSaveAllToFolder.Enabled := APagesAvailable;

    aAutoCompletion.Enabled := APagesAvailable;
    aCrypterCrypt.Enabled := APagesAvailable;
    aCrypterCheck.Enabled := APagesAvailable;
    aQuickBackup.Enabled := APagesAvailable;
    aPublish.Enabled := APagesAvailable;

    aPublishItemVisit.Enabled := APagesAvailable;
    aPublishItemPreview.Enabled := APagesAvailable;
    aPublishItemPublish.Enabled := APagesAvailable;
    aPublishItemSettings.Enabled := APagesAvailable;

    aSeriesAutoCompletion.Enabled := APagesAvailable;
    aSeriesCrypterCrypt.Enabled := APagesAvailable;
    aSeriesCrypterCheck.Enabled := APagesAvailable;
    aSeriesPublish.Enabled := APagesAvailable;

    cxTCView.Enabled := APagesAvailable;
  end;
end;

function TfMain.GetActiveViewType: TTabViewType;
begin
  Result := TTabViewType(cxTCView.TabIndex + 1);
end;

procedure TfMain.SetActiveViewType(AViewType: TTabViewType);
begin
  cxTCView.TabIndex := Integer(AViewType) - 1;

  CommonActiveViewTypeChange(AViewType);
end;

procedure TfMain.CommonActiveViewTypeChange(AViewType: TTabViewType);
begin
  if AViewType = vtData then
    CallControlAligner;

  FViewChange.Invoke(AViewType);
end;

function TfMain.GetPublishManager: IPublishManager;
begin
  Result := FPublishManager;
end;

function TfMain.GetCrawlerManager: ICrawlerManager;
begin
  Result := FCrawlerManager;
end;

function TfMain.GetCrypterManager;
begin
  Result := FCrypterManager;
end;

function TfMain.GetFileHosterManager;
begin
  Result := FFileHosterManager;
end;

function TfMain.GetImageHosterManager;
begin
  Result := FImageHosterManager;
end;

function TfMain.GetActiveTabSheetIndex;
begin
  Result := pcMain.ActivePageIndex;
end;

function TfMain.GetActiveTabSheetController;
begin
  Result := TabSheetController[ActiveTabSheetIndex];
end;

function TfMain.GetTabSheetController(index: Integer): ITabSheetController;
begin
  with pcMain do
  begin
    Properties.BeginUpdate;
    try
      Result := TTabSheetController(Pages[index]);
    finally
      Properties.CancelUpdate;
    end;
  end;
end;

function TfMain.GetChange: INotifyEvent;
begin
  Result := FChange;
end;

function TfMain.GetViewChange: IViewChangeEvent;
begin
  Result := FViewChange;
end;

procedure TfMain.PostCreate;
var
  LPublishManager: TPublishManager;
  LCrawlerManager: TCrawlerManager;
begin
  FBackupManager := TBackupManager.Create;
  FControlAligner := TControlAligner.Create;

  LPublishManager := TPublishManager.Create;
  with LPublishManager do
  begin
    OnGUIInteraction := Main.fPublishQueue.GUIInteractionEvent;
    OnGUIInteractionItem := Main.fPublishQueue.GUIInteractionItemEvent;
  end;
  FPublishManager := LPublishManager;

  LCrawlerManager := TCrawlerManager.Create;
  LCrawlerManager.OnGUIInteraction := CrawlerGUIInteraction;
  FCrawlerManager := LCrawlerManager;

  FCrypterManager := TCrypterManager.Create;

  FFileHosterManager := TFileHosterManager.Create;
  FImageHosterManager := TImageHosterManager.Create;

  FChange := TINotifyEvent.Create;
  FViewChange := TIViewChangeEvent.Create;
end;

procedure TfMain.CallBackupManager;
begin
  if (TabSheetCount > 0) then
    FBackupManager.Backup(ActiveTabSheetController);
end;

procedure TfMain.CallControlAligner;
begin
  if (TabSheetCount > 0) then
    with FControlAligner do
    begin
      LockPageControl;
      try
        TTabSheetController(pcMain.ActivePage).DataTabSheetItem.VertScrollBar.Position := 0;

        WorkPanelWidth := pcMain.ActivePage.Width;

        ControlController := ActiveTabSheetController.ControlController;
        MirrorController := ActiveTabSheetController.MirrorController;
        Start;
      finally
        UnlockPageControl;
      end;
    end;
end;

procedure TfMain.CallPublish(ATabIndex: Integer);
begin
  if (TabSheetCount > 0) then
    PublishManager.AddPublishJob(TabSheetController[ATabIndex].PublishController.GeneratePublishJob);
end;

procedure TfMain.CallPublish;
begin
  CallPublish(ActiveTabSheetIndex);
end;

procedure TfMain.CallSeriesPublish;
var
  I: Integer;

  PublishJob: TIPublishJob;
begin
  PublishJob := TIPublishJob.Create('Everything active');

  for I := 0 to TabSheetCount - 1 do
    PublishJob.Add(TabSheetController[I].PublishController.GeneratePublishTab);

  PublishManager.AddPublishJob(PublishJob);
end;

procedure TfMain.CallAutoCompletion;
begin
  CallBackupManager;

  CrawlerManager.AddCrawlerJob(ActiveTabSheetController.ControlController);
end;

procedure TfMain.CallSeriesAutoCompletion;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CrawlerManager.AddCrawlerJob(TabSheetController[I].ControlController);
end;

procedure TfMain.CallCrypterCrypt(ATabIndex: Integer);
var
  I, J: Integer;
begin
  for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
    for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
      with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
        if SameStr('', Crypter[J].Value) then
          CrypterManager.AddCrypterJob(Crypter[J]);
end;

procedure TfMain.CallCrypterCrypt;
begin
  CallCrypterCrypt(ActiveTabSheetIndex);
end;

procedure TfMain.CallSeriesCrypterCrypt;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallCrypterCrypt(I);
end;

procedure TfMain.CallCrypterCheck(ATabIndex: Integer);
var
  I, J: Integer;
begin
  for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
    for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
      with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
        if not SameStr('', Crypter[J].Value) then
          CrypterManager.AddCrypterCheckJob(Crypter[J]);
end;

procedure TfMain.CallCrypterCheck;
begin
  CallCrypterCheck(ActiveTabSheetIndex);
end;

procedure TfMain.CallSeriesCrypterCheck;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallCrypterCheck(I);
end;

procedure TfMain.SwitchDesignView(AEnabled: Boolean);
begin
  with Main do
  begin
    aBold.Enabled := AEnabled;
    aItalic.Enabled := AEnabled;
    aUnderline.Enabled := AEnabled;
    aStrikeout.Enabled := AEnabled;
    aSize.Enabled := AEnabled;
    aColor.Enabled := AEnabled;
    aLeft.Enabled := AEnabled;
    aCenter.Enabled := AEnabled;
    aRight.Enabled := AEnabled;
    aImage.Enabled := AEnabled;
    aList.Enabled := AEnabled;
    aEMail.Enabled := AEnabled;
    aYoutube.Enabled := AEnabled;
    aURL.Enabled := AEnabled;
    aQuote.Enabled := AEnabled;
    aCode.Enabled := AEnabled;
    aSpoiler.Enabled := AEnabled;
    aHide.Enabled := AEnabled;
  end;
end;

function TfMain.Add(AFileName: WideString): Integer;
begin
  Result := Add(GetTemplatesTypeFolder + AFileName + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + AFileName + '.xml').TemplateType);
end;

function TfMain.Add(AFileName: WideString; ATypeID: TTypeID; AEmpty: WordBool = False): Integer;
var
  LNewTabSheetController: TTabSheetController;
  LMirrorIndex: Integer;
begin
  LockPageControl;
  try
    pcMain.Properties.BeginUpdate;
    try
      LNewTabSheetController := TTabSheetController.Create(pcMain, Self, ATypeID);
      with LNewTabSheetController do
      begin
        ParentColor := False;

        PageControl := pcMain;

        Color := clWhite;

        // FileName := AFileName;
        ImageIndex := Integer(ATypeID);

        TemplateFileName := ExtractFileName(ChangeFileExt(AFileName, ''));

        Install;
      end;
    finally
      pcMain.Properties.CancelUpdate; // prevent to call OnChanged
    end;

    pcMain.ActivePage := LNewTabSheetController;

    if SettingsManager.Settings.ControlAligner.MirrorPosition = mpTop then
    begin
      if not AEmpty then
      begin
        for LMirrorIndex := 0 to SettingsManager.Settings.ControlAligner.MirrorCount - 1 do
          LNewTabSheetController.MirrorController.Mirror[LNewTabSheetController.MirrorController.Add].GetDirectlink.Add('');
      end;

      GetControls(AFileName, LNewTabSheetController.ControlController, Self);
    end
    else
    begin
      GetControls(AFileName, LNewTabSheetController.ControlController, Self);

      if not AEmpty then
      begin
        for LMirrorIndex := 0 to SettingsManager.Settings.ControlAligner.MirrorCount - 1 do
          LNewTabSheetController.MirrorController.Mirror[LNewTabSheetController.MirrorController.Add].GetDirectlink.Add('');
      end;
    end;
  finally
    UnlockPageControl;
  end;

  with LNewTabSheetController do
  begin
    DataChanged := False;
    ResetControlFocused();

    with PublishController do
    begin
      OnUpdateCMSList.Add(Main.fPublish.GetUpdateCMSListEvent);
      OnUpdateCMSWebsiteList.Add(Main.fPublish.GetUpdateCMSWebsiteListEvent);
      OnUpdateCMSWebsite.Add(Main.fPublish.GetUpdateCMSWebsiteEvent);

      if not AEmpty then
      begin
        Active := True;
        pcMain.OnChange(pcMain);
      end;
    end;
  end;

  Result := ActiveTabSheetIndex;
end;

procedure TfMain.SaveTab(ATabIndex: Integer; ASaveDialog: Boolean = False);

  function GetFileName: string;
  var
    Control: IControlBasic;
  begin
    with TabSheetController[ATabIndex] do
      if length(FileName) > 0 then
        Result := FileName
      else if length(ReleaseName) > 0 then
        Result := TrueFilename(ReleaseName)
      else
      begin
        Control := ControlController.FindControl(cTitle);
        if Assigned(Control) and (length(Control.Value) > 0) then
          Result := TrueFilename(Control.Value);
      end;
  end;

var
  PluginsAvailable: Boolean;
  FileFormats: TStrings;
  FileFormatsIndex: Integer;
  FileFilter: string;
begin
  FileFilter := '';

  (* **
    SaveDialog => Visible, if ASaveDialog = True or File exists
    ASaveDialog = False, FileExists     FALSE
    ASaveDialog = False, FileNotExists  TRUE
    ASaveDialog = True,  FileExists     TRUE
    ASaveDialog = True,  FileNotExists  TRUE
    ** *)
  ASaveDialog := ASaveDialog or not FileExists(TabSheetController[ATabIndex].FileName);

  FileFormats := TPluginBasic.GetSaveFileFormats;
  try
    PluginsAvailable := FileFormats.Count > 0;

    for FileFormatsIndex := 0 to FileFormats.Count - 1 do
      FileFilter := FileFilter + FileFormats.ValueFromIndex[FileFormatsIndex];

    if PluginsAvailable then
      if not ASaveDialog then
      begin
        with TabSheetController[ATabIndex] do
          Save(FileName, FileType);
      end
      else
        with TSaveTextFileDialog.Create(nil) do
          try
            EncodingIndex := 4;

            FileName := GetFileName;

            Filter := FileFilter;

            if Execute then
            begin
              if not(ExtractFileExt(FileName) = '.xml') then
                FileName := FileName + '.xml';

              TabSheetController[ATabIndex].Save(FileName, FileFormats.Names[FilterIndex - 1]);
            end;
          finally
            Free;
          end;
  finally
    FileFormats.Free;
  end;
end;

procedure TfMain.SaveCurrentTab;
begin
  SaveTab(ActiveTabSheetIndex);
end;

procedure TfMain.SaveCurrentTabAs;
begin
  SaveTab(ActiveTabSheetIndex, True);
end;

procedure TfMain.SaveAllTabs;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    SaveTab(I);
end;

procedure TfMain.SaveAllToFolder;
var
  TabSheetIndex: Integer;
  PluginAvailable: Boolean;
  FileFormats: TStrings;
  FileFormatsIndex: Integer;
  FileFilter, AutoFileName: string;
begin
  FileFilter := '';

  FileFormats := TPluginBasic.GetSaveFileFormats;
  try
    PluginAvailable := FileFormats.Count > 0;

    for FileFormatsIndex := 0 to FileFormats.Count - 1 do
      FileFilter := FileFilter + FileFormats.ValueFromIndex[FileFormatsIndex];

    with TSelectFolderDialog.Create(nil) do
      try
        Filter := FileFilter;

        if Execute and PluginAvailable then
          for TabSheetIndex := 0 to TabSheetCount - 1 do
            with TabSheetController[TabSheetIndex] do
            begin
              if not(ReleaseName = '') then
                AutoFileName := TrueFilename(ReleaseName)
              else if not(ControlController.FindControl(cTitle).Value = '') then
                AutoFileName := TrueFilename(ControlController.FindControl(cTitle).Value);
              AutoFileName := Path + AutoFileName;
              if not(ExtractFileExt(AutoFileName) = '.xml') then
                AutoFileName := AutoFileName + '.xml';
              if (FileExists(AutoFileName) and (MessageDlg('Override "' + ExtractFileName(AutoFileName) + '" ?', mtConfirmation, [mbyes, mbno], 0) = mrYes)) or (not FileExists(AutoFileName)) then
                Save(AutoFileName, FileFormats.Names[FilterIndex - 1]);
            end;
      finally
        Free;
      end;
  finally
    FileFormats.Free;
  end;
end;

procedure TfMain.OpenToNewTab;
var
  PluginAvailable: Boolean;
  FileFormats: TStrings;
  I, FileFormatsIndex: Integer;
  FileFilter: string;
begin

  try
    if not FileExists(AFileName) then
    begin
      FileFormats := TPluginBasic.GetLoadFileFormats;
      try
        PluginAvailable := FileFormats.Count > 0;

        for FileFormatsIndex := 0 to FileFormats.Count - 1 do
          FileFilter := FileFilter + FileFormats.ValueFromIndex[FileFormatsIndex];
      finally
        FileFormats.Free;
      end;
      with TOpenDialog.Create(nil) do
        try
          Options := Options + [ofAllowMultiSelect];
          Filter := FileFilter;

          if PluginAvailable and Execute then
            for I := 0 to Files.Count - 1 do
              TPluginBasic.LoadFile(Files.Strings[I], Self);
        finally
          Free;
        end;
    end
    else
      TPluginBasic.LoadFile(AFileName, Self);

    pcMain.OnChange(pcMain);
  except

  end;
end;

function TfMain.CanClose(ATabIndex: Integer): Boolean;
var
  LTabSheetController: ITabSheetController;
begin
  LTabSheetController := TabSheetController[ATabIndex];

  Result := not CrawlerManager.InUse(LTabSheetController) and { }
  { } not CrypterManager.InUse(LTabSheetController) and { }
  { } not FileHosterManager.InUse(LTabSheetController) and { }
  { } not ImageHosterManager.InUse(LTabSheetController);

  LTabSheetController := nil;
end;

function TfMain.CanCloseCurrentTab: Boolean;
begin
  Result := CanClose(ActiveTabSheetIndex);
end;

function TfMain.CanCloseAllTabs: Boolean;
var
  LTabSheetIndex: Integer;
  LResult: Boolean;
begin
  LResult := True;

  for LTabSheetIndex := 0 to TabSheetCount - 1 do
  begin
    LResult := CanClose(LTabSheetIndex);
    if not LResult then
      break;
  end;

  Result := LResult;
end;

function TfMain.RemoveTab(ATabIndex: Integer): Boolean;
begin
  Result := True;

  with pcMain do
  begin
    Properties.BeginUpdate;
    try
      try
        CloseTab(ATabIndex);
      except
        Result := False;
      end;
    finally
      Properties.EndUpdate;
    end;
    OnChange(pcMain);
  end;
end;

function TfMain.RemoveCurrentTab: Boolean;
begin
  Result := RemoveTab(ActiveTabSheetIndex);
end;

function TfMain.RemoveAllOtherTabs;
var
  I: Integer;
begin
  // remove all tabs on the right side
  for I := TabSheetCount - 1 downto ActiveTabSheetIndex + 1 do
    RemoveTab(I);
  // remove all tabs on the left side
  for I := TabSheetCount - 2 downto 0 do
    RemoveTab(I);

  Result := (TabSheetCount = 1);
end;

function TfMain.RemoveAllTabs: Boolean;
begin
  while TabSheetCount > 0 do
    RemoveCurrentTab;

  Result := (TabSheetCount = 0);
end;

function TfMain.TabSheetCount: Integer;
begin
  Result := pcMain.PageCount;
end;

destructor TfMain.Destroy;
begin
  FViewChange := nil;
  FChange := nil;

  FControlAligner.Free;
  FBackupManager.Free;

  inherited Destroy;
end;

end.
