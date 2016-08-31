unit ufMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, StdCtrls,
  DateUtils, StrUtils,
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
  uApiConst, uApiLogManager, uApiMain, uApiMultiCastEvent, uApiBackupManager, uApiControlAligner, uApiPlugins,
  uApiPublishController, uApiPublishManager, uApiCrawlerManager, uApiCrypterManager, uApiFileHosterManager,
  uApiImageHosterManager, uApiTabSheetController, uApiXml,
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
    procedure pcMainNewTabCreate(Sender: TObject; AIndex: Integer);
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
    FTabCaptionChange: ICaptionChangeEvent;
    FAddTab, FRemoveTab: ITabSheetEvent;
    function LockPageControl: Boolean;
    function UnlockPageControl: Boolean;
    procedure CrawlerGUIInteraction(const AControlController: IControlController; AStatus: TCrawlerTaskStatus; AProgressPosition: Extended; AMessage: string);
    function GetPagesAvailable: Boolean;
    procedure SetPagesAvailable(APagesAvailable: Boolean);
    function GetActiveViewType: TTabViewType;
    procedure SetActiveViewType(AViewType: TTabViewType);
    procedure CommonActiveViewTypeChange(AViewType: TTabViewType);
  protected
    function GetPublishManager: IPublishManager;
    function GetCrawlerManager: ICrawlerManager;
    function GetCrypterManager: ICrypterManager;
    function GetFileHosterManager: IFileHosterManager;
    function GetImageHosterManager: IImageHosterManager;
    function GetActiveTabSheetIndex: Integer;
    function GetActiveTabSheetController: ITabSheetController;
    function GetTabSheetController(AIndex: Integer): ITabSheetController;
    function GetChange: INotifyEvent;
    function GetViewChange: IViewChangeEvent;
    function GetTabCaptionChange: ICaptionChangeEvent;
    function GetAddTab: ITabSheetEvent;
    function GetRemoveTab: ITabSheetEvent;
    function GetBeforePublish: IThreadEvent;
    function GetAfterPublish: IThreadEvent;
    function GetBeforeCrawling: IThreadEvent;
    function GetAfterCrawling: IThreadEvent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PostCreate; // called after all frames are created

    procedure CallBackupManager; overload;
    procedure CallBackupManager(const ATabIndex: Integer); overload;
    procedure CallControlAligner;

    procedure CallPublish; overload;
    procedure CallPublish(const ATabIndex: Integer); overload;
    procedure CallSeriesPublish;

    procedure CallCrawler; overload;
    procedure CallCrawler(const ATabIndex: Integer); overload;
    procedure CallSeriesCrawler;

    procedure CallCrypterCrypt; overload;
    procedure CallCrypterCrypt(const ATabIndex: Integer); overload;
    procedure CallSeriesCrypterCrypt;

    procedure CallCrypterCheck; overload;
    procedure CallCrypterCheck(const ATabIndex: Integer); overload;
    procedure CallSeriesCrypterCheck;

    procedure CallFileHosterCheck; overload;
    procedure CallFileHosterCheck(const ATabIndex: Integer); overload;
    procedure CallSeriesFileHosterCheck;

    procedure CallImageHosterRemoteUpload; overload;
    procedure CallImageHosterRemoteUpload(const ATabIndex: Integer); overload;
    procedure CallSeriesImageHosterRemoteUpload;

    procedure SwitchDesignView(AEnabled: Boolean);

    property PagesAvailable: Boolean read GetPagesAvailable write SetPagesAvailable;
    property ActiveViewType: TTabViewType read GetActiveViewType write SetActiveViewType;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Create a new tab with several options. This function is provided for
    ///   the file format plug-in interface.
    /// </summary>
    /// <param name="ATemplateFileName">
    ///   The template file located in the templates_type folder or either a
    ///   different file.
    /// </param>
    /// <param name="ATypeID">
    ///   The base type of the new tab.
    /// </param>
    /// <param name="AEmptyTab">
    ///   Possibility to create an empty tab without any mirrors.
    /// </param>
    /// <returns>
    ///   The tab index of the created tab.
    /// </returns>
    {$ENDREGION}
    function CreateTabSheet(const ATemplateFileName: WideString; ATypeID: TTypeID; AEmptyTab: WordBool = True): Integer;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Create a new tab from the specified template file located in the
    ///   templates_type folder.
    /// </summary>
    /// <param name="ATemplateName">
    ///   The template file located in the templates_type folder.
    /// </param>
    /// <returns>
    ///   The tab index of the created tab.
    /// </returns>
    {$ENDREGION}
    function NewTabSheet(const ATemplateName: WideString): Integer;
    //
    function InternalOpenFile(const AFileName: string): Integer;
    function InternalOpenFiles(const AFiles: TStrings): Integer;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Open a file with the aid of the internal file format plug-ins in
    ///   order to create a new tab.
    /// </summary>
    /// <param name="AFileName">
    ///   The file name of the file to open.
    /// </param>
    /// <returns>
    ///   The tab index of the created tab.
    /// </returns>
    {$ENDREGION}
    function OpenTabSheet(const AFileName: WideString = ''): Integer;
    //
    function InternalSaveFile(const ATabSheetController: ITabSheetController; const AFileName, AFileFormat: WideString): Boolean;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Save a file with the aid of a internal file format plug-in in order
    ///   to create or override a new file.
    /// </summary>
    /// <param name="ATabIndex">
    ///   The tab index of the tab to be saved.
    /// </param>
    /// <param name="AFileName">
    ///   The file name of the file to be created.
    /// </param>
    /// <param name="AFileFormat">
    ///   The file format for the new file (= name of the file formats
    ///   plug-in). <br />
    /// </param>
    /// <param name="AForceDialog">
    ///   Force to open the file save dialog.
    /// </param>
    /// <returns>
    ///   The success of the operation.
    /// </returns>
    {$ENDREGION}
    function SaveTabSheet(const ATabIndex: Integer; const AFileName: WideString = ''; const AFileFormat: WideString = ''; const AForceDialog: WordBool = False): WordBool; overload;
    function SaveTabSheet(const ATabIndex: Integer; const AForceDialog: WordBool): WordBool; overload;

    function SaveTheCurrentTabSheet: WordBool;
    function SaveTheCurrentTabSheetAs: WordBool;
    function SaveAllTabSheets: WordBool;
    function SaveAllTabSheetsToFolder(const AFilePath: WideString = ''; const AFileFormat: WideString = ''): WordBool;
    {$REGION 'Documentation'}
    /// <param name="ATabIndex">
    ///   Checks whether a tab can be closed.
    /// </param>
    /// <returns>
    ///   The success of the operation.
    /// </returns>
    {$ENDREGION}
    function CanCloseTabSheet(const ATabIndex: Integer): WordBool;

    function CanCloseTheCurrentTabSheet: WordBool;
    function CanCloseAllExceptTheCurrentTabSheet: WordBool;
    function CanCloseAllTabSheets: WordBool;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Close a tab.
    /// </summary>
    /// <param name="ATabIndex">
    ///   The tab index of the tab to be closed. <br />
    /// </param>
    /// <returns>
    ///   The success of the operation. <br />
    /// </returns>
    {$ENDREGION}
    function CloseTabSheet(const ATabIndex: Integer): WordBool;

    function CloseTheCurrentTabSheet: WordBool;
    function CloseAllExceptTheCurrentTabSheet: WordBool;
    function CloseAllTabSheets: WordBool;

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
    property OnTabCaptionChange: ICaptionChangeEvent read GetTabCaptionChange;
    property OnAddTab: ITabSheetEvent read GetAddTab;
    property OnRemoveTab: ITabSheetEvent read GetRemoveTab;
    property OnBeforePublish: IThreadEvent read GetBeforePublish;
    property OnAfterPublish: IThreadEvent read GetAfterPublish;
    property OnBeforeCrawling: IThreadEvent read GetBeforeCrawling;
    property OnAfterCrawling: IThreadEvent read GetAfterCrawling;

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
  // OutputDebugString('CALL');
  // CallControlAligner;
  tResize.Enabled := True;
end;

procedure TfMain.tResizeTimer(Sender: TObject);
begin
  // OutputDebugString('CALL');
  CallControlAligner;
  tResize.Enabled := False;
end;

procedure TfMain.pcMainCanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
begin
  FRemoveTab.Invoke(TabSheetController[ATabIndex]);

  ACanClose := CanCloseTabSheet(ATabIndex);

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
    OnTabCaptionChange.Invoke(pcMain.ActivePage.Caption);
    with ActiveTabSheetController do
      ActiveViewType := ViewType;
  end
  else
  begin
    OnTabCaptionChange.Invoke('');
    SwitchDesignView(False);
  end;

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

procedure TfMain.pcMainNewTabCreate(Sender: TObject; AIndex: Integer);
begin
  // FAddTab.Invoke(TabSheetController[AIndex]); see: TTabSheetController.Initialized
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
  Result := True;
end;

function TfMain.UnlockPageControl: Boolean;
begin
  Dec(FLockCount);

  if (FLockCount = 0) then
  begin
    SendMessage(pcMain.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(pcMain.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
  Result := True;
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

function TfMain.GetTabSheetController(AIndex: Integer): ITabSheetController;
begin
  with pcMain do
  begin
    Properties.BeginUpdate;
    try
      Result := TTabSheetController(Pages[AIndex]);
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

function TfMain.GetTabCaptionChange: ICaptionChangeEvent;
begin
  Result := FTabCaptionChange;
end;

function TfMain.GetAddTab: ITabSheetEvent;
begin
  Result := FAddTab;
end;

function TfMain.GetRemoveTab: ITabSheetEvent;
begin
  Result := FRemoveTab;
end;

function TfMain.GetBeforePublish: IThreadEvent;
begin
  Result := PublishManager.OnBeforeExecute;
end;

function TfMain.GetAfterPublish: IThreadEvent;
begin
  Result := PublishManager.OnAfterExecute;
end;

function TfMain.GetBeforeCrawling: IThreadEvent;
begin
  Result := CrawlerManager.OnBeforeExecute;
end;

function TfMain.GetAfterCrawling: IThreadEvent;
begin
  Result := CrawlerManager.OnAfterExecute;
end;

constructor TfMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLockCount := 0;
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
  FTabCaptionChange := TICaptionChangeEvent.Create;
  FAddTab := TITabSheetEvent.Create;
  FRemoveTab := TITabSheetEvent.Create;
end;

procedure TfMain.CallBackupManager;
begin
  if (TabSheetCount > 0) then
    CallBackupManager(ActiveTabSheetIndex);
end;

procedure TfMain.CallBackupManager(const ATabIndex: Integer);
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
    FBackupManager.Backup(TabSheetController[ATabIndex]);
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

procedure TfMain.CallPublish;
begin
  if (TabSheetCount > 0) then
    CallPublish(ActiveTabSheetIndex);
end;

procedure TfMain.CallPublish(const ATabIndex: Integer);
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
    PublishManager.AddPublishJob(TabSheetController[ATabIndex].PublishController.GeneratePublishJob);
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

procedure TfMain.CallCrawler;
begin
  if (TabSheetCount > 0) then
    CallCrawler(ActiveTabSheetIndex);
end;

procedure TfMain.CallCrawler(const ATabIndex: Integer);
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    CallBackupManager;

    CrawlerManager.AddCrawlerJob(ActiveTabSheetController.ControlController);
  end;
end;

procedure TfMain.CallSeriesCrawler;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CrawlerManager.AddCrawlerJob(TabSheetController[I].ControlController);
end;

procedure TfMain.CallCrypterCrypt;
begin
  if (TabSheetCount > 0) then
    CallCrypterCrypt(ActiveTabSheetIndex);
end;

procedure TfMain.CallCrypterCrypt(const ATabIndex: Integer);
var
  I, J: Integer;
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
      for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
        with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
          if SameStr('', Crypter[J].Value) then
            CrypterManager.AddCrypterJob(Crypter[J]);
  end;
end;

procedure TfMain.CallSeriesCrypterCrypt;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallCrypterCrypt(I);
end;

procedure TfMain.CallCrypterCheck;
begin
  if (TabSheetCount > 0) then
    CallCrypterCheck(ActiveTabSheetIndex);
end;

procedure TfMain.CallCrypterCheck(const ATabIndex: Integer);
var
  I, J: Integer;
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
      for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
        with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
          if not SameStr('', Crypter[J].Value) then
            CrypterManager.AddCrypterCheckJob(Crypter[J]);
  end;
end;

procedure TfMain.CallSeriesCrypterCheck;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallCrypterCheck(I);
end;

procedure TfMain.CallFileHosterCheck;
begin
  if (TabSheetCount > 0) then
    CallFileHosterCheck(ActiveTabSheetIndex);
end;

procedure TfMain.CallFileHosterCheck(const ATabIndex: Integer);
var
  I, J: Integer;
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
      for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].DirectlinkCount - 1 do
        with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
          if not SameStr('', Directlink[J].Value) then
            FileHosterManager.AddHosterCheckJob(Directlink[J]);
  end;
end;

procedure TfMain.CallSeriesFileHosterCheck;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallFileHosterCheck(I);
end;

procedure TfMain.CallImageHosterRemoteUpload;
begin
  if (TabSheetCount > 0) then
    CallImageHosterRemoteUpload(ActiveTabSheetIndex);
end;

procedure TfMain.CallImageHosterRemoteUpload(const ATabIndex: Integer);
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
    TabSheetController[ATabIndex].ControlController.InitiateImageHosterRemoteUpload(False);
end;

procedure TfMain.CallSeriesImageHosterRemoteUpload;
var
  I: Integer;
begin
  for I := 0 to TabSheetCount - 1 do
    CallImageHosterRemoteUpload(I);
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

function TfMain.CreateTabSheet(const ATemplateFileName: WideString; ATypeID: TTypeID; AEmptyTab: WordBool = True): Integer;
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
        PageControl := pcMain;

        TemplateFileName := ExtractFileName(ChangeFileExt(ATemplateFileName, ''));

        Install;
      end;
    finally
      pcMain.Properties.CancelUpdate; // prevent to call OnChanged
    end;

    pcMain.ActivePage := LNewTabSheetController;

    if SettingsManager.Settings.ControlAligner.MirrorPosition = mpTop then
    begin
      if not AEmptyTab then
      begin
        for LMirrorIndex := 0 to SettingsManager.Settings.ControlAligner.MirrorCount - 1 do
          LNewTabSheetController.MirrorController.Mirror[LNewTabSheetController.MirrorController.Add].GetDirectlink.Add('');
      end;

      GetControls(ATemplateFileName, LNewTabSheetController.ControlController, Self);
    end
    else
    begin
      GetControls(ATemplateFileName, LNewTabSheetController.ControlController, Self);

      if not AEmptyTab then
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
    with PublishController do
    begin
      OnUpdateCMSList.Add(Main.fPublish.GetUpdateCMSListEvent);
      OnUpdateCMSWebsiteList.Add(Main.fPublish.GetUpdateCMSWebsiteListEvent);
      OnUpdateCMSWebsite.Add(Main.fPublish.GetUpdateCMSWebsiteEvent);
    end;

    if not AEmptyTab then
    begin
      Initialized;
      pcMain.OnChange(pcMain);
    end;
  end;

  Result := ActiveTabSheetIndex;
end;

function TfMain.NewTabSheet(const ATemplateName: WideString): Integer;
begin
  Result := CreateTabSheet(GetTemplatesTypeFolder + ATemplateName + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + ATemplateName + '.xml').TemplateType, False);
end;

function TfMain.InternalOpenFile(const AFileName: string): Integer;
begin
  Result := -1;
  try
    Result := TPluginBasic.LoadFile(AFileName, Self);

    if not(Result = -1) then
      TLogManager.Instance().Add(Format('File %s could not be loaded.', [AFileName]));

    pcMain.OnChange(pcMain);
  except
    TLogManager.Instance().Add(Format('Critical error loading file %s.', [AFileName]));
  end;
end;

function TfMain.InternalOpenFiles(const AFiles: TStrings): Integer;
var
  LFileIndex, LTabIndex: Integer;
begin
  Result := -1;
  LTabIndex := -1;

  LockPageControl;
  try
    for LFileIndex := 0 to AFiles.Count - 1 do
    begin
      LTabIndex := InternalOpenFile(AFiles.Strings[LFileIndex]);
      if not(LTabIndex = -1) and (Result = -1) then
        Result := LTabIndex;
    end;
  finally
    UnlockPageControl;
  end;
end;

function TfMain.OpenTabSheet(const AFileName: WideString = ''): Integer;
var
  LFileFormats: TStrings;
  LFileFormatsIndex: Integer;
  LFileFilter: string;
begin
  if not FileExists(AFileName) then
  begin
    LFileFormats := TPluginBasic.GetLoadFileFormats;
    try
      if not(LFileFormats.Count > 0) then
      begin
        MessageDlg('There is no file format plugin that can be used to open the file.', mtError, [mbOK], 0);
        Exit(-1);
      end;

      LFileFilter := '';
      for LFileFormatsIndex := 0 to LFileFormats.Count - 1 do
        LFileFilter := LFileFilter + LFileFormats.ValueFromIndex[LFileFormatsIndex];
    finally
      LFileFormats.Free;
    end;

    with TOpenDialog.Create(nil) do
      try
        InitialDir := GetDocumentsFolder;
        Filter := LFileFilter;
        Options := Options + [ofAllowMultiSelect];

        if Execute then
          Result := InternalOpenFiles(Files)
        else
          Result := -1;
      finally
        Free;
      end;
  end
  else
  begin
    Result := InternalOpenFile(AFileName);
  end;
end;

function TfMain.InternalSaveFile(const ATabSheetController: ITabSheetController; const AFileName, AFileFormat: WideString): Boolean;
var
  LPlugInCollectionItem: TPlugInCollectionItem;
  LFileExtension, LFileName: string;
begin
  with SettingsManager.Settings.Plugins do
    LPlugInCollectionItem := FindPlugInCollectionItemFromCollection(AFileFormat, FileFormats);

  with SettingsManager.Settings.Plugins do
    LFileExtension := TPluginBasic.GetFileFormatFileExtension(LPlugInCollectionItem);

  if not SameStr(LFileExtension, ExtractFileExt(AFileName)) then
    LFileName := AFileName + LFileExtension;

  Result := TPluginBasic.SaveFile(LPlugInCollectionItem, LFileName, ATabSheetController);

  if Result then
    ATabSheetController.FileSaved(AFileName, AFileFormat);
end;

function TfMain.SaveTabSheet(const ATabIndex: Integer; const AFileName: WideString = ''; const AFileFormat: WideString = ''; const AForceDialog: WordBool = False): WordBool;
var
  LNeedDialog, LFileFormatFound: Boolean;
  LFileFormats: TStrings;
  LFileFormatsIndex: Integer;
  LFileFormat, LFileFilter, LFileName: string;
begin
  // file format
  if not SameStr('', AFileFormat) then
    LFileFormat := AFileFormat
  else if not SameStr('', TabSheetController[ATabIndex].FileFormat) then
    LFileFormat := TabSheetController[ATabIndex].FileFormat
  else
    LFileFormat := TPluginBasic.GetDefaultSaveFileFormat;

  // need dialog
  LNeedDialog := AForceDialog or not FileExists(TabSheetController[ATabIndex].FileName);

  LFileFormats := TPluginBasic.GetSaveFileFormats;
  try
    if not(LFileFormats.Count > 0) then
    begin
      MessageDlg('There is no file format plugin that can be used to save the file.', mtError, [mbOK], 0);
      Exit(False);
    end;

    LFileFormatFound := False;
    for LFileFormatsIndex := 0 to LFileFormats.Count - 1 do
      if SameText(LFileFormat, LFileFormats.Names[LFileFormatsIndex]) then
      begin
        LFileFormatFound := True;
        break;
      end;
    if not LFileFormatFound then
    begin
      LFileFormat := TPluginBasic.GetDefaultSaveFileFormat;
      LNeedDialog := True;
    end;

    LFileFilter := '';
    for LFileFormatsIndex := 0 to LFileFormats.Count - 1 do
      LFileFilter := LFileFilter + LFileFormats.ValueFromIndex[LFileFormatsIndex];

    if LNeedDialog then
    begin
      LFileName := IfThen(not SameStr('', AFileName), AFileName, TabSheetController[ATabIndex].SuggestFileName);
      with TSaveDialog.Create(nil) do
        try
          FileName := IfThen(not SameStr('', LFileName), LFileName, 'Unnamed');
          if not(LFileFormats.IndexOfName(LFileFormat) = -1) then
            FilterIndex := LFileFormats.IndexOfName(LFileFormat) + 1;
          Filter := LFileFilter;
          InitialDir := GetDocumentsFolder;

          if Execute then
          begin
            LFileFormat := LFileFormats.Names[FilterIndex - 1];
            Result := InternalSaveFile(TabSheetController[ATabIndex], FileName, LFileFormat);
          end
          else
          begin
            Exit(False);
          end;
        finally
          Free;
        end;
    end
    else
    begin
      Result := InternalSaveFile(TabSheetController[ATabIndex], AFileName, LFileFormat);
    end;
  finally
    LFileFormats.Free;
  end;
end;

function TfMain.SaveTabSheet(const ATabIndex: Integer; const AForceDialog: WordBool): WordBool;
begin
  Result := SaveTabSheet(ATabIndex, '', '', AForceDialog);
end;

function TfMain.SaveTheCurrentTabSheet: WordBool;
begin
  Result := SaveTabSheet(ActiveTabSheetIndex);
end;

function TfMain.SaveTheCurrentTabSheetAs: WordBool;
begin
  Result := SaveTabSheet(ActiveTabSheetIndex, True);
end;

function TfMain.SaveAllTabSheets: WordBool;
var
  LTabIndex: Integer;
begin
  Result := True;

  for LTabIndex := 0 to TabSheetCount - 1 do
    Result := Result and SaveTabSheet(LTabIndex);
end;

function TfMain.SaveAllTabSheetsToFolder(const AFilePath: WideString = ''; const AFileFormat: WideString = ''): WordBool;
var
  LNeedDialog, LFileFormatFound: Boolean;
  LFileFormats: TStrings;
  LFileFormatsIndex: Integer;
  LFilePath, LFileFormat, LFileFilter, LFileName: string;
  LTabIndex: Integer;
begin
  Result := True;

  LFilePath := AFilePath;

  // file format
  LFileFormat := AFileFormat;

  // need dialog
  LNeedDialog := (SameStr('', LFilePath) or not DirectoryExists(LFilePath)) or (SameStr('', LFileFormat));

  LFileFormats := TPluginBasic.GetSaveFileFormats;
  try
    if not(LFileFormats.Count > 0) then
    begin
      MessageDlg('There is no file format plugin that can be used to save the files.', mtError, [mbOK], 0);
      Exit(False);
    end;

    LFileFormatFound := False;
    for LFileFormatsIndex := 0 to LFileFormats.Count - 1 do
      if SameText(LFileFormat, LFileFormats.Names[LFileFormatsIndex]) then
      begin
        LFileFormatFound := True;
        break;
      end;
    if not LFileFormatFound then
    begin
      LFileFormat := TPluginBasic.GetDefaultSaveFileFormat;
      LNeedDialog := True;
    end;

    LFileFilter := '';
    for LFileFormatsIndex := 0 to LFileFormats.Count - 1 do
      LFileFilter := LFileFilter + LFileFormats.ValueFromIndex[LFileFormatsIndex];

    if LNeedDialog then
    begin
      with TSelectFolderDialog.Create(nil) do
        try
          Filter := LFileFilter;
          Path := GetDocumentsFolder;

          if Execute then
          begin
            LFilePath := Path;
            LFileFormat := LFileFormats.Names[FilterIndex - 1];
          end
          else
          begin
            Exit(False);
          end;
        finally
          Free;
        end;
    end;
  finally
    LFileFormats.Free;
  end;

  for LTabIndex := 0 to TabSheetCount - 1 do
  begin
    LFileName := LFilePath + TabSheetController[LTabIndex].SuggestFileName;

    if not(FileExists(LFileName) and (MessageDlg('Override "' + ExtractFileName(LFileName) + '" ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes)) then
      Continue;

    Result := Result and InternalSaveFile(TabSheetController[LTabIndex], LFileName, LFileFormat);
  end;
end;

function TfMain.CanCloseTabSheet(const ATabIndex: Integer): WordBool;
var
  LTabSheetController: ITabSheetController;
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    LTabSheetController := TabSheetController[ATabIndex];

    Result := not CrawlerManager.InUse(LTabSheetController) and { }
    { } not CrypterManager.InUse(LTabSheetController) and { }
    { } not FileHosterManager.InUse(LTabSheetController) and { }
    { } not ImageHosterManager.InUse(LTabSheetController);

    LTabSheetController := nil;
  end
  else
  begin
    Result := False;
  end;
end;

function TfMain.CanCloseTheCurrentTabSheet: WordBool;
begin
  Result := CanCloseTabSheet(ActiveTabSheetIndex);
end;

function TfMain.CanCloseAllExceptTheCurrentTabSheet: WordBool;
var
  LTabIndex: Integer;
  LResult: Boolean;
begin
  LResult := True;

  for LTabIndex := 0 to TabSheetCount - 1 do
    if not(LTabIndex = ActiveTabSheetIndex) then
    begin
      LResult := CanCloseTabSheet(LTabIndex);
      if not LResult then
        break;
    end;

  Result := LResult;
end;

function TfMain.CanCloseAllTabSheets: WordBool;
var
  LTabSheetIndex: Integer;
  LResult: Boolean;
begin
  LResult := True;

  for LTabSheetIndex := 0 to TabSheetCount - 1 do
  begin
    LResult := CanCloseTabSheet(LTabSheetIndex);
    if not LResult then
      break;
  end;

  Result := LResult;
end;

function TfMain.CloseTabSheet(const ATabIndex: Integer): WordBool;
begin
  if (ATabIndex >= 0) and (ATabIndex < TabSheetCount) then
  begin
    with pcMain do
    begin
      Properties.BeginUpdate;
      try
        try
          CloseTab(ATabIndex);
          Result := True;
        except
          Result := False;
        end;
      finally
        Properties.EndUpdate;
      end;
      OnChange(pcMain);
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function TfMain.CloseTheCurrentTabSheet: WordBool;
begin
  Result := CloseTabSheet(ActiveTabSheetIndex);
end;

function TfMain.CloseAllExceptTheCurrentTabSheet: WordBool;
var
  LTabIndex: Integer;
begin
  // remove all tabs on the right side
  for LTabIndex := TabSheetCount - 1 downto ActiveTabSheetIndex + 1 do
    CloseTabSheet(LTabIndex);
  // remove all tabs on the left side
  for LTabIndex := TabSheetCount - 2 downto 0 do
    CloseTabSheet(LTabIndex);

  Result := (TabSheetCount = 1);
end;

function TfMain.CloseAllTabSheets: WordBool;
begin
  while TabSheetCount > 0 do
    CloseTheCurrentTabSheet;

  Result := (TabSheetCount = 0);
end;

function TfMain.TabSheetCount: Integer;
begin
  Result := pcMain.PageCount;
end;

destructor TfMain.Destroy;
begin
  FRemoveTab := nil;
  FAddTab := nil;
  FTabCaptionChange := nil;
  FViewChange := nil;
  FChange := nil;

  FControlAligner.Free;
  FBackupManager.Free;

  inherited Destroy;
end;

end.
