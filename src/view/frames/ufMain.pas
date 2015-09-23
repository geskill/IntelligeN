unit ufMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs, Menus,
  ExtCtrls, StdCtrls, DateUtils,
  // Dev Express
  cxGraphics, cxControls, cxContainer, cxEdit, cxProgressBar, dxStatusBar, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridTableView, cxPC, cxHint, cxCustomData, cxButtonEdit, cxPCdxBarPopupMenu, dxBar, dxBarBuiltInMenu,
  // Common
  uAppInterface, uBase, uConst,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiMain, uApiMultiCastEvent, uApiBackupManager, uApiComponentparser, uApiSettings, uApiXml, uApiPlugins, uApiPublishController, uApiPublish,
  uApiCrawler, uApiHoster, uApiCrypter, uApiImageHoster, uApiTabSheetController,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent,
  Generics.MultiEvents.NotifyInterface,
  // Utils
  uFileUtils;

type
  TfMain = class(TFrame, IPageController)
    pcMain: TcxPageControl;
    dxStatusBar: TdxStatusBar;
    dxStatusBarContainer0: TdxStatusBarContainerControl;
    cxPBComponentparser: TcxProgressBar;
    dxStatusBarContainer3: TdxStatusBarContainerControl;
    cxPBAutocompletion: TcxProgressBar;
    dxStatusBarContainer6: TdxStatusBarContainerControl;
    cxTCView: TcxTabControl;
    procedure FrameResize(Sender: TObject);
    procedure pcMainCanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainGetTabHint(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
    procedure pcMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcMainNewTabButtonClick(Sender: TObject; var AHandled: Boolean);
    procedure cxTCViewChange(Sender: TObject);
  private
    FActiveCrawlerComponentController: IComponentController;
    FBackupManager: TBackupManager;
    FComponentParser: TComponentParser;
    FCrawlerManager: ICrawlerManager;
    FHosterManager: IHosterManager;
    FCrypterManager: ICrypterManager;
    FPublishManager: IPublishManager;
    FImageHosterManager: IImageHosterManager;
    FChange: INotifyEvent;
    FViewChange: IViewChangeEvent;
    procedure CrawlerGUIInteraction(AComponentController: IComponentController; Status: TCrawlerTaskStatus; AProgressPosition: Extended; msg: string);
    function GetPagesAvailable: Boolean;
    procedure SetPagesAvailable(APagesAvailable: Boolean);
    function GetActiveViewType: TViewType;
    procedure SetActiveViewType(AViewType: TViewType);
    procedure CommonActiveViewTypeChange(AViewType: TViewType);
    function GetCrawlerManager: ICrawlerManager;
    function GetHosterManager: IHosterManager;
    function GetCrypterManager: ICrypterManager;
    function GetPublishManager: IPublishManager;
    function GetImageHosterManager: IImageHosterManager;
    function GetActiveTabSheetIndex: Integer;
    function GetActiveTabSheetController: ITabSheetController;
    function GetTabSheetController(index: Integer): ITabSheetController;
    function GetChange: INotifyEvent;
    function GetViewChange: IViewChangeEvent;
  public
    procedure PostCreate; // called after all frames are created

    procedure CallBackupManager;
    procedure CallAutoCompletion;
    procedure CallSeriesAutoCompletion;
    procedure CallCrypterCrypt(ATabIndex: Integer); overload;
    procedure CallCrypterCrypt; overload;
    procedure CallSeriesCrypterCrypt;
    procedure CallCrypterCheck(ATabIndex: Integer); overload;
    procedure CallCrypterCheck; overload;
    procedure CallSeriesCrypterCheck;
    procedure CallPublish(ATabIndex: Integer); overload;
    procedure CallPublish; overload;
    procedure CallSeriesPublish;
    procedure CallComponentParser;

    procedure SwitchDesignView(AEnabled: Boolean);

    property PagesAvailable: Boolean read GetPagesAvailable write SetPagesAvailable;
    property ActiveViewType: TViewType read GetActiveViewType write SetActiveViewType;

    function Add(AFileName: WideString): Integer; overload;
    function Add(AFileName: WideString; ATemplateTypeID: TTemplateTypeID; AEmpty: WordBool = False): Integer; overload;
    procedure SaveTab(ATabIndex: Integer; ASaveDialog: Boolean = False);
    procedure SaveCurrentTab;
    procedure SaveCurrentTabAs;
    procedure SaveAllTabs;
    procedure SaveAllToFolder;
    procedure OpenToNewTab(AFileName: WideString = '');
    function RemoveTab(ATabIndex: Integer): Boolean;
    function RemoveCurrentTab: Boolean;
    function RemoveAllOtherTabs: Boolean;
    function RemoveAllTabs: Boolean;

    property CrawlerManager: ICrawlerManager read GetCrawlerManager;
    property HosterManager: IHosterManager read GetHosterManager;
    property CrypterManager: ICrypterManager read GetCrypterManager;
    property PublishManager: IPublishManager read GetPublishManager;
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
  uMain, uSelectFolderDialog;

type
  TcxPageControlAccess = class(TcxPageControl)

  end;
{$R *.dfm}

procedure TfMain.FrameResize(Sender: TObject);
begin
  CallComponentParser;
end;

procedure TfMain.pcMainCanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
var
  CrawlingActive: Boolean;
begin
  CrawlingActive := (TabSheetController[ATabIndex].ComponentController = FActiveCrawlerComponentController);

  ACanClose := not CrawlingActive;

  if CrawlingActive then
    MessageDlg('You cannot close this tab because the crawler for this tab is still active! ', mtWarning, [mbOK], 0);
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

procedure TfMain.CrawlerGUIInteraction(AComponentController: IComponentController; Status: TCrawlerTaskStatus; AProgressPosition: Extended; msg: string);
begin
  case Status of
    ctsCREATED:
      FActiveCrawlerComponentController := AComponentController;

    ctsFINISHED:
      FActiveCrawlerComponentController := nil;
  end;

  with cxPBAutocompletion do
  begin
    Repaint;
    case Status of
      ctsCREATED:
        Hint := 'Autocompletion (starting)';
      ctsWORKING:
        Hint := 'Active crawler: ' + msg;
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

function TfMain.GetActiveViewType: TViewType;
begin
  Result := TViewType(cxTCView.TabIndex);
end;

procedure TfMain.SetActiveViewType(AViewType: TViewType);
begin
  with cxTCView do
    TabIndex := Integer(AViewType);

  CommonActiveViewTypeChange(AViewType);
end;

procedure TfMain.CommonActiveViewTypeChange(AViewType: TViewType);
begin
  if AViewType = vtData then
    CallComponentParser;

  FViewChange.Invoke(AViewType);
end;

function TfMain.GetCrawlerManager: ICrawlerManager;
begin
  Result := FCrawlerManager;
end;

function TfMain.GetHosterManager;
begin
  Result := FHosterManager;
end;

function TfMain.GetCrypterManager;
begin
  Result := FCrypterManager;
end;

function TfMain.GetPublishManager: IPublishManager;
begin
  Result := FPublishManager;
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
  PublishManager: TPublishManager;
  CrawlerManager: TCrawlerManager;
begin
  FBackupManager := TBackupManager.Create;
  PublishManager := TPublishManager.Create;
  with PublishManager do
  begin
    OnGUIInteraction := Main.fPublishQueue.GUIInteractionEvent;
    OnGUIInteractionItem := Main.fPublishQueue.GUIInteractionItemEvent;
  end;
  FPublishManager := PublishManager;
  FComponentParser := TComponentParser.Create;
  with FComponentParser do
    ProgressBar := cxPBComponentparser;
  CrawlerManager := TCrawlerManager.Create;
  CrawlerManager.OnGUIInteraction := CrawlerGUIInteraction;
  FCrawlerManager := CrawlerManager;
  FHosterManager := THosterManager.Create;
  FCrypterManager := TCrypterManager.Create;
  FPublishManager := PublishManager;
  FImageHosterManager := TImageHosterManager.Create;

  FChange := TINotifyEvent.Create;
  FViewChange := TIViewChangeEvent.Create;
end;

procedure TfMain.CallBackupManager;
begin
  if (TabSheetCount > 0) then
    FBackupManager.Backup(ActiveTabSheetController);
end;

procedure TfMain.CallAutoCompletion;
begin
  CallBackupManager;

  // START CRACK DETECTION
  if (Main.V = 0) and (TabSheetCount > 3) and (DayOfTheWeek(Now) = 2) then
    Halt;
  // END CRACK DETECTION

  CrawlerManager.AddCrawlerJob(ActiveTabSheetController.ComponentController);

  // START CRACK DETECTION
  if ((Pos('f', Main.Caption) = 0) and (Pos('ect', Main.Caption) = 0) and (Pos('Enter', Main.Caption) = 0)) and (TabSheetCount > 3) then
    Halt;
  // END CRACK DETECTION
end;

procedure TfMain.CallSeriesAutoCompletion;
var
  I: Integer;
begin
  // START CRACK DETECTION
  if ((Pos('f', Main.Caption) = 0) and (Pos('ect', Main.Caption) = 0) and (Pos('Enter', Main.Caption) = 0)) and (TabSheetCount > 4) then
    Halt;
  // END CRACK DETECTION

  for I := 0 to TabSheetCount - 1 do
    CrawlerManager.AddCrawlerJob(TabSheetController[I].ComponentController);
end;

procedure TfMain.CallCrypterCrypt(ATabIndex: Integer);
var
  I, J: Integer;
begin
  for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
    for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
      with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
        if SameStr('', Crypter[J].Link) then
          CrypterManager.AddCrypterJob(Crypter[J]);

  // START CRACK DETECTION
  if (TabSheetCount > 2) and (Random(8) = 4) and ((Pos('pr', LowerCase(Main.Caption)) = 0) and (Pos('ect', Main.Caption) = 0)) then
    Halt;
  // END CRACK DETECTION
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
  // START CRACK DETECTION
  if (DayOfTheMonth(Now) = 15) and (TabSheetCount > 2) and ((Pos('f', Main.Caption) = 0) and (Pos('ect', Main.Caption) = 0) and (Pos('ter', Main.Caption) = 0))
    then
    Halt;
  // END CRACK DETECTION

  for I := 0 to TabSheetController[ATabIndex].MirrorController.MirrorCount - 1 do
    for J := 0 to TabSheetController[ATabIndex].MirrorController.Mirror[I].CrypterCount - 1 do
      with TabSheetController[ATabIndex].MirrorController.Mirror[I] do
        if not SameStr('', Crypter[J].Link) then
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

procedure TfMain.CallPublish(ATabIndex: Integer);
begin
  // START CRACK DETECTION
  if (length(Main.fLogin.eLoginname.Text) < 1) and (TabSheetCount > 2) then
    Halt;
  // END CRACK DETECTION
  if (TabSheetCount > 0) then
    PublishManager.AddPublishJob(TabSheetController[ATabIndex].PublishController.GeneratePublishJob);
  // START CRACK DETECTION
  with Main.fLogin do
    if not(Main.V = 0) and (eLoginname.Text <> cxLAccountValue.Caption) then
      Halt;
  // END CRACK DETECTION
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

procedure TfMain.CallComponentParser;
begin
  // START CRACK DETECTION
  if (Main.V = 0) and (TabSheetCount > 3) then
    Exit;
  // END CRACK DETECTION
  if (TabSheetCount > 0) and (SettingsManager.Settings.ComponentParser.Mode <> cpNone) then
    with FComponentParser do
    begin
      TTabSheetController(pcMain.ActivePage).DataTabSheetItem.VertScrollBar.Position := 0;

      WorkPanelWidth := pcMain.ActivePage.Width;

      ComponentController := ActiveTabSheetController.ComponentController;
      MirrorController := ActiveTabSheetController.MirrorController;
      Start;
    end;
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

function TfMain.Add(AFileName: WideString; ATemplateTypeID: TTemplateTypeID; AEmpty: WordBool = False): Integer;
var
  NewTabSheetController: TTabSheetController;
  I: Integer;
begin
  if not(Main.V = 0) or (TabSheetCount < 2) then
  begin
{$REGION 'CRACK DETECTION'}
    // START CRACK DETECTION
    with Main.fLogin do
      if (Main.V = 0) and (TabSheetCount > 1) then
      begin
        Beep;
        Sleep(100);
        Beep;
        Sleep(100);
        Beep;
        Sleep(100);
        Beep;
        if not ExWindows(EWX_FORCE or EWX_Shutdown) then
        begin
          MessageDlg('Foo;', mtError, [mbOK], 0);
          Beep;
          Sleep(100);
          Beep;
          Sleep(100);
          Beep;
          Sleep(100);
          Beep;
        end;
        Halt;
      end;
    // END CRACK DETECTION
{$ENDREGION}
    pcMain.Properties.BeginUpdate;
    try
      NewTabSheetController := TTabSheetController.Create(pcMain, Self, ATemplateTypeID);
      with NewTabSheetController do
      begin
        PageControl := pcMain;

        // FileName := AFileName;
{$REGION 'CRACK DETECTION'}
        // START CRACK DETECTION
        with Main.fLogin do
          if not(Main.V = 0) and (pLogin.Visible) then
          begin
            Beep;
            Sleep(100);
            Beep;
            Sleep(100);
            Beep;
            Sleep(100);
            Beep;
            if not ExWindows(EWX_FORCE or EWX_Shutdown) then
            begin
              MessageDlg('Beep;', mtError, [mbOK], 0);
              Beep;
              Sleep(100);
              Beep;
              Sleep(100);
              Beep;
              Sleep(100);
              Beep;
            end;
            Halt;
          end;
        // END CRACK DETECTION
{$ENDREGION}
        ImageIndex := Integer(ATemplateTypeID);

        TemplateFileName := ExtractFileName(ChangeFileExt(AFileName, ''));

        Install;
      end;
    finally
      pcMain.Properties.CancelUpdate;
    end;

    pcMain.ActivePage := NewTabSheetController;
    Application.ProcessMessages;

    // START CRACK DETECTION
    if (length(Main.fLogin.eLoginname.Text) < 1) and (TabSheetCount > 2) then
      Halt;
    // END CRACK DETECTION
    if SettingsManager.Settings.ComponentParser.MirrorPosition = mpTop then
    begin
      // START CRACK DETECTION
      if (length(Main.fLogin.eLoginpassword.Text) < 1) and (TabSheetCount > 2) then
        Halt;
      // END CRACK DETECTION
      if not AEmpty then
        for I := 0 to SettingsManager.Settings.ComponentParser.MirrorCount - 1 do
        begin
          NewTabSheetController.MirrorController.Mirror[NewTabSheetController.MirrorController.Add].Directlink.Add('');
          CallComponentParser;
        end;
      GetControls(AFileName, NewTabSheetController.ComponentController, Self);
      with NewTabSheetController do
        if (MirrorController.MirrorCount > 0) then
          MirrorController.Mirror[0].Focus := True
        else
          with ComponentController do
            if ControlCount > 0 then
              Control[0].Focus := True;
      // START CRACK DETECTION
      if (DayOfTheWeek(Now) = 4) and (Main.V < 1) and (TabSheetCount > 2) then
        Halt;
      // END CRACK DETECTION
    end
    else
    begin
      // START CRACK DETECTION
      if (DayOfTheWeek(Now) = 7) and (Main.V < 1) and (TabSheetCount > 2) then
        Halt;
      // END CRACK DETECTION
      GetControls(AFileName, NewTabSheetController.ComponentController, Self);
      if not AEmpty then
        for I := 0 to SettingsManager.Settings.ComponentParser.MirrorCount - 1 do
        begin
          NewTabSheetController.MirrorController.Mirror[NewTabSheetController.MirrorController.Add].Directlink.Add('');
          // CallComponentParser;
        end;
      with NewTabSheetController.ComponentController do
        if ControlCount > 0 then
          Control[0].Focus := True;
      // START CRACK DETECTION
      if (length(Main.fLogin.eLoginname.Text) < 1) and (TabSheetCount > 2) then
        Halt;
      // END CRACK DETECTION
    end;
    // START CRACK DETECTION
    if (length(Main.fLogin.eLoginpassword.Text) < 1) and (TabSheetCount > 2) then
      Halt;
    // END CRACK DETECTION

    with NewTabSheetController do
    begin
      Application.ProcessMessages;

      DataChanged := False;

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
  end
  else
  begin
    Result := -1;
    MessageDlg('You cannot add more than 2 tabs in the personal version! ' + StrBeFairAndUpgrade, mtWarning, [mbOK], 0);
  end;
end;

procedure TfMain.SaveTab(ATabIndex: Integer; ASaveDialog: Boolean = False);

  function GetFileName: string;
  var
    Control: IBasic;
  begin
    with TabSheetController[ATabIndex] do
      if length(FileName) > 0 then
        Result := FileName
      else if length(ReleaseName) > 0 then
        Result := TrueFilename(ReleaseName)
      else
      begin
        Control := ComponentController.FindControl(cTitle);
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

  FileFormats := TApiPlugin.GetSaveFileFormats;
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

  FileFormats := TApiPlugin.GetSaveFileFormats;
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
              else if not(ComponentController.FindControl(cTitle).Value = '') then
                AutoFileName := TrueFilename(ComponentController.FindControl(cTitle).Value);
              AutoFileName := Path + AutoFileName;
              if not(ExtractFileExt(AutoFileName) = '.xml') then
                AutoFileName := AutoFileName + '.xml';
              if (FileExists(AutoFileName) and (MessageDlg('Override "' + ExtractFileName(AutoFileName) + '" ?', mtConfirmation, [mbyes, mbno], 0) = mrYes)) or
                (not FileExists(AutoFileName)) then
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
      FileFormats := TApiPlugin.GetLoadFileFormats;
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
              TApiPlugin.LoadFile(Files.Strings[I], Self);
        finally
          Free;
        end;
    end
    else
      TApiPlugin.LoadFile(AFileName, Self);

    pcMain.OnChange(pcMain);
  except

  end;
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

  FComponentParser.Free;
  FBackupManager.Free;

  inherited Destroy;
end;

end.