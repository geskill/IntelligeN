unit uMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls,
  ImgList, ActnList, ShellAPI,
  // Dev Express
  dxBar, cxClasses, dxDockControl, dxDockPanel, cxEdit, cxDropDownEdit, cxLookAndFeels, cxPC, cxBarEditItem,
  // dxSkinsCore, dxSkinsdxDockControlPainter, dxSkinsdxBarPainter, dxSkinsForm,
  // DEC
  DECFmt,
  // OneInstance
  { OneInstance, }
  // MultiEvent
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiCodeTag, uApiConst, uApiLogManager, uApiMainMenu, uApiMain, uApiSettings, uApiTabSheetController, uApiXml,
  // DLLs
  uExport,
  // Forms
  uSettings, uSelectDialog, uAbout, uUpdate,
  // Frames
  ufLogin, ufLinklist, ufMain, ufDesigner, ufDesignObjectInspector, ufControlEditor, ufDatabase, ufPublish, ufHTTPLogger, ufPublishQueue, ufErrorLogger;

type
  TMain = class(TForm, IAppController)
    ActionList: TActionList;
{$REGION 'TAction'}
    aBold: TAction;
    aItalic: TAction;
    aUnderline: TAction;
    aStrikeout: TAction;
    aSize: TAction;
    aColor: TAction;
    aLeft: TAction;
    aCenter: TAction;
    aRight: TAction;
    aImage: TAction;
    aList: TAction;
    aEMail: TAction;
    aYoutube: TAction;
    aURL: TAction;
    aQuote: TAction;
    aCode: TAction;
    aSpoiler: TAction;
    aHide: TAction;
    aNew: TAction;
    aOpen: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aSaveAll: TAction;
    aSaveAllToFolder: TAction;
    aClose: TAction;
    aCloseAllOther: TAction;
    aCloseAll: TAction;
    aExit: TAction;
    aWindowControlEditor: TAction;
    aWindowDatabase: TAction;
    aWindowLogin: TAction;
    aWindowMain: TAction;
    aWindowErrorLogger: TAction;
    aWindowHTTPLogger: TAction;
    aWindowPublish: TAction;
    aWindowPublishQueue: TAction;

    aSaveActiveDesktop: TAction;
    aDeleteDesktop: TAction;
    aOptions: TAction;
    aAutoCompletion: TAction;
    aCrypterCrypt: TAction;
    aCrypterCheck: TAction;
    aQuickBackup: TAction;
    aPublish: TAction;
    aPublishItemVisit: TAction;
    aPublishItemPreview: TAction;
    aPublishItemPublish: TAction;
    aPublishItemSettings: TAction;
    aMirrorItemAdd: TAction;
    aSeriesAutoCompletion: TAction;
    aSeriesCrypterCrypt: TAction;
    aSeriesCrypterCheck: TAction;
    aSeriesPublish: TAction;
    aHelpFile: TAction;
    aCheckforUpdates: TAction;
    aSupportBoard: TAction;
    aVisitBlog: TAction;
    aReportIssue: TAction;
    aAbout: TAction;
{$ENDREGION}
    dxBarManager: TdxBarManager;
    dxBarDockControl: TdxBarDockControl;
    dxBarManagerBarMainMenu: TdxBar;
{$REGION 'TdxBar[SubItem/Button]'}
    nFile: TdxBarSubItem;
    nNew: TdxBarButton;
    dxBarSeparator1: TdxBarSeparator;
    nLoadFromXML: TdxBarButton;
    nSave: TdxBarButton;
    nSaveAs: TdxBarButton;
    nSaveAll: TdxBarButton;
    nSaveAllToFolder: TdxBarButton;
    nClose: TdxBarButton;
    nCloseAll: TdxBarButton;
    dxBarSeparator2: TdxBarSeparator;
    nExit: TdxBarButton;
    nEdit: TdxBarSubItem;
    nView: TdxBarSubItem;
    nControlEditor: TdxBarButton;
    nWindowDatabase: TdxBarButton;
    nWindowLogin: TdxBarButton;
    nWindowMain: TdxBarButton;
    nWindowErrorLogger: TdxBarButton;
    nWindowHTTPLogger: TdxBarButton;
    nWindowPublish: TdxBarButton;
    nWindowPublishQueue: TdxBarButton;
    nDesktop: TdxBarSubItem;
    dxBarSeparator4: TdxBarSeparator;
    nSaveActiveDesktop: TdxBarButton;
    nDeleteDesktop: TdxBarButton;
    nTools: TdxBarSubItem;
    nOptions: TdxBarButton;
    nAutoCompletion: TdxBarButton;
    nCrypterCrypt: TdxBarButton;
    nCrypterCheck: TdxBarButton;
    nQuickBackup: TdxBarButton;
    nPublish: TdxBarButton;
    nSeries: TdxBarSubItem;
    nSeriesAutoCompletion: TdxBarButton;
    nSeriesCrypterCrypt: TdxBarButton;
    nSeriesCrypterCheck: TdxBarButton;
    nSeriesPublish: TdxBarButton;
    nHelp: TdxBarSubItem;
    nHelpFile: TdxBarButton;
    nCheckforUpdates: TdxBarButton;
    nSupportBoard: TdxBarButton;
    nVisitBlog: TdxBarButton;
    nReportIssue: TdxBarButton;
    nAbout: TdxBarButton;
{$ENDREGION}
    dxBarManagerBarQuickBar: TdxBar;
    dxBBSaveAsXML: TdxBarButton;
    dxBBLoadFromXML: TdxBarButton;
    dxBBAutoCompletion: TdxBarButton;
    dxBBPublish: TdxBarButton;

    dxBarManagerBarType: TdxBar;
    dxBCType: TdxBarCombo;
    dxBBNew: TdxBarButton;

    dxBarManagerBarDefaultTags: TdxBar;
{$REGION 'TdxBar[SubItem/Button]'}
    cxBarEditItemEditorType: TcxBarEditItem;
    dxBBBold: TdxBarButton;
    dxBBItalic: TdxBarButton;
    dxBBUnderline: TdxBarButton;
    dxBBStrikeout: TdxBarButton;
    dxBBSize: TdxBarButton;
    dxBBColor: TdxBarButton;
    dxBBLeft: TdxBarButton;
    dxBBCenter: TdxBarButton;
    dxBBRight: TdxBarButton;
    dxBBImage: TdxBarButton;
    dxBBList: TdxBarButton;
    dxBBEMail: TdxBarButton;
    dxBBYoutube: TdxBarButton;
    dxBBUrl: TdxBarButton;
    dxBBQuote: TdxBarButton;
    dxBBCode: TdxBarButton;
    dxBBSpoiler: TdxBarButton;
    dxBBHide: TdxBarButton;
{$ENDREGION}
    dxBarManagerBarLayout: TdxBar;
    cxBEILayout: TcxBarEditItem;
    dxBBSaveActiveDesktop: TdxBarButton;

    dxDockingManager: TdxDockingManager;
    dxDockSite: TdxDockSite;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
    dxDPLogin: TdxDockPanel;
    fLogin: TfLogin;
    dxDPControlEditor: TdxDockPanel;
    fControlEditor: TfControlEditor;
    dxLayoutDockSite4: TdxLayoutDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxLayoutDockSite3: TdxLayoutDockSite;
    dxTabContainerDockSite1: TdxTabContainerDockSite;
    dxDPMain: TdxDockPanel;
    fMain: TfMain;
    dxDPErrorLogger: TdxDockPanel;
    fErrorLogger: TfErrorLogger;
    dxDPHTTPLogger: TdxDockPanel;
    fHTTPLogger: TfHTTPLogger;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    dxDPDatabase: TdxDockPanel;
    dxTabContainerDockSite2: TdxTabContainerDockSite;
    dxDPPublish: TdxDockPanel;
    fPublish: TfPublish;
    dxDPPublishQueue: TdxDockPanel;
    fPublishQueue: TfPublishQueue;
{$ENDREGION}
    ImageList: TImageList;
    ILContainerStatusImages: TImageList;

    cxLookAndFeelController: TcxLookAndFeelController;
    ILTypeIDs: TImageList;

    dxBpmPageControlRightClick: TdxBarPopupMenu;
    dxBBmiClose: TdxBarButton;
    dxBBmiCloseAllOther: TdxBarButton;

    dxBpmPublishDropDownClick: TdxBarPopupMenu;
    dxBBPublishItemPreview: TdxBarButton;
    dxBBPublishItemPublish: TdxBarButton;
    dxBBPublishItemSettings: TdxBarButton;
    dxBBPublishItemVisit: TdxBarButton;
    dxBpmMirrorRightClick: TdxBarPopupMenu;
    dxBBMirrorItemAdd: TdxBarButton;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
{$REGION 'TAction.Execute'}
    // DesignCode
    procedure aBoldExecute(Sender: TObject);
    procedure aItalicExecute(Sender: TObject);
    procedure aUnderlineExecute(Sender: TObject);
    procedure aStrikeoutExecute(Sender: TObject);
    procedure aSizeExecute(Sender: TObject);
    procedure aColorExecute(Sender: TObject);
    procedure aLeftExecute(Sender: TObject);
    procedure aCenterExecute(Sender: TObject);
    procedure aRightExecute(Sender: TObject);
    procedure aImageExecute(Sender: TObject);
    procedure aListExecute(Sender: TObject);
    procedure aEMailExecute(Sender: TObject);
    procedure aYoutubeExecute(Sender: TObject);
    procedure aURLExecute(Sender: TObject);
    procedure aQuoteExecute(Sender: TObject);
    procedure aCodeExecute(Sender: TObject);
    procedure aSpoilerExecute(Sender: TObject);
    procedure aHideExecute(Sender: TObject);
    // File
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveAllExecute(Sender: TObject);
    procedure aSaveAllToFolderExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aCloseAllOtherExecute(Sender: TObject);
    procedure aCloseAllExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    // Edit
    // View
    procedure aWindowControlEditorExecute(Sender: TObject);
    procedure aWindowDatabaseExecute(Sender: TObject);
    procedure aWindowLoginExecute(Sender: TObject);
    procedure aWindowMainExecute(Sender: TObject);
    procedure aWindowErrorLoggerExecute(Sender: TObject);
    procedure aWindowHTTPLoggerExecute(Sender: TObject);
    procedure aWindowPublishExecute(Sender: TObject);
    procedure aWindowPublishQueueExecute(Sender: TObject);
    procedure aSaveActiveDesktopExecute(Sender: TObject);
    procedure aDeleteDesktopExecute(Sender: TObject);
    // Tools
    procedure aOptionsExecute(Sender: TObject);
    procedure aAutoCompletionExecute(Sender: TObject);
    procedure aCrypterCryptExecute(Sender: TObject);
    procedure aCrypterCheckExecute(Sender: TObject);
    procedure aQuickBackupExecute(Sender: TObject);
    procedure aPublishExecute(Sender: TObject);
    // ActivePublishItem
    procedure aPublishItemExecute(Sender: TObject);
    // ActiveMirrorItem
    procedure aMirrorItemAddExecute(Sender: TObject);
    // Series
    procedure aSeriesAutoCompletionExecute(Sender: TObject);
    procedure aSeriesCrypterCryptExecute(Sender: TObject);
    procedure aSeriesCrypterCheckExecute(Sender: TObject);
    procedure aSeriesPublishExecute(Sender: TObject);
    // Help
    procedure aHelpFileExecute(Sender: TObject);
    procedure aCheckforUpdatesExecute(Sender: TObject);
    procedure aSupportBoardExecute(Sender: TObject);
    procedure aVisitBlogExecute(Sender: TObject);
    procedure aReportIssueExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
{$ENDREGION}
    procedure dxBCTypeChange(Sender: TObject);
    procedure dxBCTypeDropDown(Sender: TObject);
    procedure cxBarEditItemEditorTypePropertiesChange(Sender: TObject);
    procedure cxBarEditItemEditorTypePropertiesInitPopup(Sender: TObject);
    procedure cxBEILayoutPropertiesCloseUp(Sender: TObject);
    procedure cxBEILayoutPropertiesInitPopup(Sender: TObject);
  private
    FOnStartup: INotifyEvent;
    FMainMenu: IMainMenu;
    FCodeDefinition: TCodeDefinition;
    // FMonitorManager: TMonitorManager;
    // FOneInstance: TOneInstance;
    // FdwJumpLists: TdwJumpLists;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure OneInstanceGetParams(Sender: TObject; const Args: array of string);
    procedure LayoutClick(Sender: TObject);
    procedure LayoutChanged(Sender: TObject);
    procedure InsertTextBetweenSelected(TagName: string);
    function GetLogManager: ILogManager;
    function GetMainMenu: IMainMenu;
    function GetPageController: IPageController;
    function GetFileHosters: WideString;
    function GetImageHosters: WideString;
    function GetCustomisedHoster(const AHoster: WideString; AShortName: WordBool = False): WideString;
    function GetControlValues(const ATypeID: TTypeID; const AComponentID: TControlID): WideString;
  public
    procedure LoadLayout(ALayoutCollectionItem: TLayoutCollectionItem);
    procedure SaveLayout(ALayoutName: string);
    procedure SetEditMenu(AMenuItems: TdxBarItemLinks);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

type
  TcxComboBoxAccess = class(TcxComboBox)
  end;

resourcestring
  StrOpen = 'Open';
  StrSaveDesktop = 'Save Desktop';
  StrSaveCurentDesktop = 'Save curent desktop as:';
  StrDeleteDesktop = 'Delete Desktop';

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMain.RemoveAllTabs;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := fMain.CanCloseAllTabs;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  if FileExists(GetTemplatesTypeFolder + SettingsManager.Settings.ActiveTypeName + '.xml') then
  begin
    dxBCType.Text := SettingsManager.Settings.ActiveTypeName;
    aNew.Enabled := True;
  end;

  with TCodeDefinitions.GetCodeDefinitions do
    try
      TcxCustomComboBoxProperties(cxBarEditItemEditorType.Properties).Items.Text := Text;
      if IndexOf(SettingsManager.Settings.ActiveEditorTypeName) <> -1 then
        cxBarEditItemEditorType.EditValue := SettingsManager.Settings.ActiveEditorTypeName
      else if Count > 0 then
        cxBarEditItemEditorType.EditValue := Strings[0];
    finally
      Free;
    end;
  cxBarEditItemEditorType.Properties.OnChange(nil);

  SettingsManager.Settings.Layout.OnLayoutChanged := LayoutChanged;

  with SettingsManager.Settings do
  begin
    cxLookAndFeelController.NativeStyle := NativeStyle;
    // dxSkinController.UseSkins := UseSkins;
    // dxSkinController.SkinName := DefaultSkin;
  end;

  {
    FOneInstance := TOneInstance.Create(Main);
    FOneInstance.Active := True;
    FOneInstance.AutoCheck := True;
    FOneInstance.AutoActivateApp := True;
    FOneInstance.Id := 'Main';
    FOneInstance.OnGetParams := OneInstanceGetParams;
    }

  SettingsManager.Settings.Layout.Main.LoadLayout(Self);

  Caption := ProgrammName;

  FMainMenu := TIMainMenu.Create(dxBarManagerBarMainMenu);

  // FMonitorManager := TMonitorManager.Create;

  fMain.PostCreate;

  DragAcceptFiles(Handle, True);
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  DragAcceptFiles(Handle, False);

  for I := 0 to length(FCodeDefinition.CodeTags) - 1 do
  begin
    FCodeDefinition.CodeTags[I].Params.Free;
    FCodeDefinition.CodeTags[I].ParamValues.Free;
  end;

  // FMonitorManager.Free;

  FOnStartup := nil;
  FMainMenu := nil;

  SettingsManager.Settings.Layout.Main.SaveLayout(Self);

  // FOneInstance.Free;
end;

procedure TMain.FormShow(Sender: TObject);
begin
  with SettingsManager.Settings.ControlAligner.DefaultStartup do
  begin
    if ActiveA and FileExists(GetTemplatesTypeFolder + TypeA + '.xml') then
      fMain.Add(GetTemplatesTypeFolder + TypeA + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + TypeA + '.xml').TemplateType, False);
    if ActiveB and FileExists(GetTemplatesTypeFolder + TypeB + '.xml') then
      fMain.Add(GetTemplatesTypeFolder + TypeB + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + TypeB + '.xml').TemplateType, False);
    if ActiveC and FileExists(GetTemplatesTypeFolder + TypeC + '.xml') then
      fMain.Add(GetTemplatesTypeFolder + TypeC + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + TypeC + '.xml').TemplateType, False);
    if ActiveD and FileExists(GetTemplatesTypeFolder + TypeD + '.xml') then
      fMain.Add(GetTemplatesTypeFolder + TypeD + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + TypeD + '.xml').TemplateType, False);
    if ActiveE and FileExists(GetTemplatesTypeFolder + TypeE + '.xml') then
      fMain.Add(GetTemplatesTypeFolder + TypeE + '.xml', TApiXml.GetControlsTemplateInfo(GetTemplatesTypeFolder + TypeE + '.xml').TemplateType, False);
  end;
end;
{$REGION 'TAction.Execute'}

procedure TMain.aBoldExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('bold');
end;

procedure TMain.aItalicExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('italic');
end;

procedure TMain.aUnderlineExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('underline');
end;

procedure TMain.aStrikeoutExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('strikethrough');
end;

procedure TMain.aSizeExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('size');
end;

procedure TMain.aColorExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('color');
end;

procedure TMain.aLeftExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('left');
end;

procedure TMain.aCenterExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('center');
end;

procedure TMain.aRightExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('right');
end;

procedure TMain.aImageExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('image');
end;

procedure TMain.aListExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('list');
end;

procedure TMain.aEMailExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('email');
end;

procedure TMain.aYoutubeExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('youtube');
end;

procedure TMain.aURLExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('url');
end;

procedure TMain.aQuoteExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('quote');
end;

procedure TMain.aCodeExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('code');
end;

procedure TMain.aSpoilerExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('spoiler');
end;

procedure TMain.aHideExecute(Sender: TObject);
begin
  InsertTextBetweenSelected('hide');
end;

procedure TMain.aNewExecute(Sender: TObject);
begin
  fMain.Add(dxBCType.Text);
end;

procedure TMain.aOpenExecute(Sender: TObject);
begin
  fMain.OpenToNewTab;
end;

procedure TMain.aSaveExecute(Sender: TObject);
begin
  fMain.SaveCurrentTab;
end;

procedure TMain.aSaveAsExecute(Sender: TObject);
begin
  fMain.SaveCurrentTabAs;
end;

procedure TMain.aSaveAllExecute(Sender: TObject);
begin
  fMain.SaveAllTabs;
end;

procedure TMain.aSaveAllToFolderExecute(Sender: TObject);
begin
  fMain.SaveAllToFolder;
end;

procedure TMain.aCloseExecute(Sender: TObject);
begin
  fMain.RemoveCurrentTab;
end;

procedure TMain.aCloseAllOtherExecute(Sender: TObject);
begin
  fMain.RemoveAllOtherTabs;
end;

procedure TMain.aCloseAllExecute(Sender: TObject);
begin
  fMain.RemoveAllTabs;
end;

procedure TMain.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.aWindowControlEditorExecute(Sender: TObject);
begin
  dxDPControlEditor.Show;
end;

procedure TMain.aWindowDatabaseExecute(Sender: TObject);
begin
  dxDPDatabase.Show;
end;

procedure TMain.aWindowLoginExecute(Sender: TObject);
begin
  dxDPLogin.Show;
end;

procedure TMain.aWindowMainExecute(Sender: TObject);
begin
  dxDPMain.Show;
end;

procedure TMain.aWindowErrorLoggerExecute(Sender: TObject);
begin
  dxDPErrorLogger.Show;
end;

procedure TMain.aWindowHTTPLoggerExecute(Sender: TObject);
begin
  dxDPHTTPLogger.Show;
end;

procedure TMain.aWindowPublishExecute(Sender: TObject);
begin
  dxDPPublish.Show;
end;

procedure TMain.aWindowPublishQueueExecute(Sender: TObject);
begin
  dxDPPublishQueue.Show;
end;

procedure TMain.aSaveActiveDesktopExecute(Sender: TObject);
begin
  with TSelectDialog.Create(nil) do
    try
      SelectedItem := SettingsManager.Settings.Layout.ActiveLayoutName;
      Caption := StrSaveDesktop;
      Description := StrSaveCurentDesktop;

      with SettingsManager.Settings.Layout.GetLayoutItemList do
        try
          Items.Text := Text;
        finally
          Free;
        end;

      if Execute then
        SaveLayout(SelectedItem);
    finally
      Free;
    end;
end;

procedure TMain.aDeleteDesktopExecute(Sender: TObject);
begin
  with TSelectDialog.Create(nil) do
    try
      Caption := StrDeleteDesktop;
      Description := StrDeleteDesktop + ':';

      with SettingsManager.Settings.Layout.GetLayoutItemList do
        try
          Items.Text := Text;
        finally
          Free;
        end;

      cxCOBSelect.Properties.DropDownListStyle := lsFixedList;
      cxCOBSelect.ItemIndex := 0;

      if Execute then
        with SettingsManager.Settings.Layout do
        begin
          FindLayout(SelectedItem).Free;
          if SelectedItem = ActiveLayoutName then
            ActiveLayoutName := '';
        end;
    finally
      Free;
    end;
end;

procedure TMain.aOptionsExecute(Sender: TObject);
begin
  Settings.Show;
end;

procedure TMain.aAutoCompletionExecute(Sender: TObject);
begin
  fMain.CallAutoCompletion;
end;

procedure TMain.aCrypterCryptExecute(Sender: TObject);
begin
  fMain.CallCrypterCrypt;
end;

procedure TMain.aCrypterCheckExecute(Sender: TObject);
begin
  fMain.CallCrypterCheck;
end;

procedure TMain.aQuickBackupExecute(Sender: TObject);
begin
  fMain.CallBackupManager;
end;

procedure TMain.aPublishExecute(Sender: TObject);
begin
  fMain.CallPublish;
end;

procedure TMain.aPublishItemExecute(Sender: TObject);
begin
  fPublish.ExecuteActivePublishItem(TComponent(Sender).Tag);
end;

procedure TMain.aMirrorItemAddExecute(Sender: TObject);
begin
  fMain.ActiveTabSheetController.MirrorController.Add;
  fMain.CallControlAligner;
end;

procedure TMain.aSeriesAutoCompletionExecute(Sender: TObject);
begin
  fMain.CallSeriesAutoCompletion;
end;

procedure TMain.aSeriesCrypterCryptExecute(Sender: TObject);
begin
  fMain.CallSeriesCrypterCrypt;
end;

procedure TMain.aSeriesCrypterCheckExecute(Sender: TObject);
begin
  fMain.CallSeriesCrypterCheck;
end;

procedure TMain.aSeriesPublishExecute(Sender: TObject);
begin
  fMain.CallSeriesPublish;
end;

procedure TMain.aHelpFileExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', Homepage + 'help/', nil, nil, SW_SHOW);
end;

procedure TMain.aCheckforUpdatesExecute(Sender: TObject);
begin
  uUpdate.Update.Show;
end;

procedure TMain.aSupportBoardExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', Homepage + 'forum/', nil, nil, SW_SHOW);
end;

procedure TMain.aVisitBlogExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', Homepage + 'blog/', nil, nil, SW_SHOW);
end;

procedure TMain.aReportIssueExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', Homepage + 'report-issue/', nil, nil, SW_SHOW);
end;

procedure TMain.aAboutExecute(Sender: TObject);
begin
  if not Assigned(About) then
    Application.CreateForm(TAbout, About);
  About.Show;
end;
{$ENDREGION}

procedure TMain.dxBCTypeChange(Sender: TObject);
begin
  // Damit man nur neue Vorlagen erstellen kann wenn auch welche vorhanden sind
  aNew.Enabled := (dxBCType.ItemIndex <> -1);
  // fMain.pcMain.Properties.NewButton.Visible := (dxBCType.ItemIndex <> -1);

  SettingsManager.Settings.ActiveTypeName := dxBCType.Text;
end;

procedure TMain.dxBCTypeDropDown(Sender: TObject);
var
  StringList: TStrings;
begin
  StringList := GetTemplateList;
  try
    dxBCType.Items.Text := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TMain.cxBarEditItemEditorTypePropertiesChange(Sender: TObject);
var
  I: Integer;
begin
  SettingsManager.Settings.ActiveEditorTypeName := cxBarEditItemEditorType.EditValue;

  for I := 0 to length(FCodeDefinition.CodeTags) - 1 do
  begin
    FCodeDefinition.CodeTags[I].Params.Free;
    FCodeDefinition.CodeTags[I].ParamValues.Free;
  end;

  FCodeDefinition := TCodeDefinitions.GetCodeDefinition(cxBarEditItemEditorType.EditValue);
end;

procedure TMain.cxBarEditItemEditorTypePropertiesInitPopup(Sender: TObject);
begin
  with TCodeDefinitions.GetCodeDefinitions do
    try
      TcxCustomComboBoxProperties(cxBarEditItemEditorType.Properties).Items.Text := Text;
    finally
      Free;
    end;
end;

procedure TMain.cxBEILayoutPropertiesCloseUp(Sender: TObject);
begin
  if (TcxComboBoxAccess(Sender).FCloseUpReason = crEnter) and not(cxBEILayout.EditValue = '') then
    LoadLayout(SettingsManager.Settings.Layout.FindLayout(cxBEILayout.EditValue));
end;

procedure TMain.cxBEILayoutPropertiesInitPopup(Sender: TObject);
var
  StringList: TStrings;
begin
  StringList := SettingsManager.Settings.Layout.GetLayoutItemList;
  try
    with TcxComboBoxAccess(Sender).Properties do
      Items.Text := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  if (Msg.CmdType and $FFF0 = SC_RESTORE) or (Msg.CmdType and $FFF0 = SC_MAXIMIZE) then
  begin
    inherited;

  end
  else
    inherited;
end;

procedure TMain.WMDropFiles(var Msg: TWMDropFiles);

var
  s: array [0 .. 1023] of char;
  I, FileCount: Integer;
begin
  FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  for I := 0 to FileCount - 1 do
  begin
    DragQueryFile(Msg.Drop, I, s, sizeof(s));
    fMain.OpenToNewTab(s);
  end;
  DragFinish(Msg.Drop);
end;

procedure TMain.OneInstanceGetParams(Sender: TObject; const Args: array of string);
begin
  // AnalyzeStartupParams(Args);
end;

procedure TMain.LayoutClick(Sender: TObject);
begin
  LoadLayout(SettingsManager.Settings.Layout.FindLayout((Sender as TdxBarButton).Caption));
end;

procedure TMain.LayoutChanged(Sender: TObject);
var
  LIndex: Integer;

  StringList: TStrings;
  LBarButton: TdxBarButton;
begin
  with cxBEILayout do
    EditValue := SettingsManager.Settings.Layout.ActiveLayoutName;

  with nDesktop.ItemLinks do
    for LIndex := Count - 4 downto 0 do
      Items[LIndex].Free;

  StringList := SettingsManager.Settings.Layout.GetLayoutItemList;
  try
    aDeleteDesktop.Enabled := (StringList.Count > 0);

    for LIndex := 0 to StringList.Count - 1 do
    begin
      LBarButton := TdxBarButton.Create(nDesktop);
      with LBarButton do
      begin
        Index := LIndex;
        ButtonStyle := bsChecked;
        Caption := StringList[LIndex];
        if (StringList[LIndex] = SettingsManager.Settings.Layout.ActiveLayoutName) then
          Down := True;

        OnClick := LayoutClick;
      end;
      with nDesktop.ItemLinks.Add do
      begin
        Index := LBarButton.Index;
        Item := LBarButton;
      end;
    end;

  finally
    StringList.Free;
  end;
end;

procedure TMain.InsertTextBetweenSelected(TagName: string);

  function GetCodeTag(ATagName: string): TCodeTag;
  var
    I: Integer;
  begin
    for I := 0 to length(FCodeDefinition.CodeTags) - 1 do
      if SameText(ATagName, FCodeDefinition.CodeTags[I].Name) then
        Exit(FCodeDefinition.CodeTags[I]);
  end;

begin
  TTabSheetController(fMain.pcMain.ActivePage).DesignTabSheetItem.InsertTextBetweenSelected(GetCodeTag(TagName));
end;

function TMain.GetLogManager;
begin
  Result := TLogManager.Instance();
end;

function TMain.GetMainMenu;
begin
  Result := FMainMenu;
end;

function TMain.GetPageController;
begin
  Result := Main.fMain;
end;

function TMain.GetFileHosters: WideString;
begin
  Result := THosterConfiguration.GetHosters;
end;

function TMain.GetImageHosters: WideString;
var
  StringList: TStringList;
  I: Integer;
begin
  StringList := TStringList.Create;
  with StringList do
    try
      Add('OriginalValue');
      for I := 0 to SettingsManager.Settings.Plugins.ImageHoster.Count - 1 do
        Add(TPlugInCollectionItem(SettingsManager.Settings.Plugins.ImageHoster.Items[I]).name);
      Result := StringList.Text;
    finally
      Free;
    end;
end;

function TMain.GetCustomisedHoster(const AHoster: WideString; AShortName: WordBool = False): WideString;
begin
  Result := THosterConfiguration.GetCustomisedHoster(AHoster, AShortName);
end;

function TMain.GetControlValues(const ATypeID: TTypeID; const AComponentID: TControlID): WideString;
begin
  Result := SettingsManager.Settings.Controls.Controls[ATypeID, AComponentID].GetItems;
end;

procedure TMain.LoadLayout(ALayoutCollectionItem: TLayoutCollectionItem);
var
  _StringStream: TStringStream;
begin
  with ALayoutCollectionItem do
  begin
    _StringStream := TStringStream.Create(TFormat_MIME64.Decode(DockControls));
    try
      dxDockingManager.LoadLayoutFromStream(_StringStream);
    finally
      _StringStream.Free;
    end;

    _StringStream := TStringStream.Create(TFormat_MIME64.Decode(BarManager));
    try
      dxBarManager.LoadFromStream(_StringStream);
    finally
      _StringStream.Free;
    end;
  end;

  SettingsManager.Settings.Layout.ActiveLayoutName := ALayoutCollectionItem.name;
end;

procedure TMain.SaveLayout(ALayoutName: string);

var
  LayoutCollectionItem: TLayoutCollectionItem;
  _StringStream: TStringStream;
begin
  LayoutCollectionItem := SettingsManager.Settings.Layout.FindLayout(ALayoutName);

  if not Assigned(LayoutCollectionItem) then
    LayoutCollectionItem := TLayoutCollectionItem.Create(SettingsManager.Settings.Layout.Layout);

  with LayoutCollectionItem do
  begin
    name := ALayoutName;

    _StringStream := TStringStream.Create('');
    try
      dxDockingManager.SaveLayoutToStream(_StringStream);
      DockControls := TFormat_MIME64.Encode(_StringStream.DataString);
    finally
      _StringStream.Free;
    end;

    _StringStream := TStringStream.Create('');
    try
      dxBarManager.SaveToStream(_StringStream);
      BarManager := TFormat_MIME64.Encode(_StringStream.DataString);
    finally
      _StringStream.Free;
    end;
  end;

  SettingsManager.Settings.Layout.ActiveLayoutName := ALayoutName;
  SettingsManager.SaveSettings;
end;

procedure TMain.SetEditMenu(AMenuItems: TdxBarItemLinks);
begin
  if Assigned(AMenuItems) then
    nEdit.ItemLinks := AMenuItems
  else
    nEdit.ItemLinks.Clear;
end;

end.
