program IntelligeN;

{$R *.dres}

uses
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  ExceptionLog7,
  Forms,
  Windows,
  SysUtils,
  ShellAPI,
  Dialogs,
  uMain in 'forms\uMain.pas' {Main},
  uSettings in 'forms\uSettings.pas' {Settings},
  uAbout in 'forms\uAbout.pas' {About},
  uRegister in 'forms\uRegister.pas' {Register},
  uLicenceInfo in 'forms\uLicenceInfo.pas' {LicenceInfo},
  uUpdate in 'forms\uUpdate.pas' {Update},
  uCAPTCHA in 'forms\uCAPTCHA.pas' {CAPTCHA},
  uIntelligentPosting in 'forms\uIntelligentPosting.pas' {IntelligentPosting},
  uSelectDialog in 'forms\uSelectDialog.pas' { SelectDialog },
  uSelectFolderDialog in 'forms\uSelectFolderDialog.pas' {SelectFolderDialog},
  uNewDesignWindow in 'forms\uNewDesignWindow.pas' {NewDesignWindow },
  uApiBackupManager in 'api\uApiBackupManager.pas',
  uApiCodeTag in 'api\uApiCodeTag.pas',
  uApiConst in 'api\uApiConst.pas',
  uApiControlAligner in 'api\uApiControlAligner.pas',
  uApiControlController in 'api\uApiControlController.pas',
  uApiControlControllerBase in 'api\uApiControlControllerBase.pas',
  uApiControls in 'api\uApiControls.pas',
  uApiControlsBase in 'api\uApiControlsBase.pas',
  uApiCrawler in 'api\uApiCrawler.pas',
  uApiCrypter in 'api\uApiCrypter.pas',
  uApiDirectoryMonitor in 'api\uApiDirectoryMonitor.pas',
  uApiDLMF in 'api\uApiDLMF.pas',
  uApiFile in 'api\uApiFile.pas',
  uApiFileHoster in 'api\uApiFileHoster.pas',
  uApiHTTP in 'api\uApiHTTP.pas',
  uApiImageHoster in 'api\uApiImageHoster.pas',
  uApiIScriptFormatter in 'api\uApiIScriptFormatter.pas',
  uApiIScriptParser in 'api\uApiIScriptParser.pas',
  uApiMain in 'api\uApiMain.pas',
  uApiMainMenu in 'api\uApiMainMenu.pas',
  uApiMirrorControl in 'api\uApiMirrorControl.pas',
  uApiMirrorControlBase in 'api\uApiMirrorControlBase.pas',
  uApiMirrorController in 'api\uApiMirrorController.pas',
  uApiMirrorControllerBase in 'api\uApiMirrorControllerBase.pas',
  uApiMody in 'api\uApiMody.pas',
  uApiMultiCastEvent in 'api\uApiMultiCastEvent.pas',
  uApiPlugins in 'api\uApiPlugins.pas',
  uApiPluginsBase in 'api\uApiPluginsBase.pas',
  uApiPluginsAdd in 'api\uApiPluginsAdd.pas',
  uApiPrerequisite in 'api\uApiPrerequisite.pas',
  uApiPublishController in 'api\uApiPublishController.pas',
  uApiPublish in 'api\uApiPublish.pas',
  uApiSettings in 'api\uApiSettings.pas',
  uApiSettingsManager in 'api\uApiSettingsManager.pas',
  uApiSettingsExport in 'api\uApiSettingsExport.pas',
  uApiSettingsPluginsCheckListBox in 'api\uApiSettingsPluginsCheckListBox.pas',
  uApiTabSheetController in 'api\uApiTabSheetController.pas',
  uApiTabSheetData in 'api\uApiTabSheetData.pas',
  uApiTabSheetItem in 'api\uApiTabSheetItem.pas',
  uApiThreadPoolManager in 'api\uApiThreadPoolManager.pas',
  uApiUpdate in 'api\uApiUpdate.pas',
  uApiUpdateInterfaceBase in 'api\uApiUpdateInterfaceBase.pas',
  uApiUpdateModelBase in 'api\uApiUpdateModelBase.pas',
  uApiWebsiteEditor in 'api\uApiWebsiteEditor.pas',
  uApiXml in 'api\uApiXml.pas',
  uApiXmlSettings in 'api\uApiXmlSettings.pas',
  uAppInterface in '..\core\common\uAppInterface.pas',
  uBaseConst in '..\core\common\uBaseConst.pas',
  uAppConst in '..\core\common\uAppConst.pas',
  uFileInterface in '..\core\common\uFileInterface.pas',
  uBaseInterface in '..\core\common\uBaseInterface.pas',
  ufAddWebsiteWizard in 'frames\ufAddWebsiteWizard.pas' {fAddWebsiteWizard: TFrame},
  ufControlEditor in 'frames\ufControlEditor.pas' {fControlEditor: TFrame},
  ufDesigner in 'frames\ufDesigner.pas' {fDesigner: TFrame},
  ufDesignObjectInspector in 'frames\ufDesignObjectInspector.pas' {fDesignObjectInspector: TFrame},
  ufDatabase in 'frames\ufDatabase.pas' {fDatabase: TFrame},
  ufPublish in 'frames\ufPublish.pas' {fPublish: TFrame},
  ufPublishQueue in 'frames\ufPublishQueue.pas' {fPublishQueue: TFrame},
  ufIScriptDesigner in 'frames\ufIScriptDesigner.pas' {IScriptDesigner: TFrame},
  ufLinklist in 'frames\ufLinklist.pas' {fLinklist: TFrame},
  ufLogin in 'frames\ufLogin.pas' {fLogin: TFrame},
  ufMain in 'frames\ufMain.pas' {fMain: TFrame},
  ufHTTPLogger in 'frames\ufHTTPLogger.pas' {fHTTPLogger: TFrame},
  uMyAdvmJScriptStyler in 'mods\uMyAdvmJScriptStyler.pas',
  uMycxCheckComboBox in 'mods\uMycxCheckComboBox.pas',
  uMycxRichEdit in 'mods\uMycxRichEdit.pas',
  uMyPopupMenu in 'mods\uMyPopupMenu.pas',
  uMydxBarPopupMenu in 'mods\uMydxBarPopupMenu.pas',
  uMyTMonospaceHint in 'mods\uMyTMonospaceHint.pas',
  uExport in '..\sdk\dlls\uExport.pas',
  uPlugInInterface in '..\sdk\plugins\uPlugInInterface.pas',
  uPlugInInterfaceAdv in '..\sdk\plugins\uPlugInInterfaceAdv.pas',
  uPlugInClass in '..\sdk\plugins\uPlugInClass.pas',
  uPlugInConst in '..\sdk\plugins\uPlugInConst.pas',
  uPlugInEvent in '..\sdk\plugins\uPlugInEvent.pas',
  uPlugInHTTPClasses in '..\sdk\plugins\uPlugInHTTPClasses.pas',
  uPlugInAppClass in '..\sdk\plugins\app\uPlugInAppClass.pas',
  uPlugInCAPTCHAClass in '..\sdk\plugins\captcha\uPlugInCAPTCHAClass.pas',
  uPlugInCMSClass in '..\sdk\plugins\cms\uPlugInCMSClass.pas',
  uPlugInCMSBoardClass in '..\sdk\plugins\cms\uPlugInCMSBoardClass.pas',
  uPlugInCMSSettingsHelper in '..\sdk\plugins\cms\uPlugInCMSSettingsHelper.pas',
  uPlugInCMSFormbasedClass in '..\sdk\plugins\cms\uPlugInCMSFormbasedClass.pas',
  uPlugInCMSBlogClass in '..\sdk\plugins\cms\uPlugInCMSBlogClass.pas',
  uPlugInCrawlerClass in '..\sdk\plugins\crawler\uPlugInCrawlerClass.pas',
  uPlugInCrypterClass in '..\sdk\plugins\crypter\uPlugInCrypterClass.pas',
  uPlugInFileFormatClass in '..\sdk\plugins\fileformats\uPlugInFileFormatClass.pas',
  uPlugInFileHosterClass in '..\sdk\plugins\filehoster\uPlugInFileHosterClass.pas',
  uPlugInImageHosterClass in '..\sdk\plugins\imagehoster\uPlugInImageHosterClass.pas',
  uFileUtils in '..\core\utils\uFileUtils.pas',
  uHTMLUtils in '..\core\utils\uHTMLUtils.pas',
  uImageUtils in '..\core\utils\uImageUtils.pas',
  uPathUtils in '..\core\utils\uPathUtils.pas',
  uReleasenameUtils in '..\core\utils\uReleasenameUtils.pas',
  uSetUtils in '..\core\utils\uSetUtils.pas',
  uSizeUtils in '..\core\utils\uSizeUtils.pas',
  uStringUtils in '..\core\utils\uStringUtils.pas',
  uVariantUtils in '..\core\utils\uVariantUtils.pas',
  IntelligeN_TLB in 'ole\IntelligeN_TLB.pas',
  uOLE in 'ole\uOLE.pas' {IntelligeN2009: CoClass};

{$R *.res}

{$R *.TLB}

begin
  if DirectoryExists(GetHiddenDataDir + 'update') then
  begin
    if FileExists(GetHiddenDataDir + 'update\sleep32.exe') then
    begin
      ShellExecute(0, 'open', PChar(GetHiddenDataDir + 'update\exec_update.bat'), nil, PChar(GetHiddenDataDir + 'update'), SW_SHOW);
      Exit; // Application.Terminate funktioniert hier noch nicht, Programm würde weitermachen
    end;
    DeleteFile(GetHiddenDataDir + 'update');
  end;

  Randomize;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := ProgrammName;
{$IFDEF DEBUG}
  UseLatestCommonDialogs := False;
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'IntelligeN.chm';
  if GenerateFolderSystem then
  begin
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TSettings, Settings);
  Application.CreateForm(TUpdate, Update);
  AnalyzeStartupParams;
    if SettingsManager.Settings.Login.AutoLogin then
      Main.fLogin.cxbLoginClick(nil);
    if Assigned(SettingsManager.Settings.Layout.ActiveLayout) then
      with Main do
      begin
        LoadLayout(SettingsManager.Settings.Layout.ActiveLayout);
        Width := Width + 1;
        Width := Width - 1;
      end;
    Application.Run;
  end;

end.
