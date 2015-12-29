unit uApiPlugins;

interface

uses
  // Delphi
  Windows, Forms, SysUtils, Classes, Controls, Math, Graphics,
  // Spring Framework
  Spring.SystemUtils,
  // OmniThreadLibrary
  OtlCommon, OtlTask,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiPluginsBase, uApiHTTP, uApiSettings, uApiWebsiteEditor, uApiXml,
  // HTTPManager
  uHTTPManager,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInInterfaceAdv,
  // Utils
  uPathUtils, uURLUtils;

type
  TApiCAPTCHA = class
  protected
    class function GetCAPTCHAType(const ACAPTCHA: WideString): TCAPTCHAType;
  public
    class function PluginsHandle(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString;
      AErrorProc: TPluginErrorProc = nil): WordBool;
    class function ManualHandle(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString;
      ACAPTCHAMemoryStream: TMemoryStream; AMaxWaitMs: Cardinal = INFINITE): WordBool;
    class function DefaultHandler(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString): WordBool; safecall;
  end;

  TCAPTCHAPluginProc = reference to procedure(var ACAPTCHAPlugin: ICAPTCHAPlugIn);
  TCMSPluginProc = reference to procedure(var ACMSPlugin: ICMSPlugIn);
  TCrawlerPluginProc = reference to procedure(var ACrawlerPlugin: ICrawlerPlugIn);
  TCrypterPluginProc = reference to procedure(var ACrypterPlugin: ICrypterPlugIn);
  TFileHosterPluginProc = reference to procedure(var AFileHosterPlugin: IFileHosterPlugIn);
  TFileFormatPluginProc = reference to procedure(var AFileFormatPlugin: IFileFormatPlugIn);
  TImageHosterPluginProc = reference to procedure(var AImageHosterPlugin: IImageHosterPlugIn);

  TPluginBasic = class(TPluginBase)
  protected
    class procedure InitializePlugin(const APlugin: IPlugIn);
    class procedure UninitializePlugin(const APlugin: IPlugIn);

    class function RelativToAbsolutePath(const ARelativPluginPath: string): string;

    class function PluginTypeToString(const APluginType: TPlugInType): string;

    class function IncorrectPluginTypeErrorMsg(const ARelativPluginPath: string; const APlugin: IPlugIn; AExpectedPlugin: TPlugInType): string;

    class procedure LoadPlugin(const ARelativPluginPath: string; APluginProc: TPluginProc; AErrorProc: TPluginErrorProc = nil);

    class procedure LoadCAPTCHAPlugin(const ARelativPluginPath: string; ACAPTCHAPluginProc: TCAPTCHAPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadCMSPlugin(const ARelativPluginPath: string; ACMSPluginProc: TCMSPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadCrawlerPlugin(const ARelativPluginPath: string; ACrawlerPluginProc: TCrawlerPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadCrypterPlugin(const ARelativPluginPath: string; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadFileHosterPlugin(const ARelativPluginPath: string; AFileHosterPluginProc: TFileHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadFileFormatsPlugin(const ARelativPluginPath: string; AFileFormatPluginProc: TFileFormatPluginProc; AErrorProc: TPluginErrorProc = nil);
    class procedure LoadImageHosterPlugin(const ARelativPluginPath: string; AImageHosterPluginProc: TImageHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
  public
    class function AppLoad(App: TAppCollectionItem; const AAppController: IAppController): Boolean;
    class function AppUnLoad(App: TAppCollectionItem): Boolean;

    class function CAPTCHAExec(const ACAPTCHAPluginPath: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString;
      AErrorProc: TPluginErrorProc = nil): WordBool;

    class function CMSDefaultCharset(const ARelativPluginPath: string): string;
    class function CMSBelongsTo(const ARelativPluginPath, AWebsiteSourceCode: string): Boolean;
    class function CMSShowWebsiteSettingsEditor(const ARelativPluginPath: string; ACMSWebsite: TCMSWebsitesCollectionItem; const AAppController: IAppController): Boolean;

    class function GetSaveFileFormats: TStrings;
    class function GetLoadFileFormats: TStrings;
    class procedure SaveFile(AFileFormats: TPlugInCollectionItem; const AFileName, ATemplateFileName: string; const ATabSheetController: ITabSheetController);
    class function LoadFile(const AFileName: string; const APageController: IPageController): Boolean;
  end;

  TApiThreadedPlugin = class(TPluginBasic)
  private
    FTask: IOmniTask;
    FErrorHandler: TPluginErrorProc;
  protected
    procedure DefaultInternalErrorHandler(const AErrorMsg: string);

    function DefaultCAPTCHAInputHandler(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString): WordBool; safecall;
    function DefaultIntelligentPostingHelperHandler(const AWebsite: WideString; const ASubject: WideString; var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool; safecall;

    procedure LoadCrypterPlugin(ACrypter: TCrypterCollectionItem; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TPluginErrorProc = nil);
    procedure LoadImageHosterPlugin(AImageHoster: TImageHosterCollectionItem; AImageHosterPluginProc: TImageHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
  public
    constructor Create(const ATask: IOmniTask; AErrorHandler: TPluginErrorProc = nil);
    destructor Destroy(); override;

    function AddArticle(const APublishItem: IPublishItem; ACAPTCHAInput: TCAPTCHAInput = nil; AIntelligentPostingHandler: TIntelligentPostingHelper = nil): Boolean;

    function CrawlerExec(ACrawler: TCrawlerCollectionItem; ATypeID: TTypeID; const AControlController: IControlControllerBase): Boolean;

    function CrypterAddFolder(ACrypter: TCrypterCollectionItem; const AMirrorContainer: IDirectlinkContainer; const AControlController: IControlControllerBase; out AFolderInfo: TCrypterFolderInfo): Boolean;
    function CrypterGetFolder(ACrypter: TCrypterCollectionItem; AFolderIdentifier: string; out AFolderInfo: TCrypterFolderInfo): Boolean;

    function FileHosterCheckFiles(AFileHoster: TPlugInCollectionItem; const ALinks: string; out ALinksInfo: TLinksInfo): Boolean;

    function ImageHosterLocalUpload(AImageHoster: TImageHosterCollectionItem; ALocalPath: string; out AUrl: WideString; ACAPTCHAInput: TCAPTCHAInput = nil): Boolean;
    function ImageHosterRemoteUpload(AImageHoster: TImageHosterCollectionItem; ARemoteUrl: string; out AUrl: WideString; ACAPTCHAInput: TCAPTCHAInput = nil): Boolean;
  end;

implementation

uses
  uCAPTCHA,
  uIntelligentPosting;

{ TApiCAPTCHAClass }

class function TApiCAPTCHA.GetCAPTCHAType(const ACAPTCHA: WideString): TCAPTCHAType;
begin
  if BeginsWithHTTP(ACAPTCHA) then
    Result := ctImage
  else
    Result := ctText;
end;

class function TApiCAPTCHA.PluginsHandle(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString;
  var ACookies: WideString; AErrorProc: TPluginErrorProc = nil): WordBool;
var
  LHandled: WordBool;
  LCAPTCHAPluginIndex: Integer;
  LCookies, LCAPTCHASolution: WideString;
begin
  LHandled := False;
  ACAPTCHASolution := '';

  for LCAPTCHAPluginIndex := 0 to SettingsManager.Settings.Plugins.CAPTCHA.Count - 1 do
    if TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[LCAPTCHAPluginIndex]).Enabled then
    begin
      LCookies := ACookies;
      LHandled := TPluginBasic.CAPTCHAExec(TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[LCAPTCHAPluginIndex]).Path, ACAPTCHAType, ACAPTCHA, ACAPTCHAName, LCAPTCHASolution, LCookies, AErrorProc);
      if LHandled then
      begin
        ACAPTCHASolution := LCAPTCHASolution;
        ACookies := LCookies;
        break;
      end;
    end;

  Result := LHandled;
end;

class function TApiCAPTCHA.ManualHandle(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString;
  ACAPTCHAMemoryStream: TMemoryStream; AMaxWaitMs: Cardinal = INFINITE): WordBool;
begin
  Result := False;

  with TCAPTCHA.Create(nil, AMaxWaitMs) do
    try
      Caption := ExtractUrlHost(AWebsite) + ' - ' + Caption + ' - ' + ASubject;

      if (ACAPTCHAType = ctImage) then
        DisplayCAPTCHAImage(ACAPTCHAMemoryStream)
      else
        DisplayCAPTCHAText(ACAPTCHA);

      if (ShowModal = mrOk) then
      begin
        ACAPTCHASolution := CAPTCHA;
        Result := True;
      end;
    finally
      Free;
    end;
end;

class function TApiCAPTCHA.DefaultHandler(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString): WordBool;
var
  LResult: WordBool;
  LCAPTCHAType: TCAPTCHAType;
  LCookies, LCAPTCHASolution: WideString;

  LCAPTCHAMemoryStream: TMemoryStream;
begin
  LCAPTCHAType := TApiCAPTCHA.GetCAPTCHAType(ACAPTCHA);
  ACAPTCHASolution := '';

  LCookies := ACookies;

  LResult := TApiCAPTCHA.PluginsHandle(AWebsite, ASubject, LCAPTCHAType, ACAPTCHA, ACAPTCHAName, ACAPTCHASolution, LCookies);

  if LResult then
  begin
    ACAPTCHASolution := LCAPTCHASolution;
    ACookies := LCookies;
  end
  else if not LResult then
  begin
    ACAPTCHASolution := '';

    try
      if (LCAPTCHAType = ctImage) then
      begin
        LCookies := ACookies;

        TApiHTTP.DownloadData(ACAPTCHA, LCAPTCHAMemoryStream, LCookies,
          // TODO: Read settings at a thread-safe position
          { . } SettingsManager.Settings.HTTP.GetProxy(psaMain), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
      end;

      LResult := TApiCAPTCHA.ManualHandle(AWebsite, ASubject, LCAPTCHAType, ACAPTCHA, ACAPTCHAName, ACAPTCHASolution, LCAPTCHAMemoryStream, 1000 * 60 * 2);
    finally
      if (LCAPTCHAType = ctImage) then
      begin
        LCAPTCHAMemoryStream.Free;
      end;
    end;

    if LResult then
    begin
      ACAPTCHASolution := LCAPTCHASolution;
      ACookies := LCookies;
    end;
  end;

  Result := LResult;
end;

{ TPluginBasic }

class procedure TPluginBasic.InitializePlugin(const APlugin: IPlugIn);
begin
  APlugin.SetHTTPManager(THTTPManager.Instance());

  case APlugin.GetType of
    ptApp, ptCAPTCHA, ptFileFormats:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaMain);
    ptCMS:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCMS);
    ptCrawler:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCMS);
    ptCrypter:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCrypter);
    ptFileHoster:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaFileHoster);
    ptImageHoster:
      APlugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaImageHoster);
  end;

  APlugin.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
  APlugin.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;
end;

class procedure TPluginBasic.UninitializePlugin(const APlugin: IPlugIn);
begin
  APlugin.SetHTTPManager(nil);
end;

class function TPluginBasic.RelativToAbsolutePath(const ARelativPluginPath: string): string;
begin
  Result := PathCombineEx(GetPluginFolder, ARelativPluginPath);
end;

class function TPluginBasic.PluginTypeToString(const APluginType: TPlugInType): string;
begin
  Result := copy(TEnum.GetName<TPlugInType>(APluginType), 3, MaxInt);
end;

class function TPluginBasic.IncorrectPluginTypeErrorMsg(const ARelativPluginPath: string; const APlugin: IPlugIn; AExpectedPlugin: TPlugInType): string;
var
  LPluginFileName: string;
  LExpectedPluginType: string;
  LFoundPluginType: string;
begin
  LPluginFileName := ExtractFileName(ARelativPluginPath);
  LExpectedPluginType := TPluginBasic.PluginTypeToString(AExpectedPlugin);
  LFoundPluginType := TPluginBasic.PluginTypeToString(APlugin.GetType);

  Result := Format(StrPluginIncorrectType, [LExpectedPluginType, LFoundPluginType, LPluginFileName]);
end;

class procedure TPluginBasic.LoadPlugin(const ARelativPluginPath: string; APluginProc: TPluginProc; AErrorProc: TPluginErrorProc);
var
  LPluginHandle: Cardinal;
  LPluginFileName: string;
begin
  LPluginFileName := TPluginBasic.RelativToAbsolutePath(ARelativPluginPath);
  TPluginBasic.LoadPluginBase(LPluginFileName, LPluginHandle,
    { } procedure(var APlugin: IPlugIn)
    { } begin
    { . } TPluginBasic.InitializePlugin(APlugin);
    { . } try
    { ... } APluginProc(APlugin);
    { . } finally
    { ... } TPluginBasic.UninitializePlugin(APlugin);
    { . } end;
    { } end, True, AErrorProc);
end;

class procedure TPluginBasic.LoadCAPTCHAPlugin(const ARelativPluginPath: string; ACAPTCHAPluginProc: TCAPTCHAPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LCAPTCHAPlugin: ICAPTCHAPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICAPTCHAPlugIn, LCAPTCHAPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } ACAPTCHAPluginProc(LCAPTCHAPlugin);
    { ... } finally
    { ..... } LCAPTCHAPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptCAPTCHA), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadCMSPlugin(const ARelativPluginPath: string; ACMSPluginProc: TCMSPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LCMSPlugin: ICMSPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICMSPlugIn, LCMSPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } ACMSPluginProc(LCMSPlugin);
    { ... } finally
    { ..... } LCMSPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptCMS), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadCrawlerPlugin(const ARelativPluginPath: string; ACrawlerPluginProc: TCrawlerPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LCrawlerPlugin: ICrawlerPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICrawlerPlugIn, LCrawlerPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } ACrawlerPluginProc(LCrawlerPlugin);
    { ... } finally
    { ..... } LCrawlerPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptCrawler), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadCrypterPlugin(const ARelativPluginPath: string; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LCrypterPlugin: ICrypterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICrypterPlugIn, LCrypterPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } ACrypterPluginProc(LCrypterPlugin);
    { ... } finally
    { ..... } LCrypterPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptCrypter), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadFileHosterPlugin(const ARelativPluginPath: string; AFileHosterPluginProc: TFileHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LFileHosterPlugin: IFileHosterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IFileHosterPlugIn, LFileHosterPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } AFileHosterPluginProc(LFileHosterPlugin);
    { ... } finally
    { ..... } LFileHosterPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptFileHoster), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadFileFormatsPlugin(const ARelativPluginPath: string; AFileFormatPluginProc: TFileFormatPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LFileFormatPlugin: IFileFormatPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IFileFormatPlugIn, LFileFormatPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } AFileFormatPluginProc(LFileFormatPlugin);
    { ... } finally
    { ..... } LFileFormatPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptFileFormats), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class procedure TPluginBasic.LoadImageHosterPlugin(const ARelativPluginPath: string; AImageHosterPluginProc: TImageHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LImageHosterPlugin: IImageHosterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IImageHosterPlugIn, LImageHosterPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } AImageHosterPluginProc(LImageHosterPlugin);
    { ... } finally
    { ..... } LImageHosterPlugin := nil;
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(TPluginBasic.IncorrectPluginTypeErrorMsg(ARelativPluginPath, APlugin, ptImageHoster), AErrorProc);
    { . } end;
    { } end, AErrorProc);
end;

class function TPluginBasic.AppLoad(App: TAppCollectionItem; const AAppController: IAppController): Boolean;
var
  LLibraryHandle: Cardinal;
  LPluginFile: string;
  LResult, LHighException, LDoUnload: Boolean;
begin
  LResult := False;
  LHighException := False;
  LDoUnload := False;
  LPluginFile := TPluginBasic.RelativToAbsolutePath(App.Path);

  TPluginBasic.LoadPluginBase(LPluginFile, LLibraryHandle,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LAppPlugin: IAppPlugin;
    { } begin
    { . } App.LibraryID := LLibraryHandle;
    { . } TPluginBasic.InitializePlugin(APlugin);
    { . } if APlugin.QueryInterface(IAppPlugin, LAppPlugin) = 0 then
    { . } begin
    { ... } try
    { ..... } try
    { ....... } LResult := LAppPlugin.Start(AAppController);
    { ..... } except
    { ....... } LHighException := True;
    { ....... } LDoUnload := True;
    { ....... } LAppPlugin.Stop;
    { ....... } TPluginBasic.UninitializePlugin(LAppPlugin);
    { ....... } LAppPlugin := nil;
    { ..... } end;
    { ... } except
    { ..... } LAppPlugin := nil;
    { ..... } if LDoUnload then
    { ..... } begin
    { ....... } TPluginBasic.UnLoadPluginBase(LLibraryHandle);
    { ..... } end;
    { ... } end;

    { ... } if LResult then
    { ... } begin
    { ..... } App.Plugin := LAppPlugin;
    { ... } end
    { ... } else
    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.UninitializePlugin(LAppPlugin);
    { ..... } LAppPlugin := nil;
    { ..... } TPluginBasic.ReturnError(Format(StrPluginCouldNotLoad, [App.Name, ExtractFileName(LPluginFile)]));
    { ... } end
    { ... } else
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(LPluginFile)]));
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TPluginBasic.ReturnError(IncorrectPluginTypeErrorMsg(App.Path, APlugin, ptApp));
    { . } end;
    { } end, False);

  Result := LResult;
end;

class function TPluginBasic.AppUnLoad(App: TAppCollectionItem): Boolean;
var
  LLibraryHandle: Cardinal;
  LDoUnload: Boolean;
begin
  Result := False;

  LDoUnload := True;
  LLibraryHandle := App.LibraryID;
  if not(LLibraryHandle = 0) then
  begin
    try
      LDoUnload := App.Plugin.Stop;
      if LDoUnload then
        TPluginBasic.UninitializePlugin(App.Plugin)
      else
        TPluginBasic.ReturnError(Format(StrPluginCanNotUnLoad, [App.Name, ExtractFileName(App.Path)]));
    finally
      if LDoUnload then
       App.Plugin := nil;
    end;
  end;
  if LDoUnload then
  begin
    TPluginBasic.UnLoadPluginBase(LLibraryHandle);
    result := True;
  end;
end;

class function TPluginBasic.CAPTCHAExec(const ACAPTCHAPluginPath: WideString; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString;
  AErrorProc: TPluginErrorProc = nil): WordBool;
var
  LResult, LHighException: WordBool;
  LCookies, LCAPTCHASolution: WideString;
begin
  LResult := False;
  LHighException := False;

  LCAPTCHASolution := '';
  LCookies := ACookies;

  TPluginBasic.LoadCAPTCHAPlugin(ACAPTCHAPluginPath,
    { } procedure(var ACAPTCHAPlugin: ICAPTCHAPlugIn)
    { } begin
    { . } ACAPTCHAPlugin.CAPTCHA := ACAPTCHA;
    { . } ACAPTCHAPlugin.CAPTCHAType := ACAPTCHAType;
    { . } ACAPTCHAPlugin.CAPTCHAName := ACAPTCHAName;
    { . } ACAPTCHAPlugin.Cookies := LCookies;

    { . } try
    { ... } LResult := ACAPTCHAPlugin.Exec;
    { . } except
    { ... } LHighException := True;
    { . } end;

    { . } if LResult then
    { . } begin
    { ... } LCookies := ACAPTCHAPlugin.Cookies;
    { ... } LCAPTCHASolution := ACAPTCHAPlugin.CAPTCHAResult;
    { . } end
    { . } else
    { . } if not LResult and not LHighException then
    { . } begin
    { ... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ACAPTCHAPlugin.ErrorMsg, ExtractFileName(ACAPTCHAPluginPath)]), AErrorProc);
    { . } end
    { . } else
    { . } if LHighException then
    { . } begin
    { ... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(ACAPTCHAPluginPath)]), AErrorProc);
    { . } end;
    { } end, AErrorProc);

  ACAPTCHASolution := LCAPTCHASolution;
  ACookies := LCookies;
  Result := LResult;
end;

class function TPluginBasic.CMSDefaultCharset(const ARelativPluginPath: string): string;
var
  LResult: string;
begin
  LResult := '';
  TPluginBasic.LoadCMSPlugin(ARelativPluginPath,
    { } procedure(var ACMSPlugin: ICMSPlugIn)
    { } begin
    { . } LResult := ACMSPlugin.DefaultCharset;
    { } end);
  Result := LResult;
end;

class function TPluginBasic.CMSBelongsTo(const ARelativPluginPath, AWebsiteSourceCode: string): Boolean;
var
  LResult: Boolean;
begin
  LResult := False;
  TPluginBasic.LoadCMSPlugin(ARelativPluginPath,
    { } procedure(var ACMSPlugin: ICMSPlugIn)
    { } begin
    { . } LResult := ACMSPlugin.BelongsTo(AWebsiteSourceCode);
    { } end);
  Result := LResult;
end;

class function TPluginBasic.CMSShowWebsiteSettingsEditor(const ARelativPluginPath: string; ACMSWebsite: TCMSWebsitesCollectionItem; const AAppController: IAppController): Boolean;
var
  LResult: Boolean;
begin
  LResult := False;
  TPluginBasic.LoadCMSPlugin(ARelativPluginPath,
    { } procedure(var ACMSPlugin: ICMSPlugIn)
    { } var
    { . } LWebsiteEditor: TBasisWebsiteEditor;
    { } begin
    { . } with ACMSPlugin do
    { . } begin
    { ... } SetCAPTCHAInput(TApiCAPTCHA.DefaultHandler);

    { ... } Accountname := ACMSWebsite.Accountname;
    { ... } Accountpassword := ACMSWebsite.Accountpassword;

    { ... } SettingsFileName := ACMSWebsite.GetPath;

    { ... } Website := ACMSWebsite.Name;

    { ... } LWebsiteEditor := TWebsiteEditorFactory.GetClassType(ACMSPlugin.CMSType).Create(ACMSPlugin, AAppController, ACMSWebsite.GetPath);
    { ... } try
    { ..... } LResult := ShowWebsiteSettingsEditor(LWebsiteEditor);
    { ... } finally
    { ..... } LWebsiteEditor.Free;
    { ... } end;
    { . } end;
    { } end);
  Result := LResult;
end;

class function TPluginBasic.GetSaveFileFormats: TStrings;
var
  LResult: TStringList;
  LFileFormatCollectionIndex: Integer;
begin
  LResult := TStringList.Create;

  with SettingsManager.Settings.Plugins.FileFormats do
    for LFileFormatCollectionIndex := 0 to Count - 1 do
      with TFileFormatsCollectionItem(Items[LFileFormatCollectionIndex]) do
        if Enabled then
        begin
          TPluginBasic.LoadFileFormatsPlugin(Path,
            { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
            { } begin
            { . } if AFileFormatPlugin.CanSaveControls then
            { ... } LResult.Add(AFileFormatPlugin.GetName + LResult.NameValueSeparator + Format(AFileFormatPlugin.GetFileFormatName, [StrDocument]));
            { } end);
        end;

  Result := LResult;
end;

class function TPluginBasic.GetLoadFileFormats: TStrings;
var
  LResult: TStringList;
  LFileFormatCollectionIndex: Integer;
begin
  LResult := TStringList.Create;

  with SettingsManager.Settings.Plugins.FileFormats do
    for LFileFormatCollectionIndex := 0 to Count - 1 do
      with TFileFormatsCollectionItem(Items[LFileFormatCollectionIndex]) do
        if Enabled then
        begin
          TPluginBasic.LoadFileFormatsPlugin(Path,
            { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
            { } begin
            { . } if AFileFormatPlugin.CanLoadControls then
            { ... } LResult.Add(AFileFormatPlugin.GetName + LResult.NameValueSeparator + Format(AFileFormatPlugin.GetFileFormatName, [StrDocument]));
            { } end);
        end;

  Result := LResult;
end;

class procedure TPluginBasic.SaveFile(AFileFormats: TPlugInCollectionItem; const AFileName, ATemplateFileName: string; const ATabSheetController: ITabSheetController);
begin
  TPluginBasic.LoadFileFormatsPlugin(AFileFormats.Path,
    { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
    { } begin
    { . } if AFileFormatPlugin.CanSaveControls then
    { ... } AFileFormatPlugin.SaveControls(AFileName, ATemplateFileName, ATabSheetController)
    { } end);
end;

class function TPluginBasic.LoadFile(const AFileName: string; const APageController: IPageController): Boolean;
var
  LFileFormatCollectionIndex, LTabIndex: Integer;
  LHandled: Boolean;
begin
  LHandled := False;

  with SettingsManager.Settings.Plugins.FileFormats do
    for LFileFormatCollectionIndex := 0 to Count - 1 do
      if not LHandled then
        with TFileFormatsCollectionItem(Items[LFileFormatCollectionIndex]) do
          if Enabled then
            TPluginBasic.LoadFileFormatsPlugin(Path,
              { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
              { } begin
              { . } if AFileFormatPlugin.CanLoadControls then
              { . } begin
              { ... } try
              { ..... } AFileFormatPlugin.ForceAddCrypter := ForceAddCrypter;
              { ..... } AFileFormatPlugin.ForceAddImageMirror := ForceAddImageMirror;
              { ..... } LTabIndex := AFileFormatPlugin.LoadControls(AFileName, GetTemplatesTypeFolder, APageController);
              { ..... } LHandled := not (LTabIndex = -1);
              { ..... } if LHandled then
              { ....... } with APageController.TabSheetController[LTabIndex] do
              { ....... } begin
              { ......... } Application.ProcessMessages; // TODO: remove this line
              { ......... } Initialized(AFileName, AFileFormatPlugin.GetName);
              { ....... } end;
              { ... } except

              { ... } end;
              { . } end;
              { } end);

  Result := LHandled;
end;

procedure TApiThreadedPlugin.DefaultInternalErrorHandler(const AErrorMsg: string);
begin
  FTask.Invoke(
    { } procedure
    { } begin
    { ... } raise EIntelligeNPluginException.Create(AErrorMsg);
    { } end);
end;

function TApiThreadedPlugin.DefaultCAPTCHAInputHandler(const AWebsite: WideString; const ASubject: WideString; const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString): WordBool;
var
  LResult: WordBool;
  LCAPTCHAType: TCAPTCHAType;

  LCAPTCHASolution, LCAPTCHACookies: WideString;
  LCAPTCHAWaitable: IOmniWaitableValue;

  LCAPTCHAMemoryStream: TMemoryStream;
begin
  LCAPTCHAType := TApiCAPTCHA.GetCAPTCHAType(ACAPTCHA);

  LCAPTCHASolution := '';
  LCAPTCHACookies := ACookies;

  LResult := TApiCAPTCHA.PluginsHandle(AWebsite, ASubject, LCAPTCHAType, ACAPTCHA, ACAPTCHAName, ACAPTCHASolution, LCAPTCHACookies, FErrorHandler);

  if LResult then
  begin
    ACAPTCHASolution := LCAPTCHACookies;
    ACookies := LCAPTCHACookies;
  end
  else if not LResult then
  begin
    LCAPTCHAWaitable := CreateWaitableValue;

    LCAPTCHASolution := '';

    try
      if (LCAPTCHAType = ctImage) then
      begin
        LCAPTCHACookies := ACookies;
        TApiHTTP.DownloadData(ACAPTCHA, LCAPTCHAMemoryStream, LCAPTCHACookies,
          // TODO: Read settings at a thread-safe position
          { . } SettingsManager.Settings.HTTP.GetProxy(psaMain), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
      end;

      FTask.Invoke(
        { } procedure
        { } begin
        { . } LResult := TApiCAPTCHA.ManualHandle(AWebsite, ASubject, LCAPTCHAType, ACAPTCHA, ACAPTCHAName, LCAPTCHASolution, LCAPTCHAMemoryStream, 1000 * 60 * 2); // wait max. 2 minutes
        { . } LCAPTCHAWaitable.Signal;
        { } end);

      LCAPTCHAWaitable.WaitFor(1000 * 60 * 2 + 15000); // wait max. 2 minutes + 15 seconds

      if LResult then
      begin
        ACAPTCHASolution := LCAPTCHASolution;
        ACookies := LCAPTCHACookies;
      end;

      LCAPTCHAWaitable := nil;
    finally
      if (LCAPTCHAType = ctImage) then
      begin
        LCAPTCHAMemoryStream.Free;
      end;
    end;
  end;

  Result := LResult;
end;

function TApiThreadedPlugin.DefaultIntelligentPostingHelperHandler(const AWebsite: WideString; const ASubject: WideString; var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool;
var
  LResult: WordBool;

  LHost: WideString;
  LIntelligentPostingSearchValue, LIntelligentPostingSearchResults: WideString;
  LIntelligentPostingSearchIndex: Integer;
  LIntelligentPostingRedoSearch: WordBool;

  LIntelligentPostingHelperWaitable: IOmniWaitableValue;
begin
  LIntelligentPostingHelperWaitable := CreateWaitableValue;

  LHost := ExtractUrlHost(AWebsite);

  LIntelligentPostingSearchValue := ASearchValue;
  LIntelligentPostingSearchResults := ASearchResults;
  LIntelligentPostingSearchIndex := ASearchIndex;

  FTask.Invoke(
    { } procedure
    { } begin
    { . } LResult := TIntelligentPostingClass.IntelligentPostingHandler(LHost, ASubject, LIntelligentPostingSearchValue, LIntelligentPostingSearchResults, LIntelligentPostingSearchIndex, LIntelligentPostingRedoSearch);
    { . } LIntelligentPostingHelperWaitable.Signal;
    { } end);

  LIntelligentPostingHelperWaitable.WaitFor();

  if LResult then
  begin
    ASearchValue := LIntelligentPostingSearchValue;
    ASearchIndex := LIntelligentPostingSearchIndex;
  end;
  ARedoSearch := LIntelligentPostingRedoSearch;

  LIntelligentPostingHelperWaitable := nil;

  Result := LResult;
end;

procedure TApiThreadedPlugin.LoadCrypterPlugin(ACrypter: TCrypterCollectionItem; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadCrypterPlugin(ACrypter.Path,
    { } procedure(var ACrypterPlugin: ICrypterPlugIn)
    { } begin
    { . } with ACrypterPlugin do
    { . } begin
    { ... } UseAccount := ACrypter.UseAccount;
    { ... } if ACrypter.UseAccount then
    { ... } begin
    { ..... } Accountname := ACrypter.Accountname;
    { ..... } Accountpassword := ACrypter.Accountpassword;
    { ... } end
    { ... } else
    { ... } begin
    { ..... } Accountname := '';
    { ..... } Accountpassword := '';
    { ... } end;

    { ... } ACrypterPluginProc(ACrypterPlugin);
    { . } end;
    { } end, AErrorProc);
end;

procedure TApiThreadedPlugin.LoadImageHosterPlugin(AImageHoster: TImageHosterCollectionItem; AImageHosterPluginProc: TImageHosterPluginProc; AErrorProc: TPluginErrorProc = nil);
begin
  TPluginBasic.LoadImageHosterPlugin(AImageHoster.Path,
    { } procedure(var AImageHosterPlugin: IImageHosterPlugIn)
    { } begin
    { . } with AImageHosterPlugin do
    { . } begin
    { ... } UseAccount := AImageHoster.UseAccount;
    { ... } if AImageHoster.UseAccount then
    { ... } begin
    { ..... } Accountname := AImageHoster.Accountname;
    { ..... } Accountpassword := AImageHoster.Accountpassword;
    { ... } end
    { ... } else
    { ... } begin
    { ..... } Accountname := '';
    { ..... } Accountpassword := '';
    { ... } end;

    { ... } AImageHosterPluginProc(AImageHosterPlugin);
    { . } end;
    { } end, AErrorProc);
end;

constructor TApiThreadedPlugin.Create(const ATask: IOmniTask; AErrorHandler: TPluginErrorProc = nil);
begin
  inherited Create;
  FTask := ATask;
  if not Assigned(AErrorHandler) then
    FErrorHandler := DefaultInternalErrorHandler
  else
    FErrorHandler := AErrorHandler;
end;

destructor TApiThreadedPlugin.Destroy;
begin
  FTask := nil;
  inherited Destroy;
end;

function TApiThreadedPlugin.AddArticle(const APublishItem: IPublishItem; ACAPTCHAInput: TCAPTCHAInput = nil; AIntelligentPostingHandler: TIntelligentPostingHelper = nil): Boolean;
var
  LResult, LHighException: WordBool;
begin
  LResult := False;
  LHighException := False;

  TPluginBasic.LoadCMSPlugin(APublishItem.CMSPluginPath,
    { } procedure(var ACMSPlugin: ICMSPlugIn)
    { } begin
    { . } with ACMSPlugin do
    { . } begin
    { ... } if Assigned(ACAPTCHAInput) then
    { ..... } SetCAPTCHAInput(ACAPTCHAInput)
    { ... } else
    { ... } begin
    { ..... } SetCAPTCHAInput(DefaultCAPTCHAInputHandler)
    { ... } end;

    { ... } if Assigned(AIntelligentPostingHandler) then
    { ..... } SetIntelligentPostingHelper(AIntelligentPostingHandler)
    { ... } else
    { ... } begin
    { ..... } SetIntelligentPostingHelper(DefaultIntelligentPostingHelperHandler)
    { ... } end;

    { ... } Accountname := APublishItem.Accountname;
    { ... } Accountpassword := APublishItem.Accountpassword;

    { ... } SettingsFileName := APublishItem.SettingsFileName;

    { ... } // ArticleID

    { ... } Subject := APublishItem.Subject;
    { ... } Tags := APublishItem.Tags;
    { ... } Message := APublishItem.Message;
    { ... } Website := APublishItem.Website;
    { ... } Data := APublishItem.Data;

    { ... } try
    { ..... } LResult := AddArticle;
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(APublishItem.CMSPluginPath)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(APublishItem.CMSPluginPath)]), FErrorHandler);
    { ... } end;
    { . } end;
    { } end, FErrorHandler);

  Result := LResult;
end;

function TApiThreadedPlugin.CrawlerExec(ACrawler: TCrawlerCollectionItem; ATypeID: TTypeID; const AControlController: IControlControllerBase): Boolean;
var
  LResult, LHighException: WordBool;
begin
  LResult := False;
  LHighException := False;

  TPluginBasic.LoadCrawlerPlugin(ACrawler.Path,
    { } procedure(var ACrawlerPlugin: ICrawlerPlugIn)
    { } var
    { . } LComponentIDs: TControlIDs;
    { . } LCrawlerContingentIndex: Integer;
    { } begin
    { . } with ACrawlerPlugin do
    { . } begin
    { ... } LComponentIDs := [];
    { ... } for LCrawlerContingentIndex := 0 to ACrawler.Contingent.Count - 1 do
    { ..... } with TCrawlerContingentCollectionItem(ACrawler.Contingent.Items[LCrawlerContingentIndex]) do
    { ....... } if Status and (TypeID = ATypeID) then
    { ......... } LComponentIDs := LComponentIDs + [ControlID];

    { ... } try
    { ..... } LResult := Exec(Integer(ATypeID), Longword(LComponentIDs), ACrawler.Limit, AControlController);
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(ACrawler.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(ACrawler.Path)]), FErrorHandler);
    { ... } end;
    { . } end;
    { } end, FErrorHandler);

  Result := LResult;
end;

function TApiThreadedPlugin.CrypterAddFolder;
var
  LResult, LHighException: WordBool;
  LFolderInfo: TCrypterFolderInfo;
begin
  LResult := False;
  LHighException := False;

  LoadCrypterPlugin(ACrypter,
    { } procedure(var ACrypterPlugin: ICrypterPlugIn)
    { } begin
    { . } with ACrypterPlugin do
    { . } begin
    { ... } Foldertypes := Byte(ACrypter.Foldertypes);

    { ... } ContainerTypes := Byte(ACrypter.ContainerTypes);

    { ... } UseCaptcha := ACrypter.UseCaptcha;

    { ... } case ACrypter.FolderName of
    { ..... } fnFilename:
    { ....... } FolderName := AMirrorContainer.Directlink[0].FileName;
    { ..... } fnReleasename:
    { ....... } FolderName := AControlController.FindControl(cReleaseName).Value;
    { ..... } fnTitle:
    { ....... } FolderName := AControlController.FindControl(cTitle).Value;
    { ... } end;

    { ... } AdvertismentType := Integer(ACrypter.AdvertismentType);
    { ... } AdvertismentLayerName := ACrypter.AdvertismentLayerName;
    { ... } AdvertismentLayerValue := ACrypter.AdvertismentLayerValue;
    { ... } UseAdvertismentLink := ACrypter.UseAdvertismentLink;
    { ... } AdvertismentLink := ACrypter.AdvertismentLink;
    { ... } UseAdvertismentLink := ACrypter.UseAdvertismentPicture;
    { ... } AdvertismentPicture := ACrypter.AdvertismentPicture;

    { ... } UseCoverLink := ACrypter.UseCoverLink;
    { ... } if Assigned(AControlController.FindControl(cPicture)) then
    { ..... } CoverLink := AControlController.FindControl(cPicture).Value;
    { ... } UseDescription := ACrypter.UseDescription;
    { ... } if Assigned(AControlController.FindControl(cDescription)) then
    { ..... } Description := AControlController.FindControl(cDescription).Value;
    { ... } UseCNL := ACrypter.UseCNL;
    { ... } UseWebseiteLink := ACrypter.UseWebseiteLink;
    { ... } WebseiteLink := ACrypter.WebseiteLink;

    { ... } UseEMailforStatusNotice := ACrypter.UseEMailforStatusNotice;
    { ... } EMailforStatusNotice := ACrypter.EMailforStatusNotice;

    { ... } UseFilePassword := ACrypter.UseFilePassword;
    { ... } if Assigned(AControlController.FindControl(cPassword)) then
    { ..... } FilePassword := AControlController.FindControl(cPassword).Value;
    { ... } UseAdminPassword := ACrypter.UseAdminPassword;
    { ... } AdminPassword := ACrypter.AdminPassword;
    { ... } UseVisitorPassword := ACrypter.UseVisitorPassword;
    { ... } Visitorpassword := ACrypter.Visitorpassword;

    { ... } try
    { ..... } LResult := AddFolder(AMirrorContainer, LFolderInfo);
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(ACrypter.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(ACrypter.Path)]), FErrorHandler);
    { ... } end;
    { . } end;
    { } end, FErrorHandler);

  AFolderInfo := LFolderInfo;
  Result := LResult;
end;

function TApiThreadedPlugin.CrypterGetFolder;
var
  LResult, LHighException: WordBool;
  LFolderInfo: TCrypterFolderInfo;
begin
  LResult := False;
  LHighException := False;

  LoadCrypterPlugin(ACrypter,
    { } procedure(var ACrypterPlugin: ICrypterPlugIn)
    { } begin
    { . } with ACrypterPlugin do
    { . } begin

    { ... } try
    { ..... } LResult := GetFolder(AFolderIdentifier, LFolderInfo);
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if LResult then
    { ... } begin
    { ..... } LFolderInfo.Hoster := THosterConfiguration.GetCustomisedHoster(LFolderInfo.Hoster);
    { ..... } LFolderInfo.HosterShort := THosterConfiguration.GetCustomisedHoster(LFolderInfo.Hoster, True);
    { ... } end
    { ... } else
    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(ACrypter.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(ACrypter.Path)]), FErrorHandler);
    { ... } end;
    { . } end;
    { } end, FErrorHandler);

  AFolderInfo := LFolderInfo;
  Result := LResult;
end;

function TApiThreadedPlugin.FileHosterCheckFiles(AFileHoster: TPlugInCollectionItem; const ALinks: string; out ALinksInfo: TLinksInfo): Boolean;
var
  LResult, LHighException: WordBool;
  LLinksInfo: TLinksInfo;
  unknown, online, offline, temporaryoffline: Integer;
begin
  unknown := 0;
  online := 0;
  offline := 0;
  temporaryoffline := 0;

  TPluginBasic.LoadFileHosterPlugin(AFileHoster.Path,
    { } procedure(var AFileHosterPlugin: IFileHosterPlugIn)
    { } var
    { . } LLinkCount, LLinksIndex: Integer;
    { . } LGlobalSize, LMaxPartSize: Int64;
    { . } LLinkInfo: TLinkInfo;
    { } begin
    { . } LLinkCount := 0;
    { . } LGlobalSize := 0;
    { . } LMaxPartSize := 0;

    { . } with AFileHosterPlugin do
    { . } begin
    { ... } try
    { ..... } LLinkCount := CheckLinks(ALinks);
    { ..... } LResult := True; // TODO: Improve interface with boolean result
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if (LLinkCount > 0) then
    { ... } begin
    { ..... } for LLinksIndex := 0 to LLinkCount - 1 do
    { ..... } begin
    { ....... } SetLength(LLinksInfo.Links, LLinksIndex + 1);
    { ....... } LLinkInfo := CheckedLink(LLinksIndex);

    { ....... } case LLinkInfo.Status of
    { ......... } csOffline: Inc(offline);
    { ......... } csOnline: Inc(online);
    { ......... } csUnknown: Inc(unknown);
    { ......... } csTemporaryOffline: Inc(temporaryoffline);
    { ....... } end;

    { ....... } LGlobalSize := LGlobalSize + Round(LLinkInfo.Size);
    { ....... } if (LMaxPartSize < LLinkInfo.Size) then
    { ......... } LMaxPartSize := Round(LLinkInfo.Size);

    { ....... } LLinksInfo.Links[LLinksIndex] := LLinkInfo;
    { ..... } end;

    { ..... } if (unknown = 0) and (online = 0) and (temporaryoffline = 0) then
    { ....... } LLinksInfo.Status := csOffline
    { ..... } else if (unknown = 0) and (offline = 0) then
    { ....... } LLinksInfo.Status := csOnline
    { ..... } else if (offline > 0) and (online > 0) then
    { ....... } LLinksInfo.Status := csMixedOnOffline
    { ..... } else
    { ....... } LLinksInfo.Status := csUnknown;

    { ..... } LLinksInfo.Size := RoundTo((LGlobalSize / 1048576), -2);
    { ..... } LLinksInfo.PartSize := RoundTo((LMaxPartSize / 1048576), -2);

    { ... } end
    { ... } else
    { ... } if not SameStr('', ErrorMsg) then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(AFileHoster.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(AFileHoster.Path)]), FErrorHandler);
    { ... } end;

    { . } end;
    { } end, FErrorHandler);

  ALinksInfo := LLinksInfo;
  Result := LResult;
end;

function TApiThreadedPlugin.ImageHosterLocalUpload;
var
  LResult, LHighException: WordBool;
  LResultUrl: WideString;
begin
  LResult := False;
  LHighException := False;
  LResultUrl := '';

  LoadImageHosterPlugin(AImageHoster,
    { } procedure(var AImageHosterPlugin: IImageHosterPlugIn)
    { } begin
    { . } with AImageHosterPlugin do
    { . } begin
    { ... } if Assigned(ACAPTCHAInput) then
    { ..... } SetCAPTCHAInput(ACAPTCHAInput)
    { ... } else
    { ... } begin
    { ..... } SetCAPTCHAInput(DefaultCAPTCHAInputHandler)
    { ... } end;

    { ... } ImageHostResize := AImageHoster.ImageHostResize;

    { ... } try
    { ..... } LResult := LocalUpload(ALocalPath, LResultUrl);
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(AImageHoster.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(AImageHoster.Path)]), FErrorHandler);
    { ... } end;

    { . } end;
    { } end, FErrorHandler);

  AUrl := LResultUrl;
  Result := LResult;
end;

function TApiThreadedPlugin.ImageHosterRemoteUpload;
var
  LResult, LHighException: WordBool;
  LResultUrl: WideString;
begin
  LResult := False;
  LHighException := False;
  LResultUrl := '';

  LoadImageHosterPlugin(AImageHoster,
    { } procedure(var AImageHosterPlugin: IImageHosterPlugIn)
    { } begin
    { . } with AImageHosterPlugin do
    { . } begin
    { ... } if Assigned(ACAPTCHAInput) then
    { ..... } SetCAPTCHAInput(ACAPTCHAInput)
    { ... } else
    { ... } begin
    { ..... } SetCAPTCHAInput(DefaultCAPTCHAInputHandler)
    { ... } end;

    { ... } ImageHostResize := AImageHoster.ImageHostResize;

    { ... } try
    { ..... } LResult := RemoteUpload(ARemoteUrl, LResultUrl);
    { ... } except
    { ..... } LHighException := True;
    { ... } end;

    { ... } if not LResult and not LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [ErrorMsg, ExtractFileName(AImageHoster.Path)]), FErrorHandler);
    { ... } end
    { ... } else
    { ... } if LHighException then
    { ... } begin
    { ..... } TPluginBasic.ReturnError(Format(StrPluginInternalError, [SysErrorMessage(GetLastError()), ExtractFileName(AImageHoster.Path)]), FErrorHandler);
    { ... } end;

    { . } end;
    { } end, FErrorHandler);

  AUrl := LResultUrl;
  Result := LResult;
end;

end.
