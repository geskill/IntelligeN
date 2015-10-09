unit uApiPlugins;

interface

uses
  // Delphi
  Windows, Forms, SysUtils, Classes, Math, Graphics,
  // Spring Framework
  Spring.Utils,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiCAPTCHA, uApiConst, uApiMain, uApiSettings, uApiWebsiteEditor, uApiXml,
  // HTTPManager
  uHTTPManager,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInInterfaceAdv,
  // Utils
  uPathUtils;

type
  TErrorProc = reference to procedure(AErrorMsg: string);
  TPluginProc = reference to procedure(var APlugin: IPlugIn);
  TCrypterPluginProc = reference to procedure(var ACrypterPlugin: ICrypterPlugIn);
  TFileFormatPluginProc = reference to procedure(var AFileFormatPlugin: IFileFormatPlugIn);

  TApiPlugin = class
  strict private
    class procedure ReturnError(AErrorMsg: string; AErrorProc: TErrorProc = nil);
  private
    class procedure LoadPlugin(ARelativPluginPath: string; APluginProc: TPluginProc; AErrorProc: TErrorProc = nil);
    class procedure LoadCrypterPlugin(ACrypter: TCrypterCollectionItem; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TErrorProc = nil);
    class procedure LoadFileFormatsPlugin(ARelativPluginPath: string; AFileFormatPluginProc: TFileFormatPluginProc; AErrorProc: TErrorProc = nil);
  public
    class procedure AppLoad(App: TAppCollectionItem; AppController: IAppController);
    class procedure AppUnLoad(App: TAppCollectionItem);

    class function CAPTCHAExec(ACAPTCHAPluginPath: string; const ACAPTCHAType: TCAPTCHAType; const ACAPTCHA, CAPTCHAName: string; out AText: string; var ACookies: string): Boolean;

    class function CMSExec(const APublishItem: IPublishItem; AErrorProc: TErrorProc = nil; ACAPTCHAInput: TCAPTCHAInput = nil; AIntelligentPostingHandler: TIntelligentPostingHelper = nil): Boolean;
    class function CMSDefaultCharset(ACMSPluginPath: string): string;
    class function CMSBelongsTo(ACMSPluginPath, AWebsiteSourceCode: string): Boolean;
    class procedure CMSShowWebsiteSettingsEditor(ACMSPluginPath: string; CMSWebsites: TCMSWebsitesCollectionItem; AppController: IAppController);

    class function CrawlerExec(ACrawler: TCrawlerCollectionItem; const AControlController: IControlController; out AErrorMsg: string): Boolean;

    class function CrypterAddFolder(ACrypter: TCrypterCollectionItem; const AMirrorContainer: IDirectlinkContainer; const AControlController: IControlControllerBase; out AFolderInfo: TCrypterFolderInfo; out AErrorMsg: string): Boolean;
    class function CrypterGetFolder(ACrypter: TCrypterCollectionItem; AFolderIdentifier: string; out AFolderInfo: TCrypterFolderInfo; out AErrorMsg: string): Boolean;

    class function GetSaveFileFormats: TStrings;
    class function GetLoadFileFormats: TStrings;
    class procedure SaveFile(AFileFormats: TPlugInCollectionItem; AFileName, ATemplateFileName: string; ATabSheetController: ITabSheetController);
    class procedure LoadFile(AFileName: string; APageController: IPageController);

    class function CheckFiles(FileHoster: TPlugInCollectionItem; ALinks: string): TLinksInfo;

    class function ImageHosterLocalUpload(ImageHoster: TImageHosterCollectionItem; LocalPath: string; AErrorProc: TErrorProc = nil; ACAPTCHAInput: TCAPTCHAInput = nil): string;
    class function ImageHosterRemoteUpload(ImageHoster: TImageHosterCollectionItem; ImageUrl: string; AErrorProc: TErrorProc = nil; ACAPTCHAInput: TCAPTCHAInput = nil): string;
  end;

implementation

uses
  uIntelligentPosting;
{$REGION 'TApiPlugin private'}

class procedure TApiPlugin.ReturnError;
begin
  if Assigned(AErrorProc) then
    AErrorProc(AErrorMsg)
  else
    raise Exception.Create(AErrorMsg);
end;

class procedure TApiPlugin.LoadPlugin(ARelativPluginPath: string; APluginProc: TPluginProc; AErrorProc: TErrorProc = nil);
var
  LPluginMinorVersion: Integer;
  PluginPath: string;
  hLib: Cardinal;
  MLoadPlugIn: TLoadPlugIn;
  Plugin: IPlugIn;
begin
  // OutputDebugString(PChar('LoadPlugin() START ' + IntToStr(GetCurrentThreadId)));
  PluginPath := PathCombineEx(GetPluginFolder, ARelativPluginPath);
  // OutputDebugString(PChar('LoadPlugin() after PathCombineEx() ' + IntToStr(GetCurrentThreadId)));
  if FileExists(PluginPath) then
  begin
    LPluginMinorVersion := TFileVersionInfo.GetVersionInfo(PluginPath).FileVersionNumber.Minor;
    if not(MINOR_VERSION = LPluginMinorVersion) then
      ReturnError(Format(StrThisPluginIsIncom, [MINOR_VERSION, LPluginMinorVersion]), AErrorProc);

    hLib := LoadLibrary(PChar(PluginPath));
    try
      if not(hLib = 0) then
      begin
        @MLoadPlugIn := GetProcAddress(hLib, 'LoadPlugIn');

        if MLoadPlugIn(Plugin) then
          try
            Plugin.SetHTTPManager(THTTPManager.Instance);

            case Plugin.GetType of
              ptApp, ptCAPTCHA, ptFileFormats:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaMain);
              ptCMS:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCMS);
              ptCrawler:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCMS);
              ptCrypter:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaCrypter);
              ptFileHoster:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaFileHoster);
              ptImageHoster:
                Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaImageHoster);
            end;

            Plugin.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
            Plugin.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;

            APluginProc(Plugin);

            Plugin.SetHTTPManager(nil);
          finally
            Plugin := nil;
          end;
      end
      else
        ReturnError(Format(StrPluginDamaged, [SysErrorMessage(GetLastError())]), AErrorProc);
    finally
      { TODO :
        Free library opened in threads, too! somewhere memory leak?
        => freeze error! }
      // if (GetCurrentThreadId = MainThreadID) then
      FreeLibrary(hLib);
    end;
  end
  else
    ReturnError('Plugin not found! (' + ARelativPluginPath + ')', AErrorProc);
end;

class procedure TApiPlugin.LoadCrypterPlugin(ACrypter: TCrypterCollectionItem; ACrypterPluginProc: TCrypterPluginProc; AErrorProc: TErrorProc);
begin
  LoadPlugin(ACrypter.Path,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LPlugin: ICrypterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICrypterPlugIn, LPlugin) = 0 then
    { ... } try
    { ..... } with LPlugin do
    { ..... } begin
    { ....... } UseAccount := ACrypter.UseAccount;
    { ....... } if ACrypter.UseAccount then
    { ....... } begin
    { ......... } Accountname := ACrypter.Accountname;
    { ......... } Accountpassword := ACrypter.Accountpassword;
    { ....... } end
    { ....... } else
    { ....... } begin
    { ......... } Accountname := '';
    { ......... } Accountpassword := '';
    { ....... } end;

    { ....... } try
    { ......... } ACrypterPluginProc(LPlugin);
    { ....... } except

    { ....... } end;
    { ..... } end;
    { ... } finally
    { ..... } LPlugin := nil;
    { ... } end;
    { } end, AErrorProc);
end;

class procedure TApiPlugin.LoadFileFormatsPlugin(ARelativPluginPath: string; AFileFormatPluginProc: TFileFormatPluginProc; AErrorProc: TErrorProc = nil);
begin
  LoadPlugin(ARelativPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: IFileFormatPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IFileFormatPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin
    { ....... } try
    { ......... } AFileFormatPluginProc(_Plugin);
    { ....... } except

    { ....... } end;
    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end, AErrorProc);
end;
{$ENDREGION}

class procedure TApiPlugin.AppLoad;
var
  hLib: Cardinal;
  MLoadPlugIn: TLoadAppPlugIn;
  _Error: Boolean;
begin
  _Error := False;
  hLib := LoadLibrary(PChar(PathCombineEx(GetPluginFolder, App.Path)));
  if not(hLib = 0) then
  begin
    @MLoadPlugIn := GetProcAddress(hLib, 'LoadPlugIn');
    App.LibraryID := hLib;
    if MLoadPlugIn(App.Plugin) then
      try
        App.Plugin.Proxy := SettingsManager.Settings.HTTP.GetProxy(psaMain);
        App.Plugin.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
        App.Plugin.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;
        _Error := not App.Plugin.Start(AppController);
        if _Error then
          App.Plugin.Stop;
      except
        App.Plugin := nil;
      end;
  end;
  if _Error then
    FreeLibrary(hLib);
end;

class procedure TApiPlugin.AppUnLoad(App: TAppCollectionItem);
var
  hLib: Cardinal;
  // _Error: Boolean;
begin
  hLib := App.LibraryID;
  if not(hLib = 0) then
  begin
    try
      App.Plugin.Stop;
    finally
      App.Plugin := nil;
    end;
  end;
  FreeLibrary(hLib);
end;

class function TApiPlugin.CAPTCHAExec;
var
  _Result: Boolean;
  _Cookies, _CAPTCHAResult: WideString;
begin
  _Result := False;

  _CAPTCHAResult := '';
  _Cookies := ACookies;

  LoadPlugin(ACAPTCHAPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: ICAPTCHAPlugIn;
    { . }
    { } begin
    { . } if APlugin.QueryInterface(ICAPTCHAPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } _Plugin.CAPTCHA := ACAPTCHA;
    { ..... } _Plugin.CAPTCHAType := ACAPTCHAType;
    { ..... } _Plugin.CAPTCHAName := CAPTCHAName;
    { ..... } _Plugin.Cookies := _Cookies;

    { ..... } _Result := _Plugin.Exec;

    { ..... } if _Result then
    { ..... } begin
    { ....... } _Cookies := _Plugin.Cookies;
    { ....... } _CAPTCHAResult := _Plugin.CAPTCHAResult;
    { ..... } end;

    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end);

  AText := _CAPTCHAResult;
  ACookies := _Cookies;
  Result := _Result;
end;

class function TApiPlugin.CMSExec;
var
  _Result: Boolean;
begin
  _Result := False;
  LoadPlugin(APublishItem.CMSPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: ICMSPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICMSPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin
    { ....... } if Assigned(ACAPTCHAInput) then
    { ......... } SetCAPTCHAInput(ACAPTCHAInput)
    { ....... } else
    { ....... } begin
    { ......... } SetCAPTCHAInput(TCAPTCHAClass.CAPTCHAInput)
    { ....... } end;

    { ....... } if Assigned(AIntelligentPostingHandler) then
    { ......... } SetIntelligentPostingHelper(AIntelligentPostingHandler)
    { ....... } else
    { ....... } begin
    { ......... } SetIntelligentPostingHelper(TIntelligentPostingClass.IntelligentPostingHandler)
    { ....... } end;

    { ....... } Accountname := APublishItem.Accountname;
    { ....... } Accountpassword := APublishItem.Accountpassword;

    { ....... } SettingsFileName := APublishItem.SettingsFileName;

    { ....... } // ArticleID

    { ....... } Subject := APublishItem.Subject;
    { ....... } Tags := APublishItem.Tags;
    { ....... } Message := APublishItem.Message;
    { ....... } Website := APublishItem.Website;
    { ....... } Data := APublishItem.Data;

    { ....... } try
    { ......... } _Result := Exec;
    { ......... } ReturnError(ErrorMsg, AErrorProc);
    { ....... } except
    { ......... } on E: Exception do
    { ......... } begin
    { ........... } ReturnError('Unhandled exception (' + E.message + ') occurred', AErrorProc);
    { ......... } end;
    { ....... } end;
    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end, AErrorProc);

  Result := _Result;
end;

class function TApiPlugin.CMSDefaultCharset(ACMSPluginPath: string): string;
var
  _Result: string;
begin
  _Result := '';
  LoadPlugin(ACMSPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: ICMSPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICMSPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } _Result := _Plugin.DefaultCharset;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end);
  Result := _Result;
end;

class function TApiPlugin.CMSBelongsTo(ACMSPluginPath, AWebsiteSourceCode: string): Boolean;
var
  _BelongsTo: Boolean;
begin
  _BelongsTo := False;
  LoadPlugin(ACMSPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: ICMSPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(ICMSPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } _BelongsTo := _Plugin.BelongsTo(AWebsiteSourceCode);
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end);

  Result := _BelongsTo;
end;

class procedure TApiPlugin.CMSShowWebsiteSettingsEditor;
begin
  LoadPlugin(ACMSPluginPath,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: ICMSPlugIn;
    { . } _WebsiteEditor: TBasisWebsiteEditor;
    { } begin
    { . } if APlugin.QueryInterface(ICMSPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin

    { ....... } SetCAPTCHAInput(TCAPTCHAClass.CAPTCHAInput);

    { ....... } Accountname := CMSWebsites.Accountname;
    { ....... } Accountpassword := CMSWebsites.Accountpassword;

    { ....... } SettingsFileName := CMSWebsites.GetPath;

    { ....... } Website := CMSWebsites.name;

    { ....... } _WebsiteEditor := TWebsiteEditorFactory.GetClassType(_Plugin.CMSType).Create(_Plugin, AppController, CMSWebsites.GetPath);
    { ....... } try
    { ......... } if ShowWebsiteSettingsEditor(_WebsiteEditor) then
    { ........... } ;
    { ....... } finally
    { ......... } _WebsiteEditor.Free;
    { ....... } end;
    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end);
end;

class function TApiPlugin.CrawlerExec;
var
  LResult: Boolean;
  LFolderInfo: TCrypterFolderInfo;
  LErrorMsg: string;
begin
  LResult := False;
  LoadPlugin(ACrawler.Path,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LPlugin: ICrawlerPlugIn;
    { . } LComponentIDs: TControlIDs;
    { . } LCrawlerContingentIndex: Integer;
    { } begin
    { . } if APlugin.QueryInterface(ICrawlerPlugIn, LPlugin) = 0 then
    { ... } try
    { ..... } with LPlugin do
    { ..... } begin
    { ....... } LComponentIDs := [];
    { ....... } for LCrawlerContingentIndex := 0 to ACrawler.Contingent.Count - 1 do
    { ......... } with TCrawlerContingentCollectionItem(ACrawler.Contingent.Items[LCrawlerContingentIndex]) do
    { ........... } if Status and (TypeID = AControlController.TypeID) then
    { ............. } LComponentIDs := LComponentIDs + [ControlID];

    { ....... } try
    { ......... } LResult:= Exec(Integer(AControlController.TypeID), Longword(LComponentIDs), ACrawler.Limit, AControlController);
    { ......... } LErrorMsg := ErrorMsg;
    { ....... } except
    { ......... } on E: Exception do
    { ......... } begin
    { ........... } LErrorMsg := E.message;
    { ......... } end;
    { ....... } end;

    { ..... } end;
    { ... } finally
    { ..... } LPlugin := nil;
    { ... } end;
    { } end,
    { } procedure(AErrorMsg: string)
    { } begin
    { . } LErrorMsg := AErrorMsg;
    { } end);

  AErrorMsg := LErrorMsg;
  Result := LResult;
end;

class function TApiPlugin.CrypterAddFolder;
var
  LResult: Boolean;
  LFolderInfo: TCrypterFolderInfo;
  LErrorMsg: string;
begin
  LResult := False;
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
    { ..... } LErrorMsg := ErrorMsg;
    { ... } except
    { ..... } on E: Exception do
    { ..... } begin
    { ...... } LErrorMsg := E.message;
    { ..... } end;
    { ... } end;

    { . } end;
    { } end,
    { } procedure(AErrorMsg: string)
    { } begin
    { . } LErrorMsg := AErrorMsg;
    { } end);
  AFolderInfo := LFolderInfo;
  AErrorMsg := LErrorMsg;
  Result := LResult;
end;

class function TApiPlugin.CrypterGetFolder;
var
  LResult: Boolean;
  LFolderInfo: TCrypterFolderInfo;
  LErrorMsg: string;
begin
  LResult := False;
  LoadCrypterPlugin(ACrypter,
    { } procedure(var ACrypterPlugin: ICrypterPlugIn)
    { } begin
    { . } with ACrypterPlugin do
    { . } begin
    { ... } LResult := GetFolder(AFolderIdentifier, LFolderInfo);
    { ... } if LResult then
    { ... } begin
    { ..... } LFolderInfo.Hoster := THosterConfiguration.GetCustomisedHoster(LFolderInfo.Hoster);
    { ..... } LFolderInfo.HosterShort := THosterConfiguration.GetCustomisedHoster(LFolderInfo.Hoster, True);
    { ... } end;
    { . } end;
    { } end,
    { } procedure(AErrorMsg: string)
    { } begin
    { . } LErrorMsg := AErrorMsg;
    { } end);
  AFolderInfo := LFolderInfo;
  AErrorMsg := LErrorMsg;
  Result := LResult;
end;

class function TApiPlugin.GetSaveFileFormats: TStrings;
var
  _Result: TStringList;
  _FileFormatCollectionIndex: Integer;
begin
  _Result := TStringList.Create;

  with SettingsManager.Settings.Plugins.FileFormats do
    for _FileFormatCollectionIndex := 0 to Count - 1 do
      with TFileFormatsCollectionItem(Items[_FileFormatCollectionIndex]) do
        if Enabled then
        begin
          LoadFileFormatsPlugin(Path,
            { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
            { } begin
            { . } if AFileFormatPlugin.CanSaveControls then
            { ... } _Result.Add(AFileFormatPlugin.GetName + _Result.NameValueSeparator + Format(AFileFormatPlugin.GetFileFormatName, [StrDocument]));
            { } end);
        end;

  Result := _Result;
end;

class function TApiPlugin.GetLoadFileFormats: TStrings;
var
  _Result: TStringList;
  _FileFormatCollectionIndex: Integer;
begin
  _Result := TStringList.Create;

  with SettingsManager.Settings.Plugins.FileFormats do
    for _FileFormatCollectionIndex := 0 to Count - 1 do
      with TFileFormatsCollectionItem(Items[_FileFormatCollectionIndex]) do
        if Enabled then
        begin
          LoadFileFormatsPlugin(Path,
            { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
            { } begin
            { . } if AFileFormatPlugin.CanLoadControls then
            { ... } _Result.Add(AFileFormatPlugin.GetName + _Result.NameValueSeparator + Format(AFileFormatPlugin.GetFileFormatName, [StrDocument]));
            { } end);
        end;

  Result := _Result;
end;

class procedure TApiPlugin.SaveFile(AFileFormats: TPlugInCollectionItem; AFileName, ATemplateFileName: string; ATabSheetController: ITabSheetController);
begin
  LoadFileFormatsPlugin(AFileFormats.Path,
    { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
    { } begin
    { . } if AFileFormatPlugin.CanSaveControls then
    { ... } AFileFormatPlugin.SaveControls(AFileName, ATemplateFileName, ATabSheetController)
    { } end);
end;

class procedure TApiPlugin.LoadFile(AFileName: string; APageController: IPageController);
var
  I, TabIndex: Integer;
  Stop: Boolean;
begin
  Stop := False;

  with SettingsManager.Settings.Plugins.FileFormats do
    for I := 0 to Count - 1 do
      if not Stop then
        with TFileFormatsCollectionItem(Items[I]) do
          if Enabled then
            LoadFileFormatsPlugin(Path,
              { } procedure(var AFileFormatPlugin: IFileFormatPlugIn)
              { } begin
              { . } if AFileFormatPlugin.CanLoadControls then
              { . } begin
              { ... } try
              { ..... } AFileFormatPlugin.ForceAddCrypter := ForceAddCrypter;
              { ..... } AFileFormatPlugin.ForceAddImageMirror := ForceAddImageMirror;
              { ..... } TabIndex := AFileFormatPlugin.LoadControls(AFileName, GetTemplatesTypeFolder, APageController);
              { ..... } Stop := (TabIndex = -1);
              { ..... } if not Stop then
              { ....... } with APageController.TabSheetController[TabIndex] do
              { ....... } begin
              { ......... } Application.ProcessMessages;
              { ......... } ResetDataChanged(AFileName, AFileFormatPlugin.GetName);
              { ......... } PublishController.Active := True;
              { ....... } end;
              { ... } except

              { ... } end;
              { . } end;
              { } end);
end;

class function TApiPlugin.CheckFiles(FileHoster: TPlugInCollectionItem; ALinks: string): TLinksInfo;
var
  _Result: TLinksInfo;
  unknown, online, offline, temporaryoffline: Integer;
begin
  unknown := 0;
  online := 0;
  offline := 0;
  temporaryoffline := 0;

  LoadPlugin(FileHoster.Path,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: IFileHosterPlugIn;
    { . } _Links, _LinksIndex: Integer;
    { . } _GlobalSize, _MaxPartSize: Int64;
    { . } _LinkInfo: TLinkInfo;
    { } begin
    { . } _GlobalSize := 0;
    { . } _MaxPartSize := 0;
    { . } if APlugin.QueryInterface(IFileHosterPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin
    { ....... } try
    { ......... } _Links := CheckLinks(ALinks);
    { ......... } for _LinksIndex := 0 to _Links - 1 do
    { ......... } begin
    { ........... } SetLength(_Result.Links, _LinksIndex + 1);
    { ........... } _LinkInfo := CheckedLink(_LinksIndex);

    { ........... } case _LinkInfo.Status of
    { ............. } csOffline: Inc(offline);
    { ............. } csOnline: Inc(online);
    { ............. } csUnknown: Inc(unknown);
    { ............. } csTemporaryOffline: Inc(temporaryoffline);
    { ........... } end;

    { ........... } _GlobalSize := _GlobalSize + Round(_LinkInfo.Size);
    { ........... } if (_MaxPartSize < _LinkInfo.Size) then
    { ............. } _MaxPartSize := Round(_LinkInfo.Size);

    { ........... } _Result.Links[_LinksIndex] := _LinkInfo;
    { ......... } end;

    { ......... } if (unknown = 0) and (online = 0) and (temporaryoffline = 0) then
    { ........... } _Result.Status := csOffline
    { ......... } else if (unknown = 0) and (offline = 0) then
    { ........... } _Result.Status := csOnline
    { ......... } else if (offline > 0) and (online > 0) then
    { ........... } _Result.Status := csMixedOnOffline
    { ......... } else
    { ........... } _Result.Status := csUnknown;

    { ......... } _Result.Size := RoundTo((_GlobalSize / 1048576), -2);
    { ......... } _Result.PartSize := RoundTo((_MaxPartSize / 1048576), -2);

    { ....... } except

    { ....... } end;

    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end);

  Result := _Result;
end;

class function TApiPlugin.ImageHosterLocalUpload(ImageHoster: TImageHosterCollectionItem; LocalPath: string; AErrorProc: TErrorProc; ACAPTCHAInput: TCAPTCHAInput): string;
var
  _Result: string;
begin
  _Result := '';
  LoadPlugin(ImageHoster.Path,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: IImageHosterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IImageHosterPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin
    { ....... } if Assigned(ACAPTCHAInput) then
    { ......... } SetCAPTCHAInput(ACAPTCHAInput)
    { ....... } else
    { ....... } begin
    { ......... } SetCAPTCHAInput(TCAPTCHAClass.CAPTCHAInput)
    { ....... } end;

    { ....... } UseAccount := ImageHoster.UseAccount;
    { ....... } if ImageHoster.UseAccount then
    { ....... } begin
    { ......... } Accountname := ImageHoster.Accountname;
    { ......... } Accountpassword := ImageHoster.Accountpassword;
    { ....... } end
    { ....... } else
    { ....... } begin
    { ......... } Accountname := '';
    { ......... } Accountpassword := '';
    { ....... } end;

    { ....... } ImageHostResize := ImageHoster.ImageHostResize;

    { ....... } try
    { ......... } _Result := LocalUpload(LocalPath);
    { ......... } ReturnError(ErrorMsg, AErrorProc);
    { ....... } except
    { ......... } on E: Exception do
    { ......... } begin
    { ........... } ReturnError('Unhandled exception (' + E.message + ') occurred', AErrorProc);
    { ......... } end;
    { ....... } end;

    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end, AErrorProc);

  Result := _Result;
end;

class function TApiPlugin.ImageHosterRemoteUpload;
var
  _Result: string;
begin
  _Result := ImageUrl;
  LoadPlugin(ImageHoster.Path,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } _Plugin: IImageHosterPlugIn;
    { } begin
    { . } if APlugin.QueryInterface(IImageHosterPlugIn, _Plugin) = 0 then
    { ... } try
    { ..... } with _Plugin do
    { ..... } begin
    { ....... } if Assigned(ACAPTCHAInput) then
    { ......... } SetCAPTCHAInput(ACAPTCHAInput)
    { ....... } else
    { ....... } begin
    { ......... } SetCAPTCHAInput(TCAPTCHAClass.CAPTCHAInput)
    { ....... } end;

    { ....... } UseAccount := ImageHoster.UseAccount;
    { ....... } if ImageHoster.UseAccount then
    { ....... } begin
    { ......... } Accountname := ImageHoster.Accountname;
    { ......... } Accountpassword := ImageHoster.Accountpassword;
    { ....... } end
    { ....... } else
    { ....... } begin
    { ......... } Accountname := '';
    { ......... } Accountpassword := '';
    { ....... } end;

    { ....... } ImageHostResize := ImageHoster.ImageHostResize;

    { ....... } try
    { ......... } _Result := RemoteUpload(ImageUrl);
    { ......... } ReturnError(ErrorMsg, AErrorProc);
    { ....... } except
    { ......... } on E: Exception do
    { ......... } begin
    { ........... } ReturnError('Unhandled exception (' + E.message + ') occurred', AErrorProc);
    { ......... } end;
    { ....... } end;

    { ..... } end;
    { ... } finally
    { ..... } _Plugin := nil;
    { ... } end;
    { } end, AErrorProc);

  Result := _Result;
end;

end.
