unit uPlugInInterface;

interface

uses
  // Common
  uAppInterface,
  // Plugin
  uPlugInConst;

type
  IProxy = interface
    ['{CE0B1CFC-8158-42E9-8E90-0D5B1BB1F354}']
    function GetActive: Boolean;
    function GetType: TProxyType;
    function GetServer: WideString; safecall;
    function GetPort: Integer;
    function GetAccountName: WideString; safecall;
    function GetAccountPassword: WideString; safecall;

    procedure Activate(AType: TProxyType; AServer: WideString; APort: Integer; AAccountName, AAccountPassword: WideString);
    property Active: Boolean read GetActive;
    property ServerType: TProxyType read GetType;
    property Server: WideString read GetServer;
    property Port: Integer read GetPort;
    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;
  end;

  IPlugIn = interface
    ['{D85F3CDC-641E-4103-9AD9-59FA514E2A27}']
    function GetCAPTCHAInput: TCAPTCHAInput; safecall;
    procedure SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput);
    function GetProxy: IProxy; safecall;
    procedure SetProxy(AProxy: IProxy);
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(AConnectTimeout: Integer); safecall;
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(AReadTimeout: Integer); safecall;
    function GetErrorMsg: WideString; safecall;
    procedure SetErrorMsg(AErrorMsg: WideString);

    function GetName: WideString; safecall;
    property CAPTCHAInput: TCAPTCHAInput read GetCAPTCHAInput;

    property Proxy: IProxy read GetProxy write SetProxy;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;
  end;

  IAppPlugIn = interface(IPlugIn)
    ['{DB81AD44-5514-4F6E-BF24-663E8A0AD66A}']
    function Start(const AAppController: IAppController): Boolean; stdcall;
    procedure Stop; stdcall;
  end;

  ICAPTCHAPlugIn = interface(IPlugIn)
    ['{02A910AE-7312-4D09-8194-61000E6F63D0}']
    function GetCAPTCHA: WideString; safecall;
    procedure SetCAPTCHA(ACAPTCHA: WideString);
    function GetCAPTCHAType: TCAPTCHAType; safecall;
    procedure SetCAPTCHAType(ACAPTCHAType: TCAPTCHAType);
    function GetCAPTCHAName: WideString; safecall;
    procedure SetCAPTCHAName(ACAPTCHAName: WideString);
    function GetCAPTCHAResult: WideString; safecall;
    procedure SetCAPTCHAResult(ACAPTCHAResult: WideString);
    function GetCookies: WideString; safecall;
    procedure SetCookies(ACookies: WideString);

    property CAPTCHA: WideString read GetCAPTCHA write SetCAPTCHA;
    property CAPTCHAType: TCAPTCHAType read GetCAPTCHAType write SetCAPTCHAType;
    property CAPTCHAName: WideString read GetCAPTCHAName write SetCAPTCHAName;
    property CAPTCHAResult: WideString read GetCAPTCHAResult write SetCAPTCHAResult;
    property Cookies: WideString read GetCookies write SetCookies;

    function Exec: Boolean;
  end;

  ICMSPlugIn = interface(IPlugIn)
    ['{7ABEB7A9-89E9-4BDB-B81A-CAE6FAE42C16}']
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(ASettingsFileName: WideString);
    function GetSubject: WideString; safecall;
    procedure SetSubject(ASubject: WideString);
    function GetTags: WideString; safecall;
    procedure SetTags(ATags: WideString);
    function GetMessage: WideString; safecall;
    procedure SetMessage(AMessage: WideString);
    function GetWebsite: WideString; safecall;
    procedure SetWebsite(AWebsite: WideString);

    function GetArticleID: Integer;
    procedure SetArticleID(AArticleID: Integer);

    function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;
    procedure SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper);

    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
    property SettingsFileName: WideString read GetSettingsFileName write SetSettingsFileName;
    property Subject: WideString read GetSubject write SetSubject;
    property Tags: WideString read GetTags write SetTags;
    property Message: WideString read GetMessage write SetMessage;
    property Website: WideString read GetWebsite write SetWebsite;

    property ArticleID: Integer read GetArticleID write SetArticleID;

    property IntelligentPostingHelper: TIntelligentPostingHelper read GetIntelligentPostingHelper;

    function CMSType: TCMSType;
    function DefaultCharset: WideString;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean;
    function GetIDs: Integer;
    function ReadID(AIndex: Integer): TIDInfo;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean;
  end;

  ICrawlerPlugIn = interface(IPlugIn)
    ['{81CCF306-EB14-4A7A-8D3B-37B64450B86B}']
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);

    function GetAvailableTemplateTypeIDs: Integer; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; stdcall;
    function GetLimitDefaultValue: Integer; stdcall;

    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); stdcall;
  end;

  ICrypterPlugIn = interface(IPlugIn)
    ['{A96AAEFA-8248-4BDB-B909-E76E3C98E97F}']
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);

    function GetUseCoverLink: Boolean;
    procedure SetUseCoverLink(AUseCoverLink: Boolean);
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(ACoverLink: WideString);
    function GetUseDescription: Boolean;
    procedure SetUseDescription(AUseDescription: Boolean);
    function GetDescription: WideString; safecall;
    procedure SetDescription(ADescription: WideString);
    function GetUseCNL: Boolean;
    procedure SetUseCNL(AUseCNL: Boolean);
    function GetUseWebseiteLink: Boolean;
    procedure SetUseWebseiteLink(AUseWebseiteLink: Boolean);
    function GetWebseiteLink: WideString; safecall;
    procedure SetWebseiteLink(AWebseiteLink: WideString);

    function GetFoldertypes: Integer;
    procedure SetFoldertypes(AFoldertypes: Integer);
    function GetContainerTypes: Integer;
    procedure SetContainerTypes(AContainerTypes: Integer);
    function GetUseCaptcha: Boolean;
    procedure SetUseCaptcha(AUseCaptcha: Boolean);
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(AFolderName: WideString);

    function GetAdvertismentType: Integer;
    procedure SetAdvertismentType(AAdvertismentType: Integer);
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(AAdvertismentLayerName: WideString);
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(AAdvertismentLayerValue: WideString);
    function GetUseAdvertismentLink: Boolean;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: Boolean);
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(AAdvertismentLink: WideString);
    function GetUseAdvertismentPicture: Boolean;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: Boolean);
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(AAdvertismentPicture: WideString);

    function GetUseEMailforStatusNotice: Boolean;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: Boolean);
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(AEMailforStatusNotice: WideString);
    function GetUseFilePassword: Boolean;
    procedure SetUseFilePassword(AUseFilePassword: Boolean);
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(AFilePassword: WideString);
    function GetUseAdminPassword: Boolean;
    procedure SetUseAdminPassword(AUseAdminPassword: Boolean);
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(AAdminPassword: WideString);
    function GetUseVisitorPassword: Boolean;
    procedure SetUseVisitorPassword(AUseVisitorPassword: Boolean);
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(AVisitorPassword: WideString);

    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    property Foldertypes: Integer read GetFoldertypes write SetFoldertypes;
    property ContainerTypes: Integer read GetContainerTypes write SetContainerTypes;
    property UseCaptcha: Boolean read GetUseCaptcha write SetUseCaptcha;
    property FolderName: WideString read GetFolderName write SetFolderName;

    property AdvertismentType: Integer read GetAdvertismentType write SetAdvertismentType;
    property AdvertismentLayerName: WideString read GetAdvertismentLayerName write SetAdvertismentLayerName;
    property AdvertismentLayerValue: WideString read GetAdvertismentLayerValue write SetAdvertismentLayerValue;
    property UseAdvertismentLink: Boolean read GetUseAdvertismentLink write SetUseAdvertismentLink;
    property AdvertismentLink: WideString read GetAdvertismentLink write SetAdvertismentLink;
    property UseAdvertismentPicture: Boolean read GetUseAdvertismentPicture write SetUseAdvertismentPicture;
    property AdvertismentPicture: WideString read GetAdvertismentPicture write SetAdvertismentPicture;

    property UseCoverLink: Boolean read GetUseCoverLink write SetUseCoverLink;
    property CoverLink: WideString read GetCoverLink write SetCoverLink;
    property UseDescription: Boolean read GetUseDescription write SetUseDescription;
    property Description: WideString read GetDescription write SetDescription;
    property UseCNL: Boolean read GetUseCNL write SetUseCNL;
    property UseWebseiteLink: Boolean read GetUseWebseiteLink write SetUseWebseiteLink;
    property WebseiteLink: WideString read GetWebseiteLink write SetWebseiteLink;

    property UseEMailforStatusNotice: Boolean read GetUseEMailforStatusNotice write SetUseEMailforStatusNotice;
    property EMailforStatusNotice: WideString read GetEMailforStatusNotice write SetEMailforStatusNotice;
    property UseFilePassword: Boolean read GetUseFilePassword write SetUseFilePassword;
    property FilePassword: WideString read GetFilePassword write SetFilePassword;
    property UseAdminPassword: Boolean read GetUseAdminPassword write SetUseAdminPassword;
    property AdminPassword: WideString read GetAdminPassword write SetAdminPassword;
    property UseVisitorPassword: Boolean read GetUseVisitorPassword write SetUseVisitorPassword;
    property VisitorPassword: WideString read GetVisitorPassword write SetVisitorPassword;

    function GenerateFolder(MirrorController: IMirrorControl): WideString;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); stdcall;
  end;

  IFileFormatPlugIn = interface(IPlugIn)
    ['{8A7373D1-51F3-4A98-912F-D3480274A715}']
    function GetForceAddCrypter: Boolean;
    procedure SetForceAddCrypter(AForceAddCrypter: Boolean);

    function GetFileFormatName: WideString; safecall;
    function CanSaveControls: Boolean; stdcall;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); stdcall;
    function CanLoadControls: Boolean; stdcall;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Boolean; stdcall;
    property ForceAddCrypter: Boolean read GetForceAddCrypter write SetForceAddCrypter;
  end;

  IFileHosterPlugIn = interface(IPlugIn)
    ['{609803F1-9479-4F92-A359-CAE8168104D5}']
    function CheckLink(AFile: WideString): TLinkInfo; stdcall;
    function CheckLinks(AFiles: WideString): Integer; stdcall;
    function CheckedLink(AIndex: Integer): TLinkInfo; stdcall;
  end;

  IImageHosterPlugIn = interface(IPlugIn)
    ['{F96C3D6F-CAAD-44E6-95D0-DEFBB6D9A867}']
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);
    function GetImageHostResize: TImageHostResize;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize);

    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function RemoteUpload(AImageUrl: WideString): WideString;
    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

  TLoadPlugIn = function(var PlugIn: IPlugIn): Boolean; stdcall;
  TLoadAppPlugIn = function(var PlugIn: IAppPlugIn): Boolean; stdcall;
  TLoadCAPTCHAPlugIn = function(var PlugIn: ICAPTCHAPlugIn): Boolean; stdcall;
  TLoadCMSPlugIn = function(var PlugIn: ICMSPlugIn): Boolean; stdcall;
  TLoadCrawlerPlugIn = function(var PlugIn: ICrawlerPlugIn): Boolean; stdcall;
  TLoadCrypterPlugIn = function(var PlugIn: ICrypterPlugIn): Boolean; stdcall;
  TLoadFileFormatPlugIn = function(var PlugIn: IFileFormatPlugIn): Boolean; stdcall;
  TLoadFileHosterPlugIn = function(var PlugIn: IFileHosterPlugIn): Boolean; stdcall;
  TLoadImageHosterPlugIn = function(var PlugIn: IImageHosterPlugIn): Boolean; stdcall;

implementation

end.
