unit uPlugInInterface;

interface

uses
  // Common
  uBaseInterface,
  // HTTPManager
  uHTTPInterface,
  // Plugin
  uPlugInConst;

type
  IPlugIn = interface
    ['{D85F3CDC-641E-4103-9AD9-59FA514E2A27}']
    function GetCAPTCHAInput: TCAPTCHAInput; safecall;
    procedure SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput); safecall;
    function GetHTTPManager: IHTTPManager; safecall;
    procedure SetHTTPManager(AHTTPManager: IHTTPManager); safecall;
    function GetProxy: IProxy; safecall;
    procedure SetProxy(AProxy: IProxy); safecall;
    function GetConnectTimeout: Integer; safecall;
    procedure SetConnectTimeout(AConnectTimeout: Integer); safecall;
    function GetReadTimeout: Integer; safecall;
    procedure SetReadTimeout(AReadTimeout: Integer); safecall;
    function GetErrorMsg: WideString; safecall;
    procedure SetErrorMsg(AErrorMsg: WideString); safecall;

    function GetName: WideString; safecall;
    property CAPTCHAInput: TCAPTCHAInput read GetCAPTCHAInput;

    property HTTPManager: IHTTPManager read GetHTTPManager;
    property Proxy: IProxy read GetProxy write SetProxy;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;
  end;

  ICAPTCHAPlugIn = interface(IPlugIn)
    ['{02A910AE-7312-4D09-8194-61000E6F63D0}']
    function GetCAPTCHA: WideString; safecall;
    procedure SetCAPTCHA(ACAPTCHA: WideString); safecall;
    function GetCAPTCHAType: TCAPTCHAType; safecall;
    procedure SetCAPTCHAType(ACAPTCHAType: TCAPTCHAType); safecall;
    function GetCAPTCHAName: WideString; safecall;
    procedure SetCAPTCHAName(ACAPTCHAName: WideString); safecall;
    function GetCAPTCHAResult: WideString; safecall;
    procedure SetCAPTCHAResult(ACAPTCHAResult: WideString); safecall;
    function GetCookies: WideString; safecall;
    procedure SetCookies(ACookies: WideString); safecall;

    property CAPTCHA: WideString read GetCAPTCHA write SetCAPTCHA;
    property CAPTCHAType: TCAPTCHAType read GetCAPTCHAType write SetCAPTCHAType;
    property CAPTCHAName: WideString read GetCAPTCHAName write SetCAPTCHAName;
    property CAPTCHAResult: WideString read GetCAPTCHAResult write SetCAPTCHAResult;
    property Cookies: WideString read GetCookies write SetCookies;

    function Exec: WordBool; safecall;
  end;

  ICMSPlugIn = interface(IPlugIn)
    ['{7ABEB7A9-89E9-4BDB-B81A-CAE6FAE42C16}']
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(ASettingsFileName: WideString); safecall;
    function GetSubject: WideString; safecall;
    procedure SetSubject(ASubject: WideString); safecall;
    function GetTags: WideString; safecall;
    procedure SetTags(ATags: WideString); safecall;
    function GetMessage: WideString; safecall;
    procedure SetMessage(AMessage: WideString); safecall;
    function GetWebsite: WideString; safecall;
    procedure SetWebsite(AWebsite: WideString); safecall;
    function GetData: ITabSheetData; safecall;
    procedure SetData(const AData: ITabSheetData); safecall;

    function GetArticleID: Integer; safecall;
    procedure SetArticleID(AArticleID: Integer); safecall;

    function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;
    procedure SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper); safecall;

    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
    property SettingsFileName: WideString read GetSettingsFileName write SetSettingsFileName;
    property Subject: WideString read GetSubject write SetSubject;
    property Tags: WideString read GetTags write SetTags;
    property Message: WideString read GetMessage write SetMessage;
    property Website: WideString read GetWebsite write SetWebsite;
    property Data: ITabSheetData read GetData write SetData;

    property ArticleID: Integer read GetArticleID write SetArticleID;

    property IntelligentPostingHelper: TIntelligentPostingHelper read GetIntelligentPostingHelper;

    function CMSType: TCMSType; safecall;
    function DefaultCharset: WideString; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; safecall;
    function GetIDs: Integer; safecall;
    function ReadID(AIndex: Integer): TIDInfo; safecall;
    function Login(out ARequestID: Double): Boolean; safecall;
    function Exec: WordBool; safecall;
    function ShowWebsiteSettingsEditor(const AWebsiteEditor: IWebsiteEditor): WordBool; safecall;
  end;

  ICrawlerPlugIn = interface(IPlugIn)
    ['{81CCF306-EB14-4A7A-8D3B-37B64450B86B}']
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;

    function GetAvailableTypeIDs: Integer; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; safecall;
    function GetResultsLimitDefaultValue: Integer; safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); safecall;
  end;

  ICrypterPlugIn = interface(IPlugIn)
    ['{A96AAEFA-8248-4BDB-B909-E76E3C98E97F}']
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;

    function GetUseCoverLink: WordBool; safecall;
    procedure SetUseCoverLink(AUseCoverLink: WordBool); safecall;
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(ACoverLink: WideString); safecall;
    function GetUseDescription: WordBool; safecall;
    procedure SetUseDescription(AUseDescription: WordBool); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(ADescription: WideString); safecall;
    function GetUseCNL: WordBool; safecall;
    procedure SetUseCNL(AUseCNL: WordBool); safecall;
    function GetUseWebseiteLink: WordBool; safecall;
    procedure SetUseWebseiteLink(AUseWebseiteLink: WordBool); safecall;
    function GetWebseiteLink: WideString; safecall;
    procedure SetWebseiteLink(AWebseiteLink: WideString); safecall;

    function GetFoldertypes: Integer; safecall;
    procedure SetFoldertypes(AFoldertypes: Integer); safecall;
    function GetContainerTypes: Integer; safecall;
    procedure SetContainerTypes(AContainerTypes: Integer); safecall;
    function GetUseCaptcha: WordBool; safecall;
    procedure SetUseCaptcha(AUseCaptcha: WordBool); safecall;
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(AFolderName: WideString); safecall;

    function GetAdvertismentType: Integer; safecall;
    procedure SetAdvertismentType(AAdvertismentType: Integer); safecall;
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(AAdvertismentLayerName: WideString); safecall;
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(AAdvertismentLayerValue: WideString); safecall;
    function GetUseAdvertismentLink: WordBool; safecall;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: WordBool); safecall;
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(AAdvertismentLink: WideString); safecall;
    function GetUseAdvertismentPicture: WordBool; safecall;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool); safecall;
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(AAdvertismentPicture: WideString); safecall;

    function GetUseEMailforStatusNotice: WordBool; safecall;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool); safecall;
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(AEMailforStatusNotice: WideString); safecall;
    function GetUseFilePassword: WordBool; safecall;
    procedure SetUseFilePassword(AUseFilePassword: WordBool); safecall;
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(AFilePassword: WideString); safecall;
    function GetUseAdminPassword: WordBool; safecall;
    procedure SetUseAdminPassword(AUseAdminPassword: WordBool); safecall;
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(AAdminPassword: WideString); safecall;
    function GetUseVisitorPassword: WordBool; safecall;
    procedure SetUseVisitorPassword(AUseVisitorPassword: WordBool); safecall;
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(AVisitorPassword: WideString); safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    property Foldertypes: Integer read GetFoldertypes write SetFoldertypes;
    property ContainerTypes: Integer read GetContainerTypes write SetContainerTypes;
    property UseCaptcha: WordBool read GetUseCaptcha write SetUseCaptcha;
    property FolderName: WideString read GetFolderName write SetFolderName;

    property AdvertismentType: Integer read GetAdvertismentType write SetAdvertismentType;
    property AdvertismentLayerName: WideString read GetAdvertismentLayerName write SetAdvertismentLayerName;
    property AdvertismentLayerValue: WideString read GetAdvertismentLayerValue write SetAdvertismentLayerValue;
    property UseAdvertismentLink: WordBool read GetUseAdvertismentLink write SetUseAdvertismentLink;
    property AdvertismentLink: WideString read GetAdvertismentLink write SetAdvertismentLink;
    property UseAdvertismentPicture: WordBool read GetUseAdvertismentPicture write SetUseAdvertismentPicture;
    property AdvertismentPicture: WideString read GetAdvertismentPicture write SetAdvertismentPicture;

    property UseCoverLink: WordBool read GetUseCoverLink write SetUseCoverLink;
    property CoverLink: WideString read GetCoverLink write SetCoverLink;
    property UseDescription: WordBool read GetUseDescription write SetUseDescription;
    property Description: WideString read GetDescription write SetDescription;
    property UseCNL: WordBool read GetUseCNL write SetUseCNL;
    property UseWebseiteLink: WordBool read GetUseWebseiteLink write SetUseWebseiteLink;
    property WebseiteLink: WideString read GetWebseiteLink write SetWebseiteLink;

    property UseEMailforStatusNotice: WordBool read GetUseEMailforStatusNotice write SetUseEMailforStatusNotice;
    property EMailforStatusNotice: WideString read GetEMailforStatusNotice write SetEMailforStatusNotice;
    property UseFilePassword: WordBool read GetUseFilePassword write SetUseFilePassword;
    property FilePassword: WideString read GetFilePassword write SetFilePassword;
    property UseAdminPassword: WordBool read GetUseAdminPassword write SetUseAdminPassword;
    property AdminPassword: WideString read GetAdminPassword write SetAdminPassword;
    property UseVisitorPassword: WordBool read GetUseVisitorPassword write SetUseVisitorPassword;
    property VisitorPassword: WideString read GetVisitorPassword write SetVisitorPassword;

    function AddFolder(const AMirrorContainer: IMirrorContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
    function EditFolder(const AMirrorContainer: IMirrorContainer; ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
  end;

  IFileHosterPlugIn = interface(IPlugIn)
    ['{609803F1-9479-4F92-A359-CAE8168104D5}']
    function CheckLink(AFile: WideString): TLinkInfo; safecall;
    function CheckLinks(AFiles: WideString): Integer; safecall;
    function CheckedLink(AIndex: Integer): TLinkInfo; safecall;
  end;

  IImageHosterPlugIn = interface(IPlugIn)
    ['{F96C3D6F-CAAD-44E6-95D0-DEFBB6D9A867}']
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;
    function GetImageHostResize: TImageHostResize; safecall;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize); safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function LocalUpload(ALocalPath: WideString): WideString; safecall;
    function RemoteUpload(AImageUrl: WideString): WideString; safecall;

    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

  TLoadPlugIn = function(var APlugIn: IPlugIn): WordBool; safecall;
  TLoadCAPTCHAPlugIn = function(var APlugIn: ICAPTCHAPlugIn): WordBool; safecall;
  TLoadCMSPlugIn = function(var APlugIn: ICMSPlugIn): WordBool; safecall;
  TLoadCrawlerPlugIn = function(var APlugIn: ICrawlerPlugIn): WordBool; safecall;
  TLoadCrypterPlugIn = function(var APlugIn: ICrypterPlugIn): WordBool; safecall;
  TLoadFileHosterPlugIn = function(var APlugIn: IFileHosterPlugIn): WordBool; safecall;
  TLoadImageHosterPlugIn = function(var APlugIn: IImageHosterPlugIn): WordBool; safecall;

implementation

end.
