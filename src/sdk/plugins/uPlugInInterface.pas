{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn interface                                    *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
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
  IPlugIn = interface(IUnknown)
    ['{D85F3CDC-641E-4103-9AD9-59FA514E2A27}']    
    function GetAuthor: WideString; safecall;
    function GetAuthorURL: WideString; safecall;
    function GetDescription: WideString; safecall;
    function GetName: WideString; safecall;
    function GetType: TPlugInType; safecall;
    
    function GetErrorMsg: WideString; safecall;

    procedure Initialize(const AHTTPManager: IHTTPManager; const AProxy: IProxy; const AConnectTimeout, AReadTimeout: Integer); safecall;

    property ErrorMsg: WideString read GetErrorMsg;
  end;
  
  IAccountData = interface(IUnknown)
    ['{CFDB39BC-F166-4785-AFEC-657DE1BEC080}']
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(const AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(const AAccountPassword: WideString); safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
  end;

  ICAPTCHAPlugIn = interface(IPlugIn)
    ['{02A910AE-7312-4D09-8194-61000E6F63D0}']
    function GetCAPTCHA: WideString; safecall;
    procedure SetCAPTCHA(const ACAPTCHA: WideString); safecall;
    function GetCAPTCHAType: TCAPTCHAType; safecall;
    procedure SetCAPTCHAType(ACAPTCHAType: TCAPTCHAType); safecall;
    function GetCAPTCHAName: WideString; safecall;
    procedure SetCAPTCHAName(const ACAPTCHAName: WideString); safecall;
    function GetCAPTCHAResult: WideString; safecall;
    procedure SetCAPTCHAResult(const ACAPTCHAResult: WideString); safecall;
    function GetCookies: WideString; safecall;
    procedure SetCookies(const ACookies: WideString); safecall;

    property CAPTCHA: WideString read GetCAPTCHA write SetCAPTCHA;
    property CAPTCHAType: TCAPTCHAType read GetCAPTCHAType write SetCAPTCHAType;
    property CAPTCHAName: WideString read GetCAPTCHAName write SetCAPTCHAName;
    property CAPTCHAResult: WideString read GetCAPTCHAResult write SetCAPTCHAResult;
    property Cookies: WideString read GetCookies write SetCookies;

    function Exec: WordBool; safecall;
  end;

  ICMSData = interface(IAccountData)
    ['{E26FB6F4-0E10-4822-A77F-30A39194BADB}']
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(const ASettingsFileName: WideString); safecall;

    function GetWebsite: WideString; safecall;
    procedure SetWebsite(const AWebsite: WideString); safecall;

    //function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;
    //procedure SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper); safecall;
    
    property SettingsFileName: WideString read GetSettingsFileName write SetSettingsFileName;

    property Website: WideString read GetWebsite write SetWebsite;

    //property IntelligentPostingHelper: TIntelligentPostingHelper read GetIntelligentPostingHelper;
  end;
  
  ICMSPublishData = interface(IUnknown)
    ['{0E7229CA-8010-4412-83D4-E57B6A2C2694}']
    function GetSubject: WideString; safecall;
    procedure SetSubject(const ASubject: WideString); safecall;
    function GetTags: WideString; safecall;
    procedure SetTags(const ATags: WideString); safecall;
    function GetMessage: WideString; safecall;
    procedure SetMessage(const AMessage: WideString); safecall;
    function GetData: ITabSheetData; safecall;
    procedure SetData(const AData: ITabSheetData); safecall;
    
    property Subject: WideString read GetSubject write SetSubject;
    property Tags: WideString read GetTags write SetTags;
    property Message: WideString read GetMessage write SetMessage;
    property Data: ITabSheetData read GetData write SetData;
  end;

  ICMSIDInfo = interface(IUnknown)
    ['{78BA5296-F0C4-450A-83CB-5AE879FD273C}']
    function GetValue: WideString; safecall;
    procedure SetValue(const AValue: WideString); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(const ADescription: WideString); safecall;

    property Value: WideString read GetValue write SetValue;
    property Description: WideString read GetDescription write SetDescription;
  end;

  ICMSIDsInfo = interface(IUnknown)
    ['{89BF279F-BF9F-48E8-BE21-272B30802250}']
    function GetCategoryType: WideString; safecall;
    function GetCount: Integer; safecall;
    function GetID(const AIndex: Integer): ICMSIDInfo; safecall;

    property CategoryType: WideString read GetCategoryType;
    property Count: Integer read GetCount;
    property ID[const AIndex: Integer]: ICMSIDInfo read GetID;
  end;

  ICMSPlugIn = interface(IPlugIn)
    ['{7ABEB7A9-89E9-4BDB-B81A-CAE6FAE42C16}']
    function GetCMSType: TCMSType; safecall;

    function GetDefaultCharset: WideString; safecall;
    function GetBelongsTo(const AWebsiteSourceCode: WideString): WordBool; safecall;

    function GetLogin(const ACMSData: ICMSData; out ARequestID: Double): WordBool; safecall;

    function GetRetrieveIDs(const ACategoryType: WideString; out ACMSIDsInfo: ICMSIDsInfo): WordBool; safecall;

    function AddArticle(const ACMSData: ICMSData; const ACMSPublishData: ICMSPublishData; out ACMSArticleInfo: TCMSArticleInfo): WordBool; safecall;
    function EditArticle(const ACMSData: ICMSData; const ACMSPublishData: ICMSPublishData; var ACMSArticleInfo: TCMSArticleInfo): WordBool; safecall;
    function DeleteArticle(const ACMSData: ICMSData; const ACMSArticleInfo: TCMSArticleInfo): WordBool; safecall;
    function GetArticle(const ACMSData: ICMSData; const ACMSArticleInfo: TCMSArticleInfo; out ACMSPublishData: ICMSPublishData): WordBool; safecall;

    function GetArticleLink(const AWebsite: WideString; const ACMSArticleInfo: TCMSArticleInfo): WideString; safecall;

    procedure Initialize(const AHTTPManager: IHTTPManager; const AProxy: IProxy; const AConnectTimeout, AReadTimeout: Integer; const AIntelligentPostingHelper: TIntelligentPostingHelper); safecall;

    function ShowWebsiteSettingsEditor(const AWebsiteEditor: IWebsiteEditor): WordBool; safecall;
  end;

  ICrawlerPlugIn = interface(IPlugIn)
    ['{81CCF306-EB14-4A7A-8D3B-37B64450B86B}']
    function GetAvailableTypeIDs: Integer; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; safecall;
    function GetDependentControlIDs: Integer; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; safecall;
    function GetResultsLimitDefaultValue: Integer; safecall;
    function GetRetrieveData(const AAccountData: IAccountData; const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; safecall;
  end;

  ICrypterData = interface(IAccountData)
    ['{42F989B8-4FB1-4C0F-A638-2AC9A021388F}']
    function GetUseCoverLink: WordBool; safecall;
    procedure SetUseCoverLink(AUseCoverLink: WordBool); safecall;
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(const ACoverLink: WideString); safecall;
    function GetUseDescription: WordBool; safecall;
    procedure SetUseDescription(AUseDescription: WordBool); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(const ADescription: WideString); safecall;
    function GetUseCNL: WordBool; safecall;
    procedure SetUseCNL(AUseCNL: WordBool); safecall;
    function GetUseWebseiteLink: WordBool; safecall;
    procedure SetUseWebseiteLink(AUseWebseiteLink: WordBool); safecall;
    function GetWebseiteLink: WideString; safecall;
    procedure SetWebseiteLink(const AWebseiteLink: WideString); safecall;

    function GetFoldertypes: Integer; safecall;
    procedure SetFoldertypes(AFoldertypes: Integer); safecall;
    function GetContainerTypes: Integer; safecall;
    procedure SetContainerTypes(AContainerTypes: Integer); safecall;
    function GetUseCaptcha: WordBool; safecall;
    procedure SetUseCaptcha(AUseCaptcha: WordBool); safecall;
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(const AFolderName: WideString); safecall;

    function GetAdvertismentType: Integer; safecall;
    procedure SetAdvertismentType(AAdvertismentType: Integer); safecall;
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(const AAdvertismentLayerName: WideString); safecall;
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(const AAdvertismentLayerValue: WideString); safecall;
    function GetUseAdvertismentLink: WordBool; safecall;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: WordBool); safecall;
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(const AAdvertismentLink: WideString); safecall;
    function GetUseAdvertismentPicture: WordBool; safecall;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool); safecall;
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(const AAdvertismentPicture: WideString); safecall;

    function GetUseEMailforStatusNotice: WordBool; safecall;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool); safecall;
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(const AEMailforStatusNotice: WideString); safecall;
    function GetUseFilePassword: WordBool; safecall;
    procedure SetUseFilePassword(AUseFilePassword: WordBool); safecall;
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(const AFilePassword: WideString); safecall;
    function GetUseAdminPassword: WordBool; safecall;
    procedure SetUseAdminPassword(AUseAdminPassword: WordBool); safecall;
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(const AAdminPassword: WideString); safecall;
    function GetUseVisitorPassword: WordBool; safecall;
    procedure SetUseVisitorPassword(AUseVisitorPassword: WordBool); safecall;
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(const AVisitorPassword: WideString); safecall;

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
  end;

  ICrypterPlugIn = interface(IPlugIn)
    ['{A96AAEFA-8248-4BDB-B909-E76E3C98E97F}']
    function GetServiceRequiresAccess: TCrypterAccess; safecall;
    function AddFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
    function EditFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
    function DeleteFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString): WordBool; safecall;
    function GetFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; safecall;
  end;

  ILinkInfo = interface(IUnknown)
    ['{8A038C53-090B-4E56-BAED-0DA906B50E51}']
    function GetLink: WideString; safecall;
    procedure SetLink(const ALink: WideString); safecall;
    function GetStatus: TLinkStatus; safecall;
    procedure SetStatus(const AStatus: TLinkStatus); safecall;
    function GetSize: UInt64; safecall;
    procedure SetSize(const ASize: UInt64); safecall;
    function GetFileName: WideString; safecall;
    procedure SetFileName(const AFileName: WideString); safecall;
    function GetChecksum: WideString; safecall;
    procedure SetChecksum(const AChecksum: WideString); safecall;
    function GetChecksumType: TChecksumType; safecall;
    procedure SetChecksumType(const AChecksumType: TChecksumType); safecall;

    property Link: WideString read GetLink write SetLink;
    property Status: TLinkStatus read GetStatus write SetStatus;
    property Size: UInt64 read GetSize write SetSize;
    property FileName: WideString read GetFileName write SetFileName;
    property Checksum: WideString read GetChecksum write SetChecksum;
    property ChecksumType: TChecksumType read GetChecksumType write SetChecksumType;
  end;

  ILinksInfo = interface(IUnknown)
    ['{8A038C53-090B-4E56-BAED-0DA906B50E51}']
    function GetCount: Integer; safecall;
    function GetLink(const AIndex: Integer): ILinkInfo; safecall;

    property Count: Integer read GetCount;
    property Link[const AIndex: Integer]: ILinkInfo read GetLink;
  end;

  IFileHosterPlugIn = interface(IPlugIn)
    ['{609803F1-9479-4F92-A359-CAE8168104D5}']
    function CheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; safecall;
    function CheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; safecall;
  end;

  IImageHosterData = interface(IAccountData)
    ['{AD2FFFF7-1470-4F85-A17E-5362691EA658}']
    function GetImageHostResize: TImageHostResize; safecall;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize); safecall;

    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

  IImageHosterPlugIn = interface(IPlugIn)
    ['{F96C3D6F-CAAD-44E6-95D0-DEFBB6D9A867}']
    function AddLocalImage(const AImageHosterData: IImageHosterData; const ALocalPath: WideString; out AUrl: WideString): WordBool; safecall;
    function AddWebImage(const AImageHosterData: IImageHosterData; const ARemoteUrl: WideString; out AUrl: WideString): WordBool; safecall;
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
