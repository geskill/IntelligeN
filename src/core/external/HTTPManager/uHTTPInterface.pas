unit uHTTPInterface;

interface

uses
  // Delphi
  ActiveX,
  // HTTPManager
  uHTTPConst;

type
  ICOMList = interface
    ['{A6545E23-E220-4965-9841-4038704C29B5}']
    function GetName(Index: Integer): WideString; safecall;
    function GetValue(const Name: WideString): WideString; safecall;
    procedure SetValue(const Name, Value: WideString); safecall;
    function GetValueFromIndex(Index: Integer): WideString; safecall;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString); safecall;
    function Get(Index: Integer): WideString; safecall;
    function GetCount: Integer; safecall;
    procedure Put(Index: Integer; const S: WideString); safecall;
    function Add(const S: WideString): Integer; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    function GetText: WideString; safecall;
    function IndexOf(const S: WideString): Integer; safecall;
    function IndexOfName(const Name: WideString): Integer; safecall;
    procedure Insert(Index: Integer; const S: WideString); safecall;
    procedure SetText(const Text: WideString); safecall;

    property Count: Integer read GetCount;
    property Names[Index: Integer]: WideString read GetName;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property Strings[Index: Integer]: WideString read Get write Put;
    property Text: WideString read GetText write SetText;
  end;

  IProxy = interface
    ['{CE0B1CFC-8158-42E9-8E90-0D5B1BB1F354}']
    function GetActive: WordBool; safecall;
    function GetType: TProxyType; safecall;
    function GetServer: WideString; safecall;
    function GetPort: Integer; safecall;
    function GetRequireAuthentication: WordBool; safecall;
    function GetAccountName: WideString; safecall;
    function GetAccountPassword: WideString; safecall;

    procedure Activate(AType: TProxyType; const AServer: WideString; APort: Integer; ARequireAuthentication: WordBool; const AAccountName, AAccountPassword: WideString); safecall;
    property Active: WordBool read GetActive;
    property ServerType: TProxyType read GetType;
    property Server: WideString read GetServer;
    property Port: Integer read GetPort;
    property RequireAuthentication: WordBool read GetRequireAuthentication;
    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;
  end;

  IHTTPParams = interface
    ['{9A880BE3-3C50-4DA0-B9EB-9B836D10143B}']
    function GetFieldName(Index: Integer): WideString; safecall;
    function GetFieldValue(const AFieldName: WideString): WideString; safecall;
    procedure SetFieldValue(const AFieldName, AFieldValue: WideString); safecall;
    function GetFieldValueFromIndex(Index: Integer): WideString; safecall;
    procedure SetFieldValueFromIndex(Index: Integer; const AFieldValue: WideString); safecall;
    function GetFileName(const AFieldName: WideString): WideString; safecall;
    procedure SetFileName(const AFieldName, AFileName: WideString); safecall;
    function GetFileNameFromIndex(Index: Integer): WideString; safecall;
    procedure SetFileNameFromIndex(Index: Integer; const AFileName: WideString); safecall;

    function GetRawData: WideString; safecall;
    procedure SetRawData(const ARawData: WideString); safecall;
    function GetParamType: TParamType; safecall;
    procedure SetParamType(AParamType: TParamType); safecall;
    function GetCount: Integer; safecall;
    function GetIsFile(Index: Integer): WordBool; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;

    procedure AddFile(const AFieldName, AFileName: WideString); safecall;
    procedure AddFormField(const AFieldName, AFieldValue: WideString); overload; safecall;
    procedure AddFormField(const AFieldName, AFieldValue, AFileName: WideString); overload; safecall;

    property Count: Integer read GetCount;
    property IsFile[Index: Integer]: WordBool read GetIsFile;
    property FieldNames[Index: Integer]: WideString read GetFieldName;
    property FieldValue[const FieldName: WideString]: WideString read GetFieldValue write SetFieldValue;
    property FieldValueFromIndex[Index: Integer]: WideString read GetFieldValueFromIndex write SetFieldValueFromIndex;
    property FieldFileName[const FieldName: WideString]: WideString read GetFileName write SetFileName;
    property FieldFileNameFromIndex[Index: Integer]: WideString read GetFileNameFromIndex write SetFileNameFromIndex;

    property RawData: WideString read GetRawData write SetRawData;

    property ParamType: TParamType read GetParamType write SetParamType;
  end;

  IHTTPHeader = interface
    ['{D55CB024-D568-41A2-880E-F7CF0A1EEE9C}']
    function GetCookies: ICOMList; safecall;
    procedure SetCookies(const ACookies: ICOMList); safecall;

    function GetCacheControl: WideString; safecall;
    procedure SetCacheControl(const ACacheControl: WideString); safecall;
    function GetCharSet: WideString; safecall;
    procedure SetCharSet(const ACharSet: WideString); safecall;
    function GetConnection: WideString; safecall;
    procedure SetConnection(const AConnection: WideString); safecall;
    function GetContentDisposition: WideString; safecall;
    procedure SetContentDisposition(const AContentDisposition: WideString); safecall;
    function GetContentEncoding: WideString; safecall;
    procedure SetContentEncoding(const AContentEncoding: WideString); safecall;
    function GetContentLanguage: WideString; safecall;
    procedure SetContentLanguage(const AContentLanguage: WideString); safecall;

    function GetContentType: WideString; safecall;
    procedure SetContentType(const AContentType: WideString); safecall;

    function GetCustomHeaders: ICOMList; safecall;
    procedure SetCustomHeaders(const ACustomHeaders: ICOMList); safecall;

    property Cookies: ICOMList read GetCookies write SetCookies;

    property CacheControl: WideString read GetCacheControl write SetCacheControl;
    property CharSet: WideString read GetCharSet write SetCharSet;
    property Connection: WideString read GetConnection write SetConnection;
    property ContentDisposition: WideString read GetContentDisposition write SetContentDisposition;
    property ContentEncoding: WideString read GetContentEncoding write SetContentEncoding;
    property ContentLanguage: WideString read GetContentLanguage write SetContentLanguage;

    property ContentType: WideString read GetContentType write SetContentType;
    // property ContentVersion

    property CustomHeaders: ICOMList read GetCustomHeaders write SetCustomHeaders;

    // property Date
    // property ETag
    // property Expires
    // property LastModified
    // property Pragma
    // property TransferEncoding
  end;

  IHTTPRequest = interface(IHTTPHeader)
    ['{4D6DCF40-E18D-4CB5-87AC-572C0686AA21}']
    function GetURL: WideString; safecall;
    procedure SetURL(const AURL: WideString); safecall;
    function GetMethod: THTTPMethod; safecall;
    procedure SetMethod(AMethod: THTTPMethod); safecall;

    function GetAccept: WideString; safecall;
    procedure SetAccept(const AAccept: WideString); safecall;
    function GetAcceptCharSet: WideString; safecall;
    procedure SetAcceptCharSet(const AAcceptCharSet: WideString); safecall;
    function GetAcceptEncoding: WideString; safecall;
    procedure SetAcceptEncoding(const AAcceptEncoding: WideString); safecall;
    function GetAcceptLanguage: WideString; safecall;
    procedure SetAcceptLanguage(const AAcceptLanguage: WideString); safecall;
    function GetHost: WideString; safecall;
    procedure SetHost(const AHost: WideString); safecall;
    function GetReferer: WideString; safecall;
    procedure SetReferer(const AReferer: WideString); safecall;
    function GetUserAgent: WideString; safecall;
    procedure SetUserAgent(const AUserAgent: WideString); safecall;

    property URL: WideString read GetURL write SetURL;
    property Method: THTTPMethod read GetMethod write SetMethod;

    property Accept: WideString read GetAccept write SetAccept;
    property AcceptCharSet: WideString read GetAcceptCharSet write SetAcceptCharSet;
    property AcceptEncoding: WideString read GetAcceptEncoding write SetAcceptEncoding;
    property AcceptLanguage: WideString read GetAcceptLanguage write SetAcceptLanguage;
    property Host: WideString read GetHost write SetHost;
    property Referer: WideString read GetReferer write SetReferer;
    property UserAgent: WideString read GetUserAgent write SetUserAgent;
  end;

  IHTTPOptions = interface
    ['{6B0335B1-1EE2-425C-8D6D-08A8DA998C75}']
    function GetUseCompressor: WordBool; safecall;
    procedure SetUseCompressor(AUseCompressor: WordBool); safecall;
    function GetProxy: IProxy; safecall;
    procedure SetProxy(AProxy: IProxy); safecall;
    function GetConnectTimeout: Integer; safecall;
    procedure SetConnectTimeout(AConnectTimeout: Integer); safecall;
    function GetReadTimeout: Integer; safecall;
    procedure SetReadTimeout(AReadTimeout: Integer); safecall;
    function GetHandleRedirects: WordBool; safecall;
    procedure SetHandleRedirects(AHandleRedirects: WordBool); safecall;
    function GetHandleSketchyRedirects: WordBool; safecall;
    procedure SetHandleSketchyRedirects(AHandleSketchyRedirects: WordBool); safecall;
    function GetRedirectMaximum: Integer; safecall;
    procedure SetRedirectMaximum(ARedirectMaximum: Integer); safecall;

    property UseCompressor: WordBool read GetUseCompressor write SetUseCompressor;

    property Proxy: IProxy read GetProxy write SetProxy;

    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;

    property HandleRedirects: WordBool read GetHandleRedirects write SetHandleRedirects;
    property HandleSketchyRedirects: WordBool read GetHandleSketchyRedirects write SetHandleSketchyRedirects;
    property RedirectMaximum: Integer read GetRedirectMaximum write SetRedirectMaximum;
  end;

  IHTTPData = interface
    ['{F1ED2AAD-88F0-45BE-B9F2-E01E01616608}']
    function GetWebsite: WideString; safecall;
    function GetHTTPParams: IHTTPParams; safecall;
    procedure SetHTTPParams(const AHTTPParams: IHTTPParams); safecall;
    function GetHTTPRequest: IHTTPRequest; safecall;
    procedure SetHTTPRequest(const AHTTPRequest: IHTTPRequest); safecall;
    function GetHTTPOptions: IHTTPOptions; safecall;
    procedure SetHTTPOptions(const AHTTPOptions: IHTTPOptions); safecall;

    property Website: WideString read GetWebsite;
    property HTTPParams: IHTTPParams read GetHTTPParams write SetHTTPParams;
    property HTTPRequest: IHTTPRequest read GetHTTPRequest write SetHTTPRequest;
    property HTTPOptions: IHTTPOptions read GetHTTPOptions write SetHTTPOptions;
  end;

  IHTTPResponse = interface(IHTTPHeader)
    ['{44B6F40F-0E1E-48B7-B8AE-BD7B5ABECBC9}']
    function GetLocation: WideString; safecall;
    procedure SetLocation(const ALocation: WideString); safecall;
    function GetRefresh: WideString; safecall;
    procedure SetRefresh(const ARefresh: WideString); safecall;

    function GetText: WideString; safecall;
    procedure SetText(const AText: WideString); safecall;
    function GetCode: Integer; safecall;
    procedure SetCode(ACode: Integer); safecall;

    function GetServer: WideString; safecall;
    procedure SetServer(const AServer: WideString); safecall;
    function GetContent: WideString; safecall;
    procedure SetContent(const AContent: WideString); safecall;
    function GetContentStream: IStream; safecall;
    procedure SetContentStream(const AContentStream: IStream); safecall;

    // property AcceptRanges
    property Location: WideString read GetLocation write SetLocation;
    property Refresh: WideString read GetRefresh write SetRefresh;

    property Text: WideString read GetText write SetText;
    property Code: Integer read GetCode write SetCode;

    property Server: WideString read GetServer write SetServer;
    property Content: WideString read GetContent write SetContent;
    property ContentStream: IStream read GetContentStream write SetContentStream;
  end;

  IHTTPResponseInfo = interface
    ['{85F3C295-2132-487A-ACE1-931FD9FDFCC2}']
    function GetLastRedirect: WideString; safecall;
    procedure SetLastRedirect(const ALastRedirect: WideString); safecall;
    function GetRedirectCount: Integer; safecall;
    procedure SetRedirectCount(ARedirectCount: Integer); safecall;
    function GetErrorClassName: WideString; safecall;
    procedure SetErrorClassName(const AErrorClassName: WideString); safecall;
    function GetErrorMessage: WideString; safecall;
    procedure SetErrorMessage(const AErrorMessage: WideString); safecall;

    property LastRedirect: WideString read GetLastRedirect write SetLastRedirect;
    property RedirectCount: Integer read GetRedirectCount write SetRedirectCount;

    property ErrorClassName: WideString read GetErrorClassName write SetErrorClassName;
    property ErrorMessage: WideString read GetErrorMessage write SetErrorMessage;
  end;

  IHTTPResult = interface
    ['{D474F5DD-36FA-4971-9DF0-F89EEF750379}']
    function GetSourceCode: WideString; safecall;
    function GetHasError: WordBool; safecall;
    function GetHTTPResponse: IHTTPResponse; safecall;
    function GetHTTPResponseInfo: IHTTPResponseInfo; safecall;

    property SourceCode: WideString read GetSourceCode;
    property HasError: WordBool read GetHasError;
    property HTTPResponse: IHTTPResponse read GetHTTPResponse;
    property HTTPResponseInfo: IHTTPResponseInfo read GetHTTPResponseInfo;
  end;

  IHTTPProcess = interface
    ['{93FDB9BC-F325-4E98-B548-DF5D9C339189}']
    function GetUniqueID: Double; safecall;
    function GetHTTPData: IHTTPData; safecall;
    procedure SetHTTPData(const AHTTPData: IHTTPData); safecall;
    function GetHTTPResult: IHTTPResult; safecall;
    procedure SetHTTPResult(const AHTTPResult: IHTTPResult); safecall;

    property UniqueID: Double read GetUniqueID;
    property HTTPData: IHTTPData read GetHTTPData write SetHTTPData;
    property HTTPResult: IHTTPResult read GetHTTPResult write SetHTTPResult;
  end;

  IHTTPProcessEventHandler = interface(IUnknown)
    ['{2FBF518D-3E91-4BFF-9713-D9BC872F1813}']
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

  IHTTPProcessEvent = interface(IUnknown)
    ['{058E81B9-E661-4C58-A56F-4F0D41D4350B}']
    procedure Add(const AHandler: IHTTPProcessEventHandler); safecall;
    procedure Remove(const AHandler: IHTTPProcessEventHandler); safecall;
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

  IHTTPAntiScrape = interface(IUnknown)
    ['{03C8BCF2-87B8-4665-9119-4E63EB5EEE2C}']
    function GetName: WideString; safecall;

    procedure Handle(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); safecall;

    property Name: WideString read GetName;
  end;

  IHTTPAntiScrapeManager = interface(IUnknown)
    ['{D5BE5CB3-CE28-42F1-8009-48E9254CC6CC}']
    function GetCount: Integer; safecall;
    function GetAntiScrape(AIndex: Integer): IHTTPAntiScrape; safecall;

    function Register(const AHTTPAntiScrape: IHTTPAntiScrape): WordBool; safecall;
    function Unregister(const AName: WideString): WordBool; safecall;

    property Count: Integer read GetCount;
    property AntiScrapes[Index: Integer]: IHTTPAntiScrape read GetAntiScrape; default;
  end;

  IHTTPImplementation = interface(IUnknown)
    ['{4C83C736-0779-4355-A26C-96ADAB6909F2}']
    function GetName: WideString; safecall;

    procedure Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult); safecall;

    property Name: WideString read GetName;
  end;

  IHTTPImplementationManager = interface(IUnknown)
    ['{9908E7A3-E2B5-40F4-9412-723F0D4686E6}']
    function GetCount: Integer; safecall;
    function GetImplementation(AIndex: Integer): IHTTPImplementation; safecall;

    function Register(const AHTTPImplementation: IHTTPImplementation): WordBool; safecall;
    function Unregister(const AName: WideString): WordBool; safecall;

    property Count: Integer read GetCount;
    property Implementations[Index: Integer]: IHTTPImplementation read GetImplementation; default;
  end;

  IHTTPManager = interface
    ['{DB7FBA4F-CE5C-454A-AA74-FB8EC7DFAB8E}']
    function GetConnectionMaximum: Integer; safecall;
    procedure SetConnectionMaximum(const AConnectionMaximum: Integer); safecall;
    function GetAntiScrapeManager: IHTTPAntiScrapeManager; safecall;
    function GetImplementor: IHTTPImplementation; safecall;
    procedure SetImplementor(const AImplementor: IHTTPImplementation); safecall;
    function GetImplementationManager: IHTTPImplementationManager; safecall;
    function GetRequestDone: IHTTPProcessEvent; safecall;

    property ConnectionMaximum: Integer read GetConnectionMaximum write SetConnectionMaximum;

    function Get(const AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    function Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    function Post(const AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    function Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;

    function HasResult(AUniqueID: Double): WordBool; safecall;
    function GetResult(AUniqueID: Double): IHTTPProcess; safecall;

    function WaitFor(AUniqueID: Double; AMaxWaitMS: Cardinal = INFINITE): WordBool; safecall;

    property AntiScrapeManager: IHTTPAntiScrapeManager read GetAntiScrapeManager;

    property Implementor: IHTTPImplementation read GetImplementor write SetImplementor;
    property ImplementationManager: IHTTPImplementationManager read GetImplementationManager;

    property OnRequestDone: IHTTPProcessEvent read GetRequestDone;
  end;

implementation

end.
