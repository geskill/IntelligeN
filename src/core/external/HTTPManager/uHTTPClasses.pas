unit uHTTPClasses;

interface

uses
  // Interface
  uHTTPInterface,
  // Const
  uHTTPConst,
  // Delphi
  SysUtils, Classes, ActiveX, Generics.Collections;

type
  TCOMList = class(TInterfacedObject, ICOMList)
  private
    FStringList: TStringList;
  protected
    function GetName(Index: Integer): WideString; safecall;
    function GetValue(const Name: WideString): WideString; safecall;
    procedure SetValue(const Name, Value: WideString); safecall;
    function GetValueFromIndex(Index: Integer): WideString; safecall;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString); safecall;
    function Get(Index: Integer): WideString; safecall;
    function GetCount: Integer; safecall;
    function GetText: WideString; safecall;
    procedure SetText(const Text: WideString); safecall;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    procedure Put(Index: Integer; const S: WideString); safecall;
    function Add(const S: WideString): Integer; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    function IndexOf(const S: WideString): Integer; safecall;
    function IndexOfName(const Name: WideString): Integer; safecall;
    procedure Insert(Index: Integer; const S: WideString); safecall;

    property Count: Integer read GetCount;
    property Names[Index: Integer]: WideString read GetName;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property Strings[Index: Integer]: WideString read Get write Put;
    property Text: WideString read GetText write SetText;
  end;

  TProxy = class(TInterfacedObject, IProxy)
  private
    FActive, FRequireAuthentication: WordBool;
    FType: TProxyType;
    FPort: Integer;
    FServer, FAccountName, FAccountPassword: WideString;
  protected
    function GetActive: WordBool; safecall;
    function GetType: TProxyType; safecall;
    function GetServer: WideString; safecall;
    function GetPort: Integer; safecall;
    function GetRequireAuthentication: WordBool; safecall;
    function GetAccountName: WideString; safecall;
    function GetAccountPassword: WideString; safecall;
  public
    constructor Create; reintroduce; virtual;
    constructor Clone(const AProxy: IProxy);
    destructor Destroy; override;

    procedure Activate(AType: TProxyType; const AServer: WideString; APort: Integer; ARequireAuthentication: WordBool; const AAccountName, AAccountPassword: WideString); safecall;
    property Active: WordBool read GetActive;
    property ServerType: TProxyType read GetType;
    property Server: WideString read GetServer;
    property Port: Integer read GetPort;
    property RequireAuthentication: WordBool read GetRequireAuthentication;
    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;
  end;

  TFieldType = (ftFormField, ftFile, ftFileStream);

  THTTPParam = class(TObject)
  private
    FFieldName, FFieldValue, FFieldFileName: string;
    FFieldType: TFieldType;
  public
    constructor Create(const AFieldName, AFieldValue: string; AFieldType: TFieldType = ftFormField; AFileName: string = ''); reintroduce; virtual;
    destructor Destroy; override;

    property FieldName: string read FFieldName write FFieldName;
    property FieldValue: string read FFieldValue write FFieldValue;
    property FieldType: TFieldType read FFieldType write FFieldType;
    property FieldFileName: string read FFieldFileName write FFieldFileName;
  end;

  THTTPParams = class(TInterfacedObject, IHTTPParams)
  private
    FHTTPParamList: TList<THTTPParam>;
    FRawData: WideString;
    FParamType: TParamType;
    FHasFile: Boolean;
    function GetFieldIndex(const AFieldName: WideString): Integer;
  protected
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
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AParamType: TParamType); reintroduce; overload; virtual;
    constructor Create(ARawData: WideString); reintroduce; overload; virtual;
    constructor Clone(const HTTPParams: IHTTPParams); overload;
    destructor Destroy; override;

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

  THTTPHeader = class(TInterfacedObject, IHTTPHeader)
  private
    FCacheControl, FCharSet, FConnection, FContentDisposition, FContentEncoding, FContentLanguage, FContentType: WideString;
    FCookies, FCustomHeaders: ICOMList;
  protected
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
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    property Cookies: ICOMList read GetCookies write SetCookies;

    property CacheControl: WideString read GetCacheControl write SetCacheControl;
    property CharSet: WideString read GetCharSet write SetCharSet;
    property Connection: WideString read GetConnection write SetConnection;
    property ContentDisposition: WideString read GetContentDisposition write SetContentDisposition;
    property ContentEncoding: WideString read GetContentEncoding write SetContentEncoding;
    property ContentLanguage: WideString read GetContentLanguage write SetContentLanguage;

    property ContentType: WideString read GetContentType write SetContentType;

    property CustomHeaders: ICOMList read GetCustomHeaders write SetCustomHeaders;
  end;

  THTTPRequest = class(THTTPHeader, IHTTPRequest)
  private
    FURL, FAccept, FAcceptCharSet, FAcceptEncoding, FAcceptLanguage, FHost, FReferer, FUserAgent: WideString;
    FMethod: THTTPMethod;
    FMethodDefined: Boolean;
  protected
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
  public
    constructor Create(const AURL: string); reintroduce; virtual;
    constructor Clone(const AHTTPRequest: IHTTPRequest);
    constructor FollowUpClone(const AHTTPProcess: IHTTPProcess; const ANewHTTPProcess: IHTTPRequest); overload;
    constructor FollowUpClone(const AHTTPProcess: IHTTPProcess; AHTTPMethod: THTTPMethod; AURL: string); overload;
    destructor Destroy; override;

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

  THTTPResponse = class(THTTPHeader, IHTTPResponse)
  private
    FLocation, FRefresh, FText, FServer, FContent: WideString;
    FCode: Integer;
    FContentStream: IStream;
  protected
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
  public
    constructor Create(const AContentStream: TMemoryStream); reintroduce; virtual;
    destructor Destroy; override;

    property Location: WideString read GetLocation write SetLocation;
    property Refresh: WideString read GetRefresh write SetRefresh;

    property Text: WideString read GetText write SetText;
    property Code: Integer read GetCode write SetCode;

    property Server: WideString read GetServer write SetServer;
    property Content: WideString read GetContent write SetContent;
    property ContentStream: IStream read GetContentStream write SetContentStream;
  end;

  THTTPOptions = class(TInterfacedObject, IHTTPOptions)
  private
    FUseCompressor, FHandleRedirects, FHandleSketchyRedirects: WordBool;
    FProxy: IProxy;
    FConnectTimeout, FReadTimeout, FRedirectMaximum: Integer;
  protected
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
  public
    constructor Create(const AProxy: IProxy = nil); reintroduce; virtual;
    constructor Clone(const AHTTPOptions: IHTTPOptions);
    destructor Destroy; override;

    property UseCompressor: WordBool read GetUseCompressor write SetUseCompressor;

    property Proxy: IProxy read GetProxy write SetProxy;

    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;

    property HandleRedirects: WordBool read GetHandleRedirects write SetHandleRedirects;
    property HandleSketchyRedirects: WordBool read GetHandleSketchyRedirects write SetHandleSketchyRedirects;
    property RedirectMaximum: Integer read GetRedirectMaximum write SetRedirectMaximum;
  end;

  THTTPResponseInfo = class(TInterfacedObject, IHTTPResponseInfo)
  private
    FLastRedirect, FErrorClassName, FErrorMessage: WideString;
    FRedirectCount: Integer;
  protected
    function GetLastRedirect: WideString; safecall;
    procedure SetLastRedirect(const ALastRedirect: WideString); safecall;
    function GetRedirectCount: Integer; safecall;
    procedure SetRedirectCount(ARedirectCount: Integer); safecall;
    function GetErrorClassName: WideString; safecall;
    procedure SetErrorClassName(const AErrorClassName: WideString); safecall;
    function GetErrorMessage: WideString; safecall;
    procedure SetErrorMessage(const AErrorMessage: WideString); safecall;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    property LastRedirect: WideString read GetLastRedirect write SetLastRedirect;
    property RedirectCount: Integer read GetRedirectCount write SetRedirectCount;
    property ErrorClassName: WideString read GetErrorClassName write SetErrorClassName;
    property ErrorMessage: WideString read GetErrorMessage write SetErrorMessage;
  end;

implementation

{ TCOMList }

constructor TCOMList.Create;
begin
  inherited Create;
  FStringList := TStringList.Create;
end;

destructor TCOMList.Destroy;
begin
  FStringList.Free;
  inherited Destroy;
end;

function TCOMList.GetName(Index: Integer): WideString;
begin
  Result := FStringList.Names[Index];
end;

function TCOMList.GetValue(const Name: WideString): WideString;
begin
  Result := FStringList.Values[Name];
end;

procedure TCOMList.SetValue(const Name, Value: WideString);
begin
  FStringList.Values[Name] := Value;
end;

function TCOMList.GetValueFromIndex(Index: Integer): WideString;
begin
  Result := FStringList.ValueFromIndex[Index];
end;

procedure TCOMList.SetValueFromIndex(Index: Integer; const Value: WideString);
begin
  FStringList.ValueFromIndex[Index] := Value;
end;

function TCOMList.Get(Index: Integer): WideString;
begin
  Result := FStringList.Strings[Index];
end;

function TCOMList.GetCount: Integer;
begin
  Result := FStringList.Count;
end;

function TCOMList.GetText: WideString;
begin
  Result := FStringList.Text;
end;

procedure TCOMList.SetText(const Text: WideString);
begin
  FStringList.Text := Text;
end;

procedure TCOMList.Put(Index: Integer; const S: WideString);
begin
  FStringList.Strings[Index] := S;
end;

function TCOMList.Add(const S: WideString): Integer;
begin
  Result := FStringList.Add(S);
end;

procedure TCOMList.Clear;
begin
  FStringList.Clear;
end;

procedure TCOMList.Delete(Index: Integer);
begin
  FStringList.Delete(Index);
end;

function TCOMList.IndexOf(const S: WideString): Integer;
begin
  Result := FStringList.IndexOf(S);
end;

function TCOMList.IndexOfName(const Name: WideString): Integer;
begin
  Result := FStringList.IndexOfName(Name)
end;

procedure TCOMList.Insert(Index: Integer; const S: WideString);
begin
  FStringList.Insert(Index, S);
end;

{ TProxy }

constructor TProxy.Create;
begin
  inherited Create;
  FActive := False;
  FServer := '';
  FPort := 0;
  FAccountName := '';
  FAccountPassword := '';
end;

constructor TProxy.Clone(const AProxy: IProxy);
begin
  Create;

  FActive := AProxy.Active;
  FType := AProxy.ServerType;
  FServer := AProxy.Server;
  FPort := AProxy.Port;
  FRequireAuthentication := AProxy.RequireAuthentication;
  FAccountName := AProxy.AccountName;
  FAccountPassword := AProxy.AccountPassword;
end;

destructor TProxy.Destroy;
begin
  inherited Destroy;
end;

procedure TProxy.Activate;
begin
  FActive := True;
  FType := AType;
  FServer := AServer;
  FPort := APort;
  FRequireAuthentication := ARequireAuthentication;
  FAccountName := AAccountName;
  FAccountPassword := AAccountPassword;
end;

function TProxy.GetActive: WordBool;
begin
  Result := FActive;
end;

function TProxy.GetType;
begin
  Result := FType;
end;

function TProxy.GetServer: WideString;
begin
  Result := FServer;
end;

function TProxy.GetPort: Integer;
begin
  Result := FPort;
end;

function TProxy.GetRequireAuthentication: WordBool;
begin
  Result := FRequireAuthentication;
end;

function TProxy.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

function TProxy.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

{ THTTPParam }

constructor THTTPParam.Create(const AFieldName, AFieldValue: string; AFieldType: TFieldType = ftFormField; AFileName: string = '');
begin
  inherited Create;
  FFieldName := AFieldName;
  FFieldValue := AFieldValue;
  FFieldType := AFieldType;
  FFieldFileName := AFileName;
end;

destructor THTTPParam.Destroy;
begin
  inherited Destroy;
end;

{ THTTPParams }

function THTTPParams.GetFieldIndex(const AFieldName: WideString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FHTTPParamList.Count - 1 do
    if SameText(AFieldName, FHTTPParamList.Items[I].FieldName) then
      Exit(I);
end;

function THTTPParams.GetFieldName(Index: Integer): WideString;
begin
  Result := FHTTPParamList.Items[Index].FieldName;
end;

function THTTPParams.GetFieldValue(const AFieldName: WideString): WideString;
begin
  Result := FHTTPParamList.Items[GetFieldIndex(AFieldName)].FieldValue;
end;

procedure THTTPParams.SetFieldValue(const AFieldName, AFieldValue: WideString);
begin
  FHTTPParamList.Items[GetFieldIndex(AFieldName)].FieldValue := AFieldValue;
end;

function THTTPParams.GetFieldValueFromIndex(Index: Integer): WideString;
begin
  Result := FHTTPParamList.Items[Index].FieldValue;
end;

procedure THTTPParams.SetFieldValueFromIndex(Index: Integer; const AFieldValue: WideString);
begin
  FHTTPParamList.Items[Index].FieldValue := AFieldValue;
end;

function THTTPParams.GetFileName(const AFieldName: WideString): WideString;
begin
  Result := FHTTPParamList.Items[GetFieldIndex(AFieldName)].FieldFileName;
end;

procedure THTTPParams.SetFileName(const AFieldName, AFileName: WideString);
begin
  FHTTPParamList.Items[GetFieldIndex(AFieldName)].FieldFileName := AFileName;
end;

function THTTPParams.GetFileNameFromIndex(Index: Integer): WideString;
begin
  Result := FHTTPParamList.Items[Index].FieldFileName;
end;

procedure THTTPParams.SetFileNameFromIndex(Index: Integer; const AFileName: WideString);
begin
  FHTTPParamList.Items[Index].FieldFileName := AFileName;
end;

function THTTPParams.GetParamType: TParamType;
begin
  if not(FRawData = '') then
    Result := ptData
  else if FHasFile then
    Result := ptMultipartFormData
  else
    Result := FParamType;
end;

procedure THTTPParams.SetParamType(AParamType: TParamType);
begin
  FParamType := AParamType;
end;

function THTTPParams.GetRawData: WideString;
begin
  Result := FRawData;
end;

procedure THTTPParams.SetRawData(const ARawData: WideString);
begin
  FRawData := ARawData;
end;

function THTTPParams.GetCount: Integer;
begin
  Result := FHTTPParamList.Count;
end;

function THTTPParams.GetIsFile(Index: Integer): WordBool;
begin
  Result := (FHTTPParamList.Items[Index].FieldType = ftFile);
end;

constructor THTTPParams.Create;
begin
  FHTTPParamList := TList<THTTPParam>.Create;
  FRawData := '';
  FHasFile := False;
end;

constructor THTTPParams.Create(AParamType: TParamType);
begin
  Create;
  FParamType := AParamType;
end;

constructor THTTPParams.Create(ARawData: WideString);
begin
  Create;
  FRawData := ARawData;
end;

constructor THTTPParams.Clone(const HTTPParams: IHTTPParams);
var
  I: Integer;
begin
  Create;

  for I := 0 to HTTPParams.Count - 1 do
    if HTTPParams.IsFile[I] then
      AddFile(HTTPParams.FieldNames[I], HTTPParams.FieldFileNameFromIndex[I])
    else
      AddFormField(HTTPParams.FieldNames[I], HTTPParams.FieldValueFromIndex[I], HTTPParams.FieldFileNameFromIndex[I]);

  RawData := HTTPParams.RawData;
  ParamType := HTTPParams.ParamType;
end;

destructor THTTPParams.Destroy;
var
  I: Integer;
begin
  for I := 0 to FHTTPParamList.Count - 1 do
    FHTTPParamList.Items[I].Free;
  FHTTPParamList.Free;
  inherited Destroy;
end;

procedure THTTPParams.Clear;
begin
  FHTTPParamList.Clear;
  FHasFile := False;
end;

procedure THTTPParams.Delete(Index: Integer);
var
  I: Integer;
begin
  FHTTPParamList.Delete(Index);

  FHasFile := False;
  for I := 0 to FHTTPParamList.Count - 1 do
    if (FHTTPParamList.Items[I].FieldType in [ftFile, ftFileStream]) then
    begin
      FHasFile := True;
      break;
    end;
end;

procedure THTTPParams.AddFile(const AFieldName, AFileName: WideString);
begin
  FHTTPParamList.Add(THTTPParam.Create(AFieldName, '', ftFile, AFileName));
  FHasFile := True;
end;

procedure THTTPParams.AddFormField(const AFieldName, AFieldValue: WideString);
begin
  FHTTPParamList.Add(THTTPParam.Create(AFieldName, AFieldValue));
end;

procedure THTTPParams.AddFormField(const AFieldName: WideString; const AFieldValue: WideString; const AFileName: WideString);
begin
  FHTTPParamList.Add(THTTPParam.Create(AFieldName, AFieldValue, ftFileStream, AFileName));
  FHasFile := True;
end;

{ THTTPHeader }

constructor THTTPHeader.Create;
begin
  inherited Create;
  FCookies := TCOMList.Create;
  FCacheControl := '';
  FCharSet := 'utf-8';
  FConnection := HTTPConnection;
  FContentDisposition := '';
  FContentEncoding := '';
  FContentLanguage := '';
  FContentType := HTTPRequestContentTypeList;
  FCustomHeaders := TCOMList.Create;
end;

destructor THTTPHeader.Destroy;
begin
  FCustomHeaders := nil;
  FCookies := nil;
  inherited Destroy;
end;

function THTTPHeader.GetCookies: ICOMList;
begin
  Result := FCookies;
end;

procedure THTTPHeader.SetCookies(const ACookies: ICOMList);
begin
  FCookies := ACookies;
end;

function THTTPHeader.GetCacheControl: WideString;
begin
  Result := FCacheControl;
end;

procedure THTTPHeader.SetCacheControl(const ACacheControl: WideString);
begin
  FCacheControl := ACacheControl;
end;

function THTTPHeader.GetCharSet: WideString;
begin
  Result := FCharSet;
end;

procedure THTTPHeader.SetCharSet(const ACharSet: WideString);
begin
  FCharSet := ACharSet;
end;

function THTTPHeader.GetConnection: WideString;
begin
  Result := FConnection;
end;

procedure THTTPHeader.SetConnection(const AConnection: WideString);
begin
  FConnection := AConnection;
end;

function THTTPHeader.GetContentDisposition: WideString;
begin
  Result := FContentDisposition;
end;

procedure THTTPHeader.SetContentDisposition(const AContentDisposition: WideString);
begin
  FContentDisposition := AContentDisposition;
end;

function THTTPHeader.GetContentEncoding: WideString;
begin
  Result := FContentEncoding;
end;

procedure THTTPHeader.SetContentEncoding(const AContentEncoding: WideString);
begin
  FContentEncoding := AContentEncoding;
end;

function THTTPHeader.GetContentLanguage: WideString;
begin
  Result := FContentLanguage;
end;

procedure THTTPHeader.SetContentLanguage(const AContentLanguage: WideString);
begin
  FContentLanguage := AContentLanguage;
end;

function THTTPHeader.GetContentType: WideString;
begin
  Result := FContentType;
end;

procedure THTTPHeader.SetContentType(const AContentType: WideString);
begin
  FContentType := AContentType;
end;

function THTTPHeader.GetCustomHeaders: ICOMList;
begin
  Result := FCustomHeaders;
end;

procedure THTTPHeader.SetCustomHeaders(const ACustomHeaders: ICOMList);
begin
  FCustomHeaders := ACustomHeaders;
end;

{ THTTPRequest }

constructor THTTPRequest.Create(const AURL: string);
begin
  inherited Create;
  FURL := AURL;
  FMethodDefined := False;
  FAccept := HTTPRequestAccept;
  FAcceptCharSet := HTTPRequestAcceptCharSet;
  FAcceptEncoding := HTTPRequestAcceptEncoding;
  FAcceptLanguage := HTTPRequestAcceptLanguage;
  FHost := '';
  FReferer := '';
  FUserAgent := HTTPRequestUserAgent;
end;

constructor THTTPRequest.Clone(const AHTTPRequest: IHTTPRequest);
begin
  Create(AHTTPRequest.URL);

  Method := AHTTPRequest.Method;

  Accept := AHTTPRequest.Accept;
  AcceptCharSet := AHTTPRequest.AcceptCharSet;
  AcceptEncoding := AHTTPRequest.AcceptEncoding;
  AcceptLanguage := AHTTPRequest.AcceptLanguage;
  Host := AHTTPRequest.Host;
  Referer := AHTTPRequest.Referer; // URL here before?
  UserAgent := AHTTPRequest.UserAgent;

  Cookies.Text := AHTTPRequest.Cookies.Text;

  CacheControl := AHTTPRequest.CacheControl;
  CharSet := AHTTPRequest.CharSet;
  Connection := AHTTPRequest.Connection;
  ContentDisposition := AHTTPRequest.ContentDisposition;
  ContentEncoding := AHTTPRequest.ContentEncoding;
  ContentLanguage := AHTTPRequest.ContentLanguage;
  ContentType := AHTTPRequest.ContentType;

  CustomHeaders.Text := AHTTPRequest.CustomHeaders.Text;
end;

constructor THTTPRequest.FollowUpClone(const AHTTPProcess: IHTTPProcess; const ANewHTTPProcess: IHTTPRequest);
begin
  Clone(ANewHTTPProcess);

  Referer := AHTTPProcess.HTTPData.Website; // Referer here?

  Cookies.Text := AHTTPProcess.HTTPData.HTTPRequest.Cookies.Text + AHTTPProcess.HTTPResult.HTTPResponse.Cookies.Text + ANewHTTPProcess.Cookies.Text;

  CustomHeaders.Text := AHTTPProcess.HTTPData.HTTPRequest.CustomHeaders.Text + AHTTPProcess.HTTPResult.HTTPResponse.CustomHeaders.Text + ANewHTTPProcess.CustomHeaders.Text;
end;

constructor THTTPRequest.FollowUpClone(const AHTTPProcess: IHTTPProcess; AHTTPMethod: THTTPMethod; AURL: string);
begin
  Clone(AHTTPProcess.HTTPData.HTTPRequest);

  URL := AURL;
  Method := AHTTPMethod;

  // maybe needs between additional line break
  Cookies.Text := AHTTPProcess.HTTPData.HTTPRequest.Cookies.Text + AHTTPProcess.HTTPResult.HTTPResponse.Cookies.Text;

  CacheControl := AHTTPProcess.HTTPResult.HTTPResponse.CacheControl;
  CharSet := AHTTPProcess.HTTPResult.HTTPResponse.CharSet;
  Connection := AHTTPProcess.HTTPResult.HTTPResponse.Connection;
  ContentDisposition := AHTTPProcess.HTTPResult.HTTPResponse.ContentDisposition;
  ContentEncoding := AHTTPProcess.HTTPResult.HTTPResponse.ContentEncoding;
  ContentLanguage := AHTTPProcess.HTTPResult.HTTPResponse.ContentLanguage;
  ContentType := AHTTPProcess.HTTPResult.HTTPResponse.ContentType;

  CustomHeaders.Text := AHTTPProcess.HTTPData.HTTPRequest.CustomHeaders.Text + AHTTPProcess.HTTPResult.HTTPResponse.CustomHeaders.Text;
end;

destructor THTTPRequest.Destroy;
begin
  inherited Destroy;
end;

function THTTPRequest.GetURL: WideString;
begin
  Result := FURL;
end;

procedure THTTPRequest.SetURL(const AURL: WideString);
begin
  FURL := AURL;
end;

function THTTPRequest.GetMethod: THTTPMethod;
begin
  if not FMethodDefined then
    raise Exception.Create('Method is not defined. This will be done calling THTTPManager.GET/POST etc.');
  Result := FMethod;
end;

procedure THTTPRequest.SetMethod(AMethod: THTTPMethod);
begin
  FMethodDefined := True;
  FMethod := AMethod;
end;

function THTTPRequest.GetAccept: WideString;
begin
  Result := FAccept;
end;

procedure THTTPRequest.SetAccept(const AAccept: WideString);
begin
  FAccept := AAccept;
end;

function THTTPRequest.GetAcceptCharSet: WideString;
begin
  Result := FAcceptCharSet;
end;

procedure THTTPRequest.SetAcceptCharSet(const AAcceptCharSet: WideString);
begin
  FAcceptCharSet := AAcceptCharSet;
end;

function THTTPRequest.GetAcceptEncoding: WideString;
begin
  Result := FAcceptEncoding;
end;

procedure THTTPRequest.SetAcceptEncoding(const AAcceptEncoding: WideString);
begin
  FAcceptEncoding := AAcceptEncoding;
end;

function THTTPRequest.GetAcceptLanguage: WideString;
begin
  Result := FAcceptLanguage;
end;

procedure THTTPRequest.SetAcceptLanguage(const AAcceptLanguage: WideString);
begin
  FAcceptLanguage := AAcceptLanguage;
end;

function THTTPRequest.GetHost: WideString;
begin
  Result := FHost;
end;

procedure THTTPRequest.SetHost(const AHost: WideString);
begin
  FHost := AHost;
end;

function THTTPRequest.GetReferer: WideString;
begin
  Result := FReferer;
end;

procedure THTTPRequest.SetReferer(const AReferer: WideString);
begin
  FReferer := AReferer;
end;

function THTTPRequest.GetUserAgent: WideString;
begin
  Result := FUserAgent;
end;

procedure THTTPRequest.SetUserAgent(const AUserAgent: WideString);
begin
  FUserAgent := AUserAgent;
end;

{ THTTPResponse }

function THTTPResponse.GetLocation: WideString;
begin
  Result := FLocation;
end;

procedure THTTPResponse.SetLocation(const ALocation: WideString);
begin
  FLocation := ALocation;
end;

function THTTPResponse.GetRefresh: WideString;
begin
  Result := FRefresh;
end;

procedure THTTPResponse.SetRefresh(const ARefresh: WideString);
begin
  FRefresh := ARefresh;
end;

function THTTPResponse.GetText: WideString;
begin
  Result := FText;
end;

procedure THTTPResponse.SetText(const AText: WideString);
begin
  FText := AText;
end;

function THTTPResponse.GetCode: Integer;
begin
  Result := FCode;
end;

procedure THTTPResponse.SetCode(ACode: Integer);
begin
  FCode := ACode;
end;

function THTTPResponse.GetServer: WideString;
begin
  Result := FServer;
end;

procedure THTTPResponse.SetServer(const AServer: WideString);
begin
  FServer := AServer;
end;

function THTTPResponse.GetContent: WideString;
begin
  Result := FContent;
end;

procedure THTTPResponse.SetContent(const AContent: WideString);
begin
  FContent := AContent;
end;

function THTTPResponse.GetContentStream: IStream;
begin
  Result := FContentStream;
end;

procedure THTTPResponse.SetContentStream(const AContentStream: IStream);
begin
  FContentStream := AContentStream;
end;

constructor THTTPResponse.Create(const AContentStream: TMemoryStream);
var
  MemoryStream: TMemoryStream;
begin
  inherited Create;
  MemoryStream := TMemoryStream.Create;
  if Assigned(AContentStream) then
    MemoryStream.LoadFromStream(AContentStream);
  MemoryStream.Position := 0;
  FContentStream := TStreamAdapter.Create(MemoryStream, soOwned);
end;

destructor THTTPResponse.Destroy;
begin
  FContentStream := nil;
  inherited Destroy;
end;

{ THTTPOptions }

constructor THTTPOptions.Create(const AProxy: IProxy = nil);
begin
  inherited Create;
  FUseCompressor := True;

  if not Assigned(AProxy) then
    FProxy := TProxy.Create
  else
    FProxy := AProxy;

  FConnectTimeout := 0;
  FReadTimeout := 0;

  FHandleRedirects := True;
  FHandleSketchyRedirects := True;
  FRedirectMaximum := 15;
end;

constructor THTTPOptions.Clone(const AHTTPOptions: IHTTPOptions);
var
  Proxy: IProxy;
begin
  Proxy := TProxy.Clone(AHTTPOptions.Proxy);

  Create(Proxy);

  UseCompressor := AHTTPOptions.UseCompressor;
  ConnectTimeout := AHTTPOptions.ConnectTimeout;
  ReadTimeout := AHTTPOptions.ReadTimeout;
  HandleRedirects := AHTTPOptions.HandleRedirects;
  HandleSketchyRedirects := AHTTPOptions.HandleSketchyRedirects;
  RedirectMaximum := AHTTPOptions.RedirectMaximum;
end;

destructor THTTPOptions.Destroy;
begin
  FProxy := nil;
  inherited Destroy;
end;

function THTTPOptions.GetUseCompressor: WordBool;
begin
  Result := FUseCompressor;
end;

procedure THTTPOptions.SetUseCompressor(AUseCompressor: WordBool);
begin
  FUseCompressor := AUseCompressor;
end;

function THTTPOptions.GetProxy: IProxy;
begin
  Result := FProxy;
end;

procedure THTTPOptions.SetProxy(AProxy: IProxy);
begin
  FProxy := AProxy;
end;

function THTTPOptions.GetConnectTimeout: Integer;
begin
  Result := FConnectTimeout;
end;

procedure THTTPOptions.SetConnectTimeout(AConnectTimeout: Integer);
begin
  FConnectTimeout := AConnectTimeout;
end;

function THTTPOptions.GetReadTimeout: Integer;
begin
  Result := FReadTimeout;
end;

procedure THTTPOptions.SetReadTimeout(AReadTimeout: Integer);
begin
  FReadTimeout := AReadTimeout;
end;

function THTTPOptions.GetHandleRedirects: WordBool;
begin
  Result := FHandleRedirects;
end;

procedure THTTPOptions.SetHandleRedirects(AHandleRedirects: WordBool);
begin
  FHandleRedirects := AHandleRedirects;
end;

function THTTPOptions.GetHandleSketchyRedirects: WordBool;
begin
  Result := FHandleSketchyRedirects;
end;

procedure THTTPOptions.SetHandleSketchyRedirects(AHandleSketchyRedirects: WordBool);
begin
  FHandleSketchyRedirects := AHandleSketchyRedirects;
end;

function THTTPOptions.GetRedirectMaximum: Integer;
begin
  Result := FRedirectMaximum;
end;

procedure THTTPOptions.SetRedirectMaximum(ARedirectMaximum: Integer);
begin
  FRedirectMaximum := ARedirectMaximum;
end;

{ THTTPResponseInfo }

function THTTPResponseInfo.GetLastRedirect: WideString;
begin
  Result := FLastRedirect;
end;

procedure THTTPResponseInfo.SetLastRedirect(const ALastRedirect: WideString);
begin
  FLastRedirect := ALastRedirect;
end;

function THTTPResponseInfo.GetRedirectCount: Integer;
begin
  Result := FRedirectCount;
end;

procedure THTTPResponseInfo.SetRedirectCount(ARedirectCount: Integer);
begin
  FRedirectCount := ARedirectCount;
end;

function THTTPResponseInfo.GetErrorClassName: WideString;
begin
  Result := FErrorClassName;
end;

procedure THTTPResponseInfo.SetErrorClassName(const AErrorClassName: WideString);
begin
  FErrorClassName := AErrorClassName;
end;

function THTTPResponseInfo.GetErrorMessage: WideString;
begin
  Result := FErrorMessage;
end;

procedure THTTPResponseInfo.SetErrorMessage(const AErrorMessage: WideString);
begin
  FErrorMessage := AErrorMessage;
end;

constructor THTTPResponseInfo.Create;
begin
  inherited Create;
end;

destructor THTTPResponseInfo.Destroy;
begin
  inherited Destroy;
end;

end.
