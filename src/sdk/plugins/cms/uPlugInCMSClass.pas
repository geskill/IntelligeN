{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInCMSClass;

interface

uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInClass, uPlugInHTTPClasses;

type
  TCMSCustomField = class(TPersistent)
  protected
    FName, FValue: string;
  published
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  TCMSCustomFields = class(TPersistent)
  private
    FOwnsObjects: Boolean;
    FCustomFieldsList: TList<TCMSCustomField>;
  protected
    function GetField(Index: Integer): TCMSCustomField;
    function GetFieldCount: Integer;
  public
    constructor Create(const AOwnsObjects: Boolean = True);
    function Add(const ACMSCustomField: TCMSCustomField): Integer;
    property Field[Index: Integer]: TCMSCustomField read GetField;
    property FieldCount: Integer read GetFieldCount;
    procedure ReleaseObjects;
    destructor Destroy; override;
  end;

  TCMSPlugInSettings = class(TPersistent)
  strict private
    FCharset: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Charset: string read FCharset write FCharset;
  published
  end;

  TCMSPlugInSettingsMeta = class of TCMSPlugInSettings;

  TCMSPlugIn = class(TPlugIn, ICMSPlugIn)
  private
    FIntelligentPostingHelper: TIntelligentPostingHelper;
  protected
    FCheckedIDsList: TList<TIDInfo>;
    procedure AddID(const AID, APath: string);
    function SettingsClass: TCMSPlugInSettingsMeta; virtual; abstract;
    function GetSettings: TCMSPlugInSettings; virtual; abstract;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); virtual; abstract;
    function LoadSettings(const ACMSData: ICMSData; const APublishData: ICMSPublishData = nil): Boolean; virtual;

    function CanLogin(const ACMSData: ICMSData): Boolean; virtual;
    function NeedPreLogin(const ACMSData: ICMSData; out ARequestURL: string): Boolean; virtual;
    function NeedLogin: Boolean; virtual;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; virtual; abstract;
    function NeedPostLogin(const ACMSData: ICMSData; out ARequestURL: string): Boolean; virtual;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; virtual; abstract;
    procedure DoHandleSessionID(AHTTPProcess: IHTTPProcess); virtual;

    function NeedBeforePostAction: Boolean; virtual;
    function DoBeforePostAction(var ARequestID: Double): Boolean; virtual;

    function NeedPrePost(const ACMSData: ICMSData; out ARequestURL: string): Boolean; virtual;
    function DoAnalyzePrePost(const AResponseStr: string): Boolean; virtual;

    function DoBuildPostRequest(const APublishData: ICMSPublishData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; virtual; abstract;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; virtual; abstract;

    function GetRetrieveIDsRequestURL(const ACMSData: ICMSData; const ACategoryType: WideString): string; virtual;
    function DoAnalyzeRetrieveIDsRequest(const ACategoryType: WideString; const AResponseStr: string; out ACMSIDsInfo: ICMSIDsInfo): WordBool; virtual;

    property Settings: TCMSPlugInSettings read GetSettings write SetSettings;

    function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;

    property IntelligentPostingHelper: TIntelligentPostingHelper read GetIntelligentPostingHelper;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetType: TPlugInType; override; safecall;

    function GetCMSType: TCMSType; virtual; safecall; abstract;

    function GetDefaultCharset: WideString; virtual; safecall; abstract;
    function GetBelongsTo(const AWebsiteSourceCode: WideString): WordBool; virtual; safecall; abstract;

    function GetLogin(const ACMSData: ICMSData; out ARequestID: Double): WordBool; virtual; safecall;

    function GetRetrieveIDs(const ACMSData: ICMSData; const ACategoryType: WideString; out ACMSIDsInfo: ICMSIDsInfo): WordBool; virtual; safecall;

    function AddArticle(const ACMSData: ICMSData; const ACMSPublishData: ICMSPublishData; out ACMSArticleInfo: TCMSArticleInfo): WordBool; virtual; safecall;
    function EditArticle(const ACMSData: ICMSData; const ACMSPublishData: ICMSPublishData; var ACMSArticleInfo: TCMSArticleInfo): WordBool; virtual; safecall;
    function DeleteArticle(const ACMSData: ICMSData; const ACMSArticleInfo: TCMSArticleInfo): WordBool; virtual; safecall;
    function GetArticle(const ACMSData: ICMSData; const ACMSArticleInfo: TCMSArticleInfo; out ACMSPublishData: ICMSPublishData): WordBool; virtual; safecall;

    function GetArticleLink(const AWebsite: WideString; const ACMSArticleInfo: TCMSArticleInfo): WideString; virtual; safecall; abstract;

    procedure Initialize(const AHTTPManager: IHTTPManager; const AProxy: IProxy; const AConnectTimeout, AReadTimeout: Integer; const AIntelligentPostingHelper: TIntelligentPostingHelper); safecall;

    function ShowWebsiteSettingsEditor(const ACMSData: ICMSData; const AWebsiteEditor: IWebsiteEditor): WordBool; safecall;
  end;

resourcestring
  StrAbortedThrougthCAP = 'Aborted througth CAPTCHA-Helper';

implementation

uses
  uPlugInCMSSettingsHelper;

{ TCMSCustomFields }

function TCMSCustomFields.GetField(Index: Integer): TCMSCustomField;
begin
  Result := FCustomFieldsList.Items[Index];
end;

function TCMSCustomFields.GetFieldCount: Integer;
begin
  Result := FCustomFieldsList.Count;
end;

function TCMSCustomFields.Add(const ACMSCustomField: TCMSCustomField): Integer;
begin
  Result := FCustomFieldsList.Add(ACMSCustomField);
end;

procedure TCMSCustomFields.ReleaseObjects;
var
  LIndex: Integer;
begin
  for LIndex := FCustomFieldsList.Count - 1 downto 0 do
  begin
    FCustomFieldsList.Items[LIndex].Free;
    FCustomFieldsList.Delete(LIndex);
  end;
end;

constructor TCMSCustomFields.Create(const AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  FCustomFieldsList := TList<TCMSCustomField>.Create;
end;

destructor TCMSCustomFields.Destroy;
begin
  if FOwnsObjects then
    ReleaseObjects;
  FCustomFieldsList.Free;
  inherited Destroy;
end;

{ TCMSPlugInSettings }

constructor TCMSPlugInSettings.Create;
begin
  inherited Create;
end;

destructor TCMSPlugInSettings.Destroy;
begin
  //
  inherited Destroy;
end;

{ TCMSPlugIn }

procedure TCMSPlugIn.AddID(const AID, APath: string);
var
  IDInfo: TIDInfo;
begin
  with IDInfo do
  begin
    ID := AID;
    Path := APath;
  end;

  FCheckedIDsList.Add(IDInfo);
end;

function TCMSPlugIn.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(ACMSData.SettingsFileName, Settings, APublishData.Data);
  with Settings do
    if Assigned(APublishData) and SameStr('', Charset) then
      Charset := GetDefaultCharset;
end;

function TCMSPlugIn.CanLogin(const ACMSData: ICMSData): Boolean;
begin
  Result := not SameStr('', ACMSData.AccountName);
end;

function TCMSPlugIn.NeedPreLogin(const ACMSData: ICMSData; out ARequestURL: string): Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.NeedLogin: Boolean;
begin
  Result := True;
end;

function TCMSPlugIn.NeedPostLogin(const ACMSData: ICMSData; out ARequestURL: string): Boolean;
begin
  Result := False;
end;

procedure TCMSPlugIn.DoHandleSessionID(AHTTPProcess: IHTTPProcess);
begin
  /// by default do nothing
end;

function TCMSPlugIn.NeedBeforePostAction: Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.DoBeforePostAction(var ARequestID: Double): Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.NeedPrePost(const ACMSData: ICMSData; out ARequestURL: string): Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.DoAnalyzePrePost(const AResponseStr: string): Boolean;
begin
  Result := True;
end;

function TCMSPlugIn.GetRetrieveIDsRequestURL(const ACMSData: ICMSData; const ACategoryType: WideString): string;
begin
  Result := ACMSData.Website + 'search.php';
end;

function TCMSPlugIn.DoAnalyzeRetrieveIDsRequest(const ACategoryType: WideString; const AResponseStr: string; out ACMSIDsInfo: ICMSIDsInfo): WordBool;
begin
  // TODO
end;

function TCMSPlugIn.GetIntelligentPostingHelper;
begin
  Result := FIntelligentPostingHelper;
end;

constructor TCMSPlugIn.Create;
begin
  inherited Create;
  FCheckedIDsList := TList<TIDInfo>.Create;
  Settings := SettingsClass.Create;
end;

destructor TCMSPlugIn.Destroy;
begin
  Settings.Free;
  FCheckedIDsList.Free;
  inherited Destroy;
end;

function TCMSPlugIn.GetType;
begin
  Result := ptCMS;
end;

function TCMSPlugIn.GetLogin;
var
  PreLoginURL: string;
  PostLoginURL: string;
  RequestID: Double;
  ResponseStr: string;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;

  HTTPProcess: IHTTPProcess;

  HTTPError, CAPTCHALogin: Boolean;
begin
  Result := False;
  RequestID := -1;
  ResponseStr := '';

  if not CanLogin(ACMSData) then
  begin
    // TODO
    ErrorMsg := 'Cant login. No username specified ...';
    Exit;
  end;

  /// pre login is commonly not used, but reserved for some pre login actions
  /// some CMS need a GET request before log-in (require cookie other security values)
  if NeedPreLogin(ACMSData, PreLoginURL) then
  begin
    HTTPRequest := THTTPRequest.Create(PreLoginURL);
    with HTTPRequest do
    begin
      Referer := ACMSData.Website;
      Charset := Settings.Charset;
    end;

    RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

    HTTPManager.WaitFor(RequestID);

    HTTPProcess := HTTPManager.GetResult(RequestID);

    DoHandleSessionID(HTTPProcess);

    ResponseStr := HTTPProcess.HTTPResult.SourceCode;
  end;

  if DoBuildLoginRequest(HTTPRequest, HTTPParams, HTTPOptions, ResponseStr) then
  begin
    if (RequestID = -1) then // no previous request actions
      ARequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions)
    else
      ARequestID := HTTPManager.Post(HTTPRequest.URL, RequestID, HTTPParams, HTTPOptions);

    HTTPManager.WaitFor(ARequestID);

    HTTPProcess := HTTPManager.GetResult(ARequestID);

    HTTPError := HTTPProcess.HTTPResult.HasError;

    if HTTPError then
      ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

    /// post login is commonly not used, but reserved for some post login actions
    /// some CMS need a GET request after log-in (refresh session, validate login)
    if NeedPostLogin(ACMSData, PostLoginURL) then
    begin
      ARequestID := HTTPManager.Get(PostLoginURL, ARequestID, TPlugInHTTPOptions.Create(Self));

      HTTPManager.WaitFor(ARequestID);

      HTTPProcess := HTTPManager.GetResult(ARequestID);

      HTTPError := HTTPProcess.HTTPResult.HasError;

      if HTTPError then
        ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    end;

    Result := not HTTPError and DoAnalyzeLogin(HTTPProcess.HTTPResult.SourceCode, CAPTCHALogin);

    if CAPTCHALogin and DoBuildLoginRequest(HTTPRequest, HTTPParams, HTTPOptions, HTTPProcess.HTTPResult.SourceCode, True) then
    begin
      ARequestID := HTTPManager.Post(HTTPRequest.URL, ARequestID, HTTPParams, HTTPOptions);

      HTTPManager.WaitFor(ARequestID);

      HTTPProcess := HTTPManager.GetResult(ARequestID);

      HTTPError := HTTPProcess.HTTPResult.HasError;

      if HTTPError then
        ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

      Result := not HTTPError and DoAnalyzeLogin(HTTPProcess.HTTPResult.SourceCode, CAPTCHALogin);
    end;

    DoHandleSessionID(HTTPProcess);
  end;
end;

function TCMSPlugIn.GetRetrieveIDs;
var
  LRequestID: Double;
  LHTTPResult: IHTTPResult;
begin
  Result := False;
  LRequestID := -1;

  // TODO
  LoadSettings(ACMSData);

  // do login if possible
  if CanLogin(ACMSData) then
    if not GetLogin(ACMSData, LRequestID) then
      Exit;

  if (LRequestID = -1) then
    LRequestID := HTTPManager.Get(THTTPRequest.Create(GetRetrieveIDsRequestURL(ACMSData, ACategoryType)), TPlugInHTTPOptions.Create(Self))
  else
    LRequestID := HTTPManager.Get(GetRetrieveIDsRequestURL(ACMSData, ACategoryType), LRequestID, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPResult := HTTPManager.GetResult(LRequestID).HTTPResult;

  if LHTTPResult.HasError then
  begin
    ErrorMsg := LHTTPResult.HTTPResponseInfo.ErrorMessage;
    Exit;
  end;

  Result := DoAnalyzeRetrieveIDsRequest(ACategoryType, LHTTPResult.SourceCode, ACMSIDsInfo);
end;

function TCMSPlugIn.AddArticle;
var
  PrePostURL: string;
  RequestID: Double;
  ResponseStr: string;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;

  HTTPProcess: IHTTPProcess;

  HTTPError: Boolean;
begin
  Result := False;

  RequestID := -1;
  ResponseStr := '';

  if LoadSettings(ACMSData, ACMSPublishData) then
    // do login if required
    if (not NeedLogin) xor (NeedLogin and GetLogin(ACMSData, RequestID)) then
    begin
      if NeedBeforePostAction then
      begin
        if DoBeforePostAction(RequestID) then
        begin
          if not(RequestID = -1) then // no previous request actions
            ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
        end
        else
        begin
          Exit;
        end;
      end;

      if NeedPrePost(ACMSData, PrePostURL) then
      begin
        if (RequestID = -1) then // no previous request actions
          RequestID := HTTPManager.Get(THTTPRequest.Create(PrePostURL), TPlugInHTTPOptions.Create(Self))
        else
          RequestID := HTTPManager.Get(PrePostURL, RequestID, TPlugInHTTPOptions.Create(Self));

        HTTPManager.WaitFor(RequestID);

        ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

        if not DoAnalyzePrePost(ResponseStr) then
          Exit;
      end;

      if DoBuildPostRequest(ACMSPublishData, HTTPRequest, HTTPParams, HTTPOptions, ResponseStr, RequestID) then
      begin
        HTTPRequest.Method := mPOST;

        if (RequestID = -1) then // no previous request actions
          RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions)
        else
          RequestID := HTTPManager.Post(THTTPRequest.FollowUpClone(HTTPManager.GetResult(RequestID), HTTPRequest), HTTPParams, HTTPOptions);

        HTTPManager.WaitFor(RequestID);

        HTTPProcess := HTTPManager.GetResult(RequestID);

        HTTPError := HTTPProcess.HTTPResult.HasError;

        if HTTPError then
          ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

        Result := DoAnalyzePost(HTTPProcess.HTTPResult.SourceCode, HTTPProcess) and not HTTPError;
      end;
    end;
end;

function TCMSPlugIn.EditArticle;
begin
  // TODO: Implement this
  Result := False;
end;

function TCMSPlugIn.DeleteArticle;
begin
  // TODO: Implement this
  Result := False;
end;

function TCMSPlugIn.GetArticle;
begin
  // TODO: Implement this
  Result := False;
end;

procedure TCMSPlugIn.Initialize;
begin
  inherited Initialize(AHTTPManager, AProxy, AConnectTimeout, AReadTimeout);
  FIntelligentPostingHelper := AIntelligentPostingHelper;
end;

function TCMSPlugIn.ShowWebsiteSettingsEditor;

/// taken from Controls unit; saves ~3kb
  function IsPositiveResult(AModalResult: Integer): Boolean;
  begin
    Result := AModalResult in [1, 6, 8, 10];
  end;

begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(ACMSData.SettingsFileName, Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
