{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
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
    FAccountname, FAccountpassword, FSettingsFileName, FSubject, FTags, FMessage, FWebsite: WideString;
    FData: ITabSheetData;
    FArticleID: Integer;
    FIntelligentPostingHelper: TIntelligentPostingHelper;
  protected
    FCheckedIDsList: TList<TIDInfo>;
    procedure AddID(const AID, APath: string);
    function SettingsClass: TCMSPlugInSettingsMeta; virtual; abstract;
    function GetSettings: TCMSPlugInSettings; virtual; abstract;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); virtual; abstract;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; virtual;

    function NeedPreLogin(out ARequestURL: string): Boolean; virtual;
    function NeedLogin: Boolean; virtual;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; virtual; abstract;
    function NeedPostLogin(out ARequestURL: string): Boolean; virtual;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; virtual; abstract;
    procedure DoHandleSessionID(AHTTPProcess: IHTTPProcess); virtual;

    function NeedBeforePostAction: Boolean; virtual;
    function DoBeforePostAction(var ARequestID: Double): Boolean; virtual;

    function NeedPrePost(out ARequestURL: string): Boolean; virtual;
    function DoAnalyzePrePost(const AResponseStr: string): Boolean; virtual;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; virtual; abstract;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; virtual; abstract;

    function GetIDsRequestURL: string; virtual;
    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; virtual;

    property Settings: TCMSPlugInSettings read GetSettings write SetSettings;

    function GetAccountName: WideString; safecall;
    procedure SetAccountName(const AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(const AAccountPassword: WideString); safecall;
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(const ASettingsFileName: WideString); safecall;
    function GetSubject: WideString; safecall;
    procedure SetSubject(const ASubject: WideString); safecall;
    function GetTags: WideString; safecall;
    procedure SetTags(const ATags: WideString); safecall;
    function GetMessage: WideString; safecall;
    procedure SetMessage(const AMessage: WideString); safecall;
    function GetWebsite: WideString; safecall;
    procedure SetWebsite(const AWebsite: WideString); safecall;
    function GetData: ITabSheetData; safecall;
    procedure SetData(const AData: ITabSheetData); safecall;

    function GetArticleID: Integer; safecall;
    procedure SetArticleID(AArticleID: Integer); safecall;

    function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;
    procedure SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper); safecall;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetType: TPlugInType; override; safecall;

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

    function CMSType: TCMSType; virtual; safecall; abstract;
    function DefaultCharset: WideString; virtual; safecall; abstract;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; virtual; safecall; abstract;
    function GetIDs: Integer; virtual; safecall;
    function ReadID(AIndex: Integer): TIDInfo; safecall;
    function Login(out ARequestID: Double): Boolean; virtual; safecall;
    function AddArticle(): WordBool; virtual; safecall;
    function EditArticle(): WordBool; virtual; safecall;
    function GetArticle(out AArticle: WideString): WordBool; virtual; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; virtual; safecall; abstract;
    function ShowWebsiteSettingsEditor(const AWebsiteEditor: IWebsiteEditor): WordBool; safecall;
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

function TCMSPlugIn.LoadSettings(const AData: ITabSheetData = nil): Boolean;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, Settings, AData);
  with Settings do
    if Assigned(AData) and SameStr('', Charset) then
      Charset := DefaultCharset;
end;

function TCMSPlugIn.NeedPreLogin(out ARequestURL: string): Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.NeedLogin: Boolean;
begin
  Result := True;
end;

function TCMSPlugIn.NeedPostLogin(out ARequestURL: string): Boolean;
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

function TCMSPlugIn.NeedPrePost(out ARequestURL: string): Boolean;
begin
  Result := False;
end;

function TCMSPlugIn.DoAnalyzePrePost(const AResponseStr: string): Boolean;
begin
  Result := True;
end;

function TCMSPlugIn.GetIDsRequestURL: string;
begin
  Result := Website + 'search.php';
end;

function TCMSPlugIn.DoAnalyzeIDsRequest(const AResponseStr: string): Integer;
begin
  Result := FCheckedIDsList.Count;
end;

function TCMSPlugIn.GetAccountName: WideString;
begin
  Result := FAccountname;
end;

procedure TCMSPlugIn.SetAccountName(const AAccountName: WideString);
begin
  FAccountname := AAccountName;
end;

function TCMSPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountpassword;
end;

procedure TCMSPlugIn.SetAccountPassword(const AAccountPassword: WideString);
begin
  FAccountpassword := AAccountPassword;
end;

function TCMSPlugIn.GetSettingsFileName: WideString;
begin
  Result := FSettingsFileName;
end;

procedure TCMSPlugIn.SetSettingsFileName(const ASettingsFileName: WideString);
begin
  FSettingsFileName := ASettingsFileName;
end;

function TCMSPlugIn.GetSubject: WideString;
begin
  Result := FSubject;
end;

procedure TCMSPlugIn.SetSubject(const ASubject: WideString);
begin
  FSubject := ASubject;
end;

function TCMSPlugIn.GetTags: WideString;
begin
  Result := FTags;
end;

procedure TCMSPlugIn.SetTags(const ATags: WideString);
begin
  FTags := ATags;
end;

function TCMSPlugIn.GetMessage: WideString;
begin
  Result := FMessage;
end;

procedure TCMSPlugIn.SetMessage(const AMessage: WideString);
begin
  FMessage := AMessage;
end;

function TCMSPlugIn.GetWebsite: WideString;
begin
  Result := FWebsite;
end;

procedure TCMSPlugIn.SetWebsite(const AWebsite: WideString);
begin
  FWebsite := AWebsite;
end;

function TCMSPlugIn.GetData: ITabSheetData;
begin
  Result := FData;
end;

procedure TCMSPlugIn.SetData(const AData: ITabSheetData);
begin
  FData := AData;
end;

function TCMSPlugIn.GetArticleID: Integer;
begin
  Result := FArticleID;
end;

procedure TCMSPlugIn.SetArticleID(AArticleID: Integer);
begin
  FArticleID := AArticleID;
end;

function TCMSPlugIn.GetIntelligentPostingHelper;
begin
  Result := FIntelligentPostingHelper;
end;

procedure TCMSPlugIn.SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper);
begin
  FIntelligentPostingHelper := AIntelligentPostingHelper;
end;

constructor TCMSPlugIn.Create;
begin
  inherited Create;
  FCheckedIDsList := TList<TIDInfo>.Create;
  Settings := SettingsClass.Create;
end;

destructor TCMSPlugIn.Destroy;
begin
  FData := nil;
  Settings.Free;
  FCheckedIDsList.Free;
  inherited Destroy;
end;

function TCMSPlugIn.GetType: TPlugInType;
begin
  Result := ptCMS;
end;

function TCMSPlugIn.GetIDs: Integer;
var
  RequestID: Double;
begin
  Result := FCheckedIDsList.Count;

  RequestID := -1;

  LoadSettings;

  if not(AccountName = '') then
    if not Login(RequestID) then
      Exit;

  if (RequestID = -1) then
    RequestID := HTTPManager.Get(THTTPRequest.Create(GetIDsRequestURL), TPlugInHTTPOptions.Create(Self))
  else
    RequestID := HTTPManager.Get(GetIDsRequestURL, RequestID, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  {
    if HTTPProcess.HTTPResult.HasError then
    begin
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    Exit;
    end;
    }

  Result := DoAnalyzeIDsRequest(HTTPManager.GetResult(RequestID).HTTPResult.SourceCode);
end;

function TCMSPlugIn.ReadID(AIndex: Integer): TIDInfo;
begin
  Result := FCheckedIDsList.Items[AIndex];
end;

function TCMSPlugIn.Login(out ARequestID: Double): Boolean;
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

  /// pre login is commonly not used, but reserved for some pre login actions
  /// some CMS need a GET request before log-in (require cookie other security values)
  if NeedPreLogin(PreLoginURL) then
  begin
    HTTPRequest := THTTPRequest.Create(PreLoginURL);
    with HTTPRequest do
    begin
      Referer := Website;
      Charset := Settings.Charset;
    end;

    RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

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

    repeat
      sleep(50);
    until HTTPManager.HasResult(ARequestID);

    HTTPProcess := HTTPManager.GetResult(ARequestID);

    HTTPError := HTTPProcess.HTTPResult.HasError;

    if HTTPError then
      ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

    /// post login is commonly not used, but reserved for some post login actions
    /// some CMS need a GET request after log-in (refresh session, validate login)
    if NeedPostLogin(PostLoginURL) then
    begin
      ARequestID := HTTPManager.Get(PostLoginURL, ARequestID, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      HTTPProcess := HTTPManager.GetResult(ARequestID);

      HTTPError := HTTPProcess.HTTPResult.HasError;

      if HTTPError then
        ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    end;

    Result := not HTTPError and DoAnalyzeLogin(HTTPProcess.HTTPResult.SourceCode, CAPTCHALogin);

    if CAPTCHALogin and DoBuildLoginRequest(HTTPRequest, HTTPParams, HTTPOptions, HTTPProcess.HTTPResult.SourceCode, True) then
    begin
      ARequestID := HTTPManager.Post(HTTPRequest.URL, ARequestID, HTTPParams, HTTPOptions);

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      HTTPProcess := HTTPManager.GetResult(ARequestID);

      HTTPError := HTTPProcess.HTTPResult.HasError;

      if HTTPError then
        ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

      Result := not HTTPError and DoAnalyzeLogin(HTTPProcess.HTTPResult.SourceCode, CAPTCHALogin);
    end;

    DoHandleSessionID(HTTPProcess);
  end;
end;

function TCMSPlugIn.AddArticle(): WordBool;
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

  if LoadSettings(Data) then
    if (not NeedLogin) xor (not SameStr('', AccountName) and NeedLogin and Login(RequestID)) xor SameStr('', AccountName) then
    begin
      if NeedBeforePostAction then
      begin
        if DoBeforePostAction(RequestID) then
        begin
          if not (RequestID = -1) then // no previous request actions
            ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
        end
        else
        begin
          Exit;
        end;
      end;

      if NeedPrePost(PrePostURL) then
      begin
        if (RequestID = -1) then // no previous request actions
          RequestID := HTTPManager.Get(THTTPRequest.Create(PrePostURL), TPlugInHTTPOptions.Create(Self))
        else
          RequestID := HTTPManager.Get(PrePostURL, RequestID, TPlugInHTTPOptions.Create(Self));

        repeat
          sleep(50);
        until HTTPManager.HasResult(RequestID);

        ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

        if not DoAnalyzePrePost(ResponseStr) then
          Exit;
      end;

      if DoBuildPostRequest(Data, HTTPRequest, HTTPParams, HTTPOptions, ResponseStr, RequestID) then
      begin
        HTTPRequest.Method := mPOST;

        if (RequestID = -1) then // no previous request actions
          RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions)
        else
          RequestID := HTTPManager.Post(THTTPRequest.FollowUpClone(HTTPManager.GetResult(RequestID), HTTPRequest), HTTPParams, HTTPOptions);

        repeat
          sleep(50);
        until HTTPManager.HasResult(RequestID);

        HTTPProcess := HTTPManager.GetResult(RequestID);

        HTTPError := HTTPProcess.HTTPResult.HasError;

        if HTTPError then
          ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;

        Result := DoAnalyzePost(HTTPProcess.HTTPResult.SourceCode, HTTPProcess) and not HTTPError;
      end;
    end;
end;

function TCMSPlugIn.EditArticle(): WordBool;
begin
  // TODO: Implement this
  Result := False;
end;

function TCMSPlugIn.GetArticle(out AArticle: WideString): WordBool;
begin
  // TODO: Implement this
  AArticle := '';
  Result := False;
end;

function TCMSPlugIn.ShowWebsiteSettingsEditor(const AWebsiteEditor: IWebsiteEditor): WordBool;

/// taken from Controls unit; saves ~3kb
  function IsPositiveResult(AModalResult: Integer): Boolean;
  begin
    Result := AModalResult in [1, 6, 8, 10];
  end;

begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
