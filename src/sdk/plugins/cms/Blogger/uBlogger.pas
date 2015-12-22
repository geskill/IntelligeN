unit uBlogger;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, HTTPApp, StrUtils, Math,
  // RegEx
  RegExpr,
  // LkJSON
  uLkJSON,
  // Utils,
  uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInHTTPClasses;

type
  TBloggerSettings = class(TCMSBlogPlugInSettings)
  strict private
    fid, fcode: string;
  public
    constructor Create; override;
  published
    property id: string read fid write fid;
    property code: string read fcode write fcode;

    property categorys;
  end;

  TBlogger = class(TCMSBlogPlugIn)
  private
    BloggerSettings: TBloggerSettings;
    Faccess_token: string;

  const
    GOOGLE_API = 'https://accounts.google.com/';
    GOOGLE_OAUTH2 = GOOGLE_API + 'o/oauth2/';
    GOOGLE_SCOPE = 'https://www.googleapis.com/auth/blogger';
    GOOGLE_CLIENT_ID = '812817183224.apps.googleusercontent.com';
    GOOGLE_CLIENT_SECRET = '2VlK2Vi1T1y0KefjGluryZj6';
    GOOGLE_REDIRECT_URI = 'urn:ietf:wg:oauth:2.0:oob';
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;
    procedure DoHandleSessionID(AHTTPProcess: IHTTPProcess); override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override;
    function GetIDs: Integer; override;
  end;

implementation

{ TBloggerSettings }

constructor TBloggerSettings.Create;
begin
  inherited Create;

  // default setup
  id := '';
  code := '';
end;

{ TBlogger }

function TBlogger.SettingsClass: TCMSPlugInSettingsMeta;
begin
  Result := TBloggerSettings;
end;

function TBlogger.GetSettings;
begin
  Result := BloggerSettings;
end;

procedure TBlogger.SetSettings;
begin
  BloggerSettings := ACMSPlugInSettings as TBloggerSettings;
end;

function TBlogger.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with BloggerSettings do
  begin
    if Assigned(AData) then
    begin
      if SameStr('', id) then
      begin
        ErrorMsg := 'blogID is incorrect or undefinded!';
        Result := False;
      end
      else if SameStr('', code) then
      begin
        ErrorMsg := 'authorizationCode is incorrect or undefinded!';
        Result := False;
      end;
    end;
  end;
end;

function TBlogger.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(GOOGLE_OAUTH2 + 'token');

  // ?code=' + HTTPEncode(BloggerSettings.code) + '&client_id=' + HTTPEncode(GOOGLE_CLIENT_ID) + '&client_secret=' + HTTPEncode(GOOGLE_CLIENT_SECRET) + '&redirect_uri=' + HTTPEncode
  // (GOOGLE_REDIRECT_URI) + '&grant_type=authorization_code');
  with AHTTPRequest do
  begin
    Accept := '';
    AcceptCharSet := '';
    AcceptEncoding := '';
    AcceptLanguage := '';
    Host := 'accounts.google.com';
    Referer := '';
    UserAgent := '';
    Charset := BloggerSettings.Charset;
    Connection := '';
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('code', BloggerSettings.code);
    AddFormField('client_id', GOOGLE_CLIENT_ID);
    AddFormField('client_secret', GOOGLE_CLIENT_SECRET);
    AddFormField('redirect_uri', GOOGLE_REDIRECT_URI);
    AddFormField('grant_type', 'authorization_code');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
  begin
    RedirectMaximum := 1;
  end;
end;

function TBlogger.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := Pos('access_token', AResponseStr) > 0;
end;

procedure TBlogger.DoHandleSessionID(AHTTPProcess: IHTTPProcess);
var
  LlkJSONobject: TlkJSONobject;
begin
  LlkJSONobject := TlkJSON.ParseText(AHTTPProcess.HTTPResult.SourceCode) as TlkJSONobject;
  try
    Faccess_token := LlkJSONobject.Field['access_token'].Value;
  finally
    LlkJSONobject.Free;
  end;
end;

function TBlogger.DoBuildPostRequest;
var
  LlkJSONobject: TlkJSONobject;
  LBlogObject: TlkJSONobject;
  LLabelsList: TlkJSONlist;
  LStringList: TStrings;
  LLabel, LJSONStr: string;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create('https://www.googleapis.com/blogger/v3/blogs/' + BloggerSettings.id + '/posts/');
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := BloggerSettings.Charset;

    ContentType := 'application/json';

    CustomHeaders.Add('Authorization: Bearer ' + Faccess_token);
  end;

  LJSONStr := '';

  LlkJSONobject := TlkJSONobject.Create;
  try
    LlkJSONobject.Add('kind', 'blogger#post');

    LBlogObject := TlkJSONobject.Create;
    LBlogObject.Add('id', BloggerSettings.id);

    LlkJSONobject.Add('blog', LBlogObject);

    LlkJSONobject.Add('title', Subject);
    LlkJSONobject.Add('content', StringReplace(Message, sLineBreak, '<br />', [rfReplaceAll]));

    LLabelsList := TlkJSONlist.Create();

    LStringList := SplittString(',', VarToStr(BloggerSettings.categorys));
    try
      for LLabel in LStringList do
        LLabelsList.Add(LLabel);
    finally
      LStringList.Free;
    end;

    LlkJSONobject.Add('labels', LLabelsList);

    LJSONStr := TlkJSON.GenerateText(LlkJSONobject);
  finally
    LlkJSONobject.Free;
  end;

  AHTTPParams := THTTPParams.Create(LJSONStr);

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
  begin
    RedirectMaximum := 1;
  end;
end;

function TBlogger.DoAnalyzePost;
begin
  Result := AHTTPProcess.HTTPResult.HTTPResponse.code = 201;
  if not Result then
    ErrorMsg := AHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage + sLineBreak + AResponseStr;
end;

function TBlogger.GetIDsRequestURL;
begin
  Result := Website + 'feeds/posts/default?alt=json-in-script';
end;

function TBlogger.DoAnalyzeIDsRequest;
var
  Categories: string;
begin
  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := '"category":(.*?),"';

      if Exec(InputString) then
      begin
        Categories := Match[1];

        with TRegExpr.Create do
          try
            InputString := Categories;
            Expression := '"term":"(.*?)"';

            if Exec(InputString) then
            begin
              repeat
                AddID(Match[1], Match[1]);
              until not ExecNext;
            end;
          finally
            Free;
          end;
      end;
    finally
      Free;
    end;
  Result := FCheckedIDsList.Count;
end;

function TBlogger.GetName;
begin
  Result := 'Blogger';
end;

function TBlogger.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TBlogger.BelongsTo;
begin
  Result := (Pos('content=''blogger'' name=''generator''', string(AWebsiteSourceCode)) > 0) or (Pos('name="generator" content="Blogger"', string(AWebsiteSourceCode)) > 0) or (Pos('content="Blogger" name="generator"', string(AWebsiteSourceCode)) > 0);
end;

function TBlogger.GetIDs;
var
  RequestID: Double;
begin
  Result := FCheckedIDsList.Count;

  LoadSettings;

  RequestID := HTTPManager.Get(THTTPRequest.Create(GetIDsRequestURL), TPlugInHTTPOptions.Create(Self));

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

end.
