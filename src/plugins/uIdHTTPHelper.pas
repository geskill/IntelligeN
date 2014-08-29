unit uIdHTTPHelper;

interface

uses
  // Delphi
  SysUtils, Classes, Dialogs, StrUtils,
  // Indy
  IdGlobal, IdURI, IdCharsets, IdHTTP, IdCookieManager, IdCookie, IdZLib, IdCompressorZLib, IdSSLOpenSSL, IdSocks, IdMultipartFormData,
  // Plugin System
  uPlugInInterface, uPlugInConst;

type
  TIdHTTPHelper = class(TIdHTTP)
  private
    FLastRedirect: string;

    function GetCookieList: string;
    procedure SetCookieList(ACookies: string);
    function GetResponseRefresh: string;

    procedure Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
  protected
    FIdCookieManager: TIdCookieManager;
    FIdCompressorZLib: TIdCompressorZLib;
    FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdSocksInfo: TIdSocksInfo;
  public
    constructor Create; overload;
    constructor Create(const APlugIn: IPlugIn); overload;
    property LastRedirect: string read FLastRedirect;
    procedure AddCookie(ACookie, AWebsite: string);
    procedure Get(AURL: string; AResponseContent: TStream); overload;
    function Post(AURL: string; ASource: TStrings; AByteEncoding: TIdTextEncoding = nil): string; overload;
    procedure Post(AURL: string; ASource, AResponseContent: TStream); overload;
    procedure Post(AURL: string; ASource: TIdMultiPartFormDataStream; AResponseContent: TStream); overload;
    property CookieList: string read GetCookieList write SetCookieList;
    property Response_Refresh: string read GetResponseRefresh;
    class function Charsets: string;
    destructor Destroy; override;
  end;

implementation

function TIdHTTPHelper.GetCookieList: string;
var
  I: Integer;
begin
  with TStringList.Create do
    try
      for I := 0 to CookieManager.CookieCollection.Count - 1 do
        Add(CookieManager.CookieCollection.Cookies[I].ServerCookie);

      Result := Text;
    finally
      Free;
    end;
end;

procedure TIdHTTPHelper.SetCookieList(ACookies: string);

  function ExtractUrl(const AURL: string): string;
  var
    I: Integer;
  begin
    I := PosEx('/', AURL, Pos('://', AURL) + 3);
    if I > 0 then
      Result := copy(AURL, 1, I)
    else
      Result := AURL;
  end;

var
  I: Integer;
begin
  with TStringList.Create do
    try
      Text := ACookies;
      for I := 0 to Count - 1 do
        AddCookie(Strings[I], ExtractUrl(Request.Referer));
    finally
      Free;
    end;
end;

function TIdHTTPHelper.GetResponseRefresh: string;
// Ähnlich dem "Location" Header
const
  url = 'url=';
var
  _RefreshHeader: string;
begin
  _RefreshHeader := LowerCase(Response.RawHeaders.Values['Refresh']);
  Result := '';
  if (Pos(url, _RefreshHeader) > 0) then
    Result := copy(_RefreshHeader, Pos(url, _RefreshHeader) + length(url));
end;

procedure TIdHTTPHelper.Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
begin
  FLastRedirect := dest;
end;

constructor TIdHTTPHelper.Create();
begin
  inherited Create(nil);
  FIdCookieManager := TIdCookieManager.Create(nil);
  FIdCompressorZLib := TIdCompressorZLib.Create(nil);
  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FIdSocksInfo := TIdSocksInfo.Create(nil);

  OnRedirect := Redirect;
end;

constructor TIdHTTPHelper.Create(const APlugIn: IPlugIn);
var
  _ICMSPlugin: ICMSPlugIn;
begin
  Create();

  with APlugIn do
    if Proxy.Active then
      if not(Proxy.ServerType = ptHTTP) then
        with FIdSocksInfo do
        begin
          Host := Proxy.Server;
          Port := Proxy.Port;
          if (Proxy.ServerType = ptSOCKS4) then
            Version := svSocks4
          else
            Version := svSocks5;
          Username := Proxy.AccountName;
          Password := Proxy.AccountPassword;

          Enabled := True;
        end
        else
          with ProxyParams do
          begin
            ProxyServer := Proxy.Server;
            ProxyPort := Proxy.Port;
            ProxyUsername := Proxy.AccountName;
            ProxyPassword := Proxy.AccountPassword;
          end;

  FIdSSLIOHandlerSocketOpenSSL.TransparentProxy := FIdSocksInfo;

  CookieManager := FIdCookieManager;
  Compressor := FIdCompressorZLib;
  IOHandler := FIdSSLIOHandlerSocketOpenSSL;

  AllowCookies := True;
  HandleRedirects := True;

  ConnectTimeout := APlugIn.ConnectTimeout;
  ReadTimeout := APlugIn.ReadTimeout;

  ProtocolVersion := pv1_1;
  HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];

  Request.Accept := 'text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1';
  Request.AcceptCharSet := 'iso-8859-1, utf-8, utf-16, *;q=0.1';
  Request.AcceptEncoding := 'deflate, gzip, identity, *;q=0';
  Request.AcceptLanguage := 'de-DE,de;q=0.9,en;q=0.8';
  Request.Connection := 'Keep-Alive';
  Request.ContentType := 'application/x-www-form-urlencoded';
  if Supports(APlugIn, ICMSPlugIn) then
  begin
    if APlugIn.QueryInterface(ICMSPlugIn, _ICMSPlugin) = 0 then
      try
        Request.Referer := _ICMSPlugin.Website;
      finally
        _ICMSPlugin := nil;
      end;
  end;
  Request.UserAgent := 'Opera/9.80 (Windows NT 6.1; U; de) Presto/2.9.168 Version/11.51';
  ReuseSocket := rsTrue;
end;

procedure TIdHTTPHelper.AddCookie(ACookie: string; AWebsite: string);
var
  IdURI: TIdURI;
begin
  IdURI := TIdURI.Create(AWebsite);
  try
    CookieManager.AddServerCookie(ACookie, IdURI);
  finally
    IdURI.Free;
  end;
end;

procedure TIdHTTPHelper.Get(AURL: string; AResponseContent: TStream);
begin
  try
    inherited Get(AURL, AResponseContent);
  except
    on E: EDecompressionError do
      ;
    on E: EIdHTTPProtocolException do
    begin
      if not(Pos('<body', LowerCase(E.ErrorMessage)) = 0) then
      begin
        if AResponseContent.InheritsFrom(TStringStream) then
          TStringStream(AResponseContent).WriteString(E.ErrorMessage);
      end
      else
        raise ;
    end;
  end;
end;

function TIdHTTPHelper.Post(AURL: string; ASource: TStrings; AByteEncoding: TIdTextEncoding = nil): string;
begin
  try
    Result := inherited Post(AURL, ASource, AByteEncoding);
  except
    on E: EDecompressionError do
      ;
    on E: EIdHTTPProtocolException do
    begin
      if not(Pos('<body', LowerCase(E.ErrorMessage)) = 0) then
        Result := E.ErrorMessage
      else
        raise ;
    end;
  end;
  if SameStr('', Result) then
  begin
    if not(Response.Location = '') then
      Result := Get(Response.Location)
    else if not(Response_Refresh = '') then
      Result := Get(Response_Refresh);
  end;
end;

procedure TIdHTTPHelper.Post(AURL: string; ASource, AResponseContent: TStream);
begin
  try
    inherited Post(AURL, ASource, AResponseContent);
  except
    on E: EDecompressionError do
      ;
    on E: EIdHTTPProtocolException do
    begin
      if not(Pos('<body', LowerCase(E.ErrorMessage)) = 0) then
      begin
        if AResponseContent.InheritsFrom(TStringStream) then
          TStringStream(AResponseContent).WriteString(E.ErrorMessage);
      end
      else
        raise ;
    end;
  end;
  if AResponseContent.InheritsFrom(TStringStream) and (TStringStream(AResponseContent).DataString = '') then
  begin
    if not(Response.Location = '') then
      Get(Response.Location, AResponseContent)
    else if not(Response_Refresh = '') then
      Get(Response_Refresh, AResponseContent);
  end;
end;

procedure TIdHTTPHelper.Post(AURL: string; ASource: TIdMultiPartFormDataStream; AResponseContent: TStream);
begin
  Assert(ASource <> nil);
  Request.ContentType := ASource.RequestContentType;
  Post(AURL, TStream(ASource), AResponseContent);
end;

class function TIdHTTPHelper.Charsets: string;
var
  Lcset: TIdCharset;
begin
  with TStringList.Create do
    try
      for Lcset := TIdCharset(1) to high(TIdCharset) do
        Add(IdCharsetNames[Lcset]);

      Result := Text;
    finally
      Free;
    end;
end;

destructor TIdHTTPHelper.Destroy;
begin
  FIdSocksInfo.Free;
  FIdSSLIOHandlerSocketOpenSSL.Free;
  FIdCompressorZLib.Free;
  FIdCookieManager.Free;
  inherited Destroy;
end;

end.
