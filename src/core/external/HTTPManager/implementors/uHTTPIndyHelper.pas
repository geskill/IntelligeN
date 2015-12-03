unit uHTTPIndyHelper;

interface

uses
  // Interface
  uHTTPInterface,
  // Const
  uHTTPConst,
  // Delphi
  SysUtils, Classes, Dialogs, StrUtils,
  // Indy
  IdGlobal, IdGlobalProtocols, IdCTypes, IdException, IdURI, IdCharsets, IdHTTP, IdCookieManager, IdCookie, IdZLib, IdCompressorZLib, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdSocks, IdMultipartFormData;

type
  THTTPIndyHelper = class(TIdHTTP)
  private
    FLastRedirect: string;
    FHandleWrongProtocolException: Boolean;
    FHandleSketchyRedirects: Boolean;

    function GetCookieList: string;
    procedure SetCookieList(ACookies: string);
    function GetUseCompressor: Boolean;
    procedure SetUseCompressor(AUseCompressor: Boolean);
    function GetResponseRefresh: string;

    function IsWrongProtocolException(ALowerCaseSourceCode: string): Boolean;

    procedure WriteErrorMsgToStream(AMsg: string; AStream: TStream);

    procedure Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: Boolean; var VMethod: TIdHTTPMethod);
  protected
    FIdCookieManager: TIdCookieManager;
    FIdCompressorZLib: TIdCompressorZLib;
    FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdSocksInfo: TIdSocksInfo;

    procedure DoStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
    procedure DoRequest(const AMethod: TIdHTTPMethod; AURL: string; ASource, AResponseContent: TStream; AIgnoreReplies: array of SmallInt); override;
  public
    constructor Create(AProxy: IProxy = nil); overload;
    constructor Create(AWebsite: string; AProxy: IProxy = nil); overload;
    property LastRedirect: string read FLastRedirect;
    procedure AddCookie(ACookie, AWebsite: string);
    function ResponseContentString: string;
    property CookieList: string read GetCookieList write SetCookieList;
    property UseCompressor: Boolean read GetUseCompressor write SetUseCompressor;

    property HandleWrongProtocolException: Boolean read FHandleWrongProtocolException write FHandleWrongProtocolException;
{$REGION 'Documentation'}
    /// <summary>
    /// <para>
    /// Not all responses which send a
    /// <see href="ms-help://embarcadero.rs2010/Indy/TIdResponseHeaderInfo_Location.html">Location</see>
    /// header are redirected through the
    /// <see href="ms-help://embarcadero.rs2010/Indy/TIdHTTP.html">TIdHTTP</see>
    /// component because only redirects with a specific
    /// <see href="ms-help://embarcadero.rs2010/Indy/TIdHTTPResponse_ResponseCode.html">ResponseCode</see>
    /// are addressed.
    /// </para>
    /// <para>
    /// By default this is True and after a POST-request the unhandled
    /// Location or Response header is processed by a additional
    /// GET-request.
    /// </para>
    /// </summary>
{$ENDREGION}
    property HandleSketchyRedirects: Boolean read FHandleSketchyRedirects write FHandleSketchyRedirects;
{$REGION 'Documentation'}
    /// <summary>
    /// This additional header information is similar to the
    /// <see href="ms-help://embarcadero.rs2010/Indy/TIdResponseHeaderInfo_Location.html">Location</see>
    /// header.
    /// </summary>
    /// <seealso href="http://stackoverflow.com/questions/283752/refresh-http-header">
    /// 'Refresh' HTTP header
    /// </seealso>
{$ENDREGION}
    property Response_Refresh: string read GetResponseRefresh;
    class function Charsets: string;
    destructor Destroy; override;
  end;

implementation

function THTTPIndyHelper.GetCookieList: string;
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

procedure THTTPIndyHelper.SetCookieList(ACookies: string);

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

function THTTPIndyHelper.GetUseCompressor: Boolean;
begin
  Result := Assigned(Compressor);
end;

procedure THTTPIndyHelper.SetUseCompressor(AUseCompressor: Boolean);
begin
  case AUseCompressor of
    True:
      Compressor := FIdCompressorZLib;
    False:
      Compressor := nil;
  end;
end;

function THTTPIndyHelper.GetResponseRefresh: string;
// similar to "Location" header
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

function THTTPIndyHelper.IsWrongProtocolException(ALowerCaseSourceCode: string): Boolean;
begin
  Result := (not(Pos('<body', ALowerCaseSourceCode) = 0));
end;

procedure THTTPIndyHelper.WriteErrorMsgToStream(AMsg: string; AStream: TStream);
begin
  WriteStringToStream(AStream, AMsg, CharsetToEncoding(ResponseCharset));
end;

procedure THTTPIndyHelper.Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: Boolean; var VMethod: TIdHTTPMethod);
begin
  FLastRedirect := dest;
end;

procedure THTTPIndyHelper.DoStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, Request.Host);
end;

procedure THTTPIndyHelper.DoRequest(const AMethod: TIdHTTPMethod; AURL: string; ASource, AResponseContent: TStream; AIgnoreReplies: array of SmallInt);

  function IsPOSTRequest: Boolean;
  begin
    Result := SameStr(Id_HTTPMethodPost, AMethod);
  end;

begin
  try
    inherited DoRequest(AMethod, AURL, ASource, AResponseContent, AIgnoreReplies);
  except
    on E: EDecompressionError do
      ;
    on E: EIdConnClosedGracefully do
      if not IsPOSTRequest then
        raise ;
    on E: EIdHTTPProtocolException do
    begin
      if HandleWrongProtocolException and IsWrongProtocolException(LowerCase(E.ErrorMessage)) then
        // handle normaly for wrong HTTP code responses
        WriteErrorMsgToStream(E.ErrorMessage, AResponseContent)
      else if ((not HandleRedirects) or (RedirectCount < RedirectMaximum)) and (ResponseCode = 302) then
        // don't raise for 302 Found errors
      else
      begin
        raise ;
      end;
    end;
    on Exception do
      raise ;
  end;
  // DO only for POST-request
  if IsPOSTRequest then
    // size = 0 correct? maybe little overhead?
    if HandleSketchyRedirects and (AResponseContent.Size = 0) then
    begin
      if not(Response.Location = '') then
        Get(Response.Location, AResponseContent)
      else if not(Response_Refresh = '') then
        Get(Response_Refresh, AResponseContent);
    end;
end;

constructor THTTPIndyHelper.Create(AProxy: IProxy = nil);
begin
  inherited Create(nil);
  FIdCookieManager := TIdCookieManager.Create(nil);

  FIdCompressorZLib := TIdCompressorZLib.Create(nil);
  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  with FIdSSLIOHandlerSocketOpenSSL do
  begin
    OnStatusInfoEx := DoStatusInfoEx;
    with SSLOptions do
    begin
      Method := sslvTLSv1_2;
      SSLVersions := [sslvTLSv1_2];
    end;
  end;
  FIdSocksInfo := TIdSocksInfo.Create(nil);

  if Assigned(AProxy) then
    if AProxy.Active then
      if not(AProxy.ServerType = ptHTTP) then
        with FIdSocksInfo do
        begin
          Host := AProxy.Server;
          Port := AProxy.Port;

          case AProxy.ServerType of
            ptSOCKS4:
              Version := svSocks4;
            ptSOCKS4A:
              Version := svSocks4A;
            ptSOCKS5:
              Version := svSocks5;
          end;

          case AProxy.RequireAuthentication of
            True:
              Authentication := saUsernamePassword;
            False:
              Authentication := saNoAuthentication;
          end;

          Username := AProxy.AccountName;
          Password := AProxy.AccountPassword;

          Enabled := True;
        end
        else
          with ProxyParams do
          begin
            ProxyServer := AProxy.Server;
            ProxyPort := AProxy.Port;
            BasicAuthentication := AProxy.RequireAuthentication;
            ProxyUsername := AProxy.AccountName;
            ProxyPassword := AProxy.AccountPassword;
          end;

  // TransparentProxy needs FIdSocksInfo class before we can assign this to the TIdHTTP.IOHandler!
  FIdSSLIOHandlerSocketOpenSSL.TransparentProxy := FIdSocksInfo;

  CookieManager := FIdCookieManager;
  Compressor := FIdCompressorZLib;
  IOHandler := FIdSSLIOHandlerSocketOpenSSL;

  AllowCookies := True;
  HandleRedirects := True;
  FHandleWrongProtocolException := True;
  FHandleSketchyRedirects := True;

  // force to use HTTP 1.1
  ProtocolVersion := pv1_1;
  HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];

  OnRedirect := Redirect;
end;

constructor THTTPIndyHelper.Create(AWebsite: string; AProxy: IProxy = nil);
begin
  Create(AProxy);

  ConnectTimeout := 10 * 1000;
  ReadTimeout := 30 * 1000;

  Request.Accept := HTTPRequestAccept;
  Request.AcceptCharSet := HTTPRequestAcceptCharSet;
  Request.AcceptEncoding := HTTPRequestAcceptEncoding;
  Request.AcceptLanguage := HTTPRequestAcceptLanguage;
  Request.Connection := HTTPConnection;
  Request.ContentType := HTTPRequestContentTypeList;
  Request.Referer := AWebsite;
  Request.UserAgent := HTTPRequestUserAgent;
end;

procedure THTTPIndyHelper.AddCookie(ACookie: string; AWebsite: string);
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

function THTTPIndyHelper.ResponseContentString: string;
begin
  Response.ContentStream.Position := 0;
  Result := ReadStringAsCharset(Response.ContentStream, ResponseCharset);
end;

class function THTTPIndyHelper.Charsets: string;
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

destructor THTTPIndyHelper.Destroy;
begin
  FIdSocksInfo.Free;
  FIdSSLIOHandlerSocketOpenSSL.Free;
  FIdCompressorZLib.Free;
  FIdCookieManager.Free;
  inherited Destroy;
end;

end.
