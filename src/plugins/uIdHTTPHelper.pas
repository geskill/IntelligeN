unit uIdHTTPHelper;

interface

uses
  // Delphi
  SysUtils, Classes, Dialogs, StrUtils,
  // Indy
  IdGlobal, IdGlobalProtocols, IdCTypes, IdException, IdURI, IdCharsets, IdHTTP, IdCookieManager, IdCookie, IdZLib, IdCompressorZLib, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdSocks, IdMultipartFormData,
  // Plugin System
  uPlugInInterface, uPlugInConst;

type
  TIdHTTPHelper = class(TIdHTTP)
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
    constructor Create; overload;
    constructor Create(const APlugIn: IPlugIn); overload;
    property LastRedirect: string read FLastRedirect;
    procedure AddCookie(ACookie, AWebsite: string);
    function ResponseContentString: string;
    property CookieList: string read GetCookieList write SetCookieList;
    property UseCompressor: Boolean read GetUseCompressor write SetUseCompressor;

    property HandleWrongProtocolException: Boolean read FHandleWrongProtocolException write FHandleWrongProtocolException;
    {$REGION 'Documentation'}
    /// <summary>
    ///   <para>
    ///     Not all responses which send a <see href="ms-help://embarcadero.rs2010/Indy/TIdResponseHeaderInfo_Location.html">
    ///     Location</see> header are redirected through the <see href="ms-help://embarcadero.rs2010/Indy/TIdHTTP.html">
    ///     TIdHTTP</see> component because only redirects with a specific <see href="ms-help://embarcadero.rs2010/Indy/TIdHTTPResponse_ResponseCode.html">
    ///     ResponseCode</see> are addressed.
    ///   </para>
    ///   <para>
    ///     By default this is True and after a POST-request the unhandled
    ///     Location or Response header is processed by a additional
    ///     GET-request.
    ///   </para>
    /// </summary>
    {$ENDREGION}
    property HandleSketchyRedirects: Boolean read FHandleSketchyRedirects write FHandleSketchyRedirects;
    {$REGION 'Documentation'}
    /// <summary>
    ///   This additional header information is similar to the <see href="ms-help://embarcadero.rs2010/Indy/TIdResponseHeaderInfo_Location.html">
    ///   Location</see> header.
    /// </summary>
    /// <seealso href="http://stackoverflow.com/questions/283752/refresh-http-header">
    ///   'Refresh' HTTP header
    /// </seealso>
    {$ENDREGION}
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

function TIdHTTPHelper.GetUseCompressor: Boolean;
begin
  Result := Assigned(Compressor);
end;

procedure TIdHTTPHelper.SetUseCompressor(AUseCompressor: Boolean);
begin
  case AUseCompressor of
    True:
      Compressor := FIdCompressorZLib;
    False:
      Compressor := nil;
  end;
end;

function TIdHTTPHelper.GetResponseRefresh: string;
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

function TIdHTTPHelper.IsWrongProtocolException(ALowerCaseSourceCode: string): Boolean;
begin
  Result := (not(Pos('<body', ALowerCaseSourceCode) = 0));
end;

procedure TIdHTTPHelper.WriteErrorMsgToStream(AMsg: string; AStream: TStream);
begin
  WriteStringToStream(AStream, AMsg, CharsetToEncoding(Response.CharSet));
end;

procedure TIdHTTPHelper.Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: Boolean; var VMethod: TIdHTTPMethod);
begin
  FLastRedirect := dest;
end;

procedure TIdHTTPHelper.DoStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, Request.Host);
end;

procedure TIdHTTPHelper.DoRequest(const AMethod: TIdHTTPMethod; AURL: string; ASource, AResponseContent: TStream; AIgnoreReplies: array of SmallInt);

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

constructor TIdHTTPHelper.Create(const APlugIn: IPlugIn);
var
  _ICMSPlugin: ICMSPlugIn;
begin
  Create();

  with FIdSSLIOHandlerSocketOpenSSL do
  begin
    OnStatusInfoEx := DoStatusInfoEx;
    with SSLOptions do
    begin
      //Method := sslvTLSv1_2;
      //SSLVersions := [sslvTLSv1_2];
    end;
  end;

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

  // TransparentProxy needs FIdSocksInfo class before we can assign this to the TIdHTTP.IOHandler!
  FIdSSLIOHandlerSocketOpenSSL.TransparentProxy := FIdSocksInfo;

  CookieManager := FIdCookieManager;
  Compressor := FIdCompressorZLib;
  IOHandler := FIdSSLIOHandlerSocketOpenSSL;

  AllowCookies := True;
  HandleRedirects := True;
  FHandleWrongProtocolException := True;
  FHandleSketchyRedirects := True;

  ConnectTimeout := APlugIn.ConnectTimeout;
  ReadTimeout := APlugIn.ReadTimeout;

  // force to use HTTP 1.1
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

constructor TIdHTTPHelper.Create();
begin
  inherited Create(nil);
  FIdCookieManager := TIdCookieManager.Create(nil);
  FIdCompressorZLib := TIdCompressorZLib.Create(nil);
  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FIdSocksInfo := TIdSocksInfo.Create(nil);

  OnRedirect := Redirect;
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

function TIdHTTPHelper.ResponseContentString: string;
begin
  Response.ContentStream.Position := 0;
  Result := ReadStringAsCharset(Response.ContentStream, Response.CharSet);
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
