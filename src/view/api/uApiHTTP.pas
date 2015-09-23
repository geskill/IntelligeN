unit uApiHTTP;

interface

uses
  // Delphi
  SysUtils,
  // Indy
  IdHTTP,
  // API
  uApiSettings,
  // HTTPManager
  uHTTPIndyHelper;

type
  TApiHTTP = class(THTTPIndyHelper)
  private
  public
    constructor Create(AProxySubActivation: TProxySubActivation = psaMain);
    constructor CreateAccounting;
  end;

implementation

uses
  // Forms
  uMain;

{ TApiHTTP }

constructor TApiHTTP.Create(AProxySubActivation: TProxySubActivation = psaMain);
begin
  inherited Create(SettingsManager.Settings.HTTP.GetProxy(AProxySubActivation));

  ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
  ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;
end;

constructor TApiHTTP.CreateAccounting;
begin
  Create(psaMain);

  // force to use HTTP 1.0
  //ProtocolVersion := pv1_0;
  //HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];

  Request.ContentType := 'application/x-www-form-urlencoded';
  Request.Referer := '#';
  //Request.UserAgent := 'Mozilla/3.0 (compatible; Indy Library)';
end;

end.
