unit uApiCAPTCHA;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, AxCtrls, ActiveX,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // API
  uApiSettings,
  // Plugin
  uPlugInConst;

type
  TCAPTCHAClass = class
  private
    class procedure CAPTCHADownload(const ACAPTCHA: WideString; ACAPTCHAMemoryStream: TMemoryStream; var ACookies: WideString; const AProxy: IProxy = nil; AConnectTimeout: Integer = 5000; AReadTimeout: Integer = 10000);
  public
    class function CAPTCHAInput(const ACAPTCHA: WideString; const AName: WideString; out AText: WideString; var ACookies: WideString): WordBool; safecall;
  end;

implementation

uses
  uApiPlugins,
  uCAPTCHA;

class procedure TCAPTCHAClass.CAPTCHADownload(const ACAPTCHA: WideString; ACAPTCHAMemoryStream: TMemoryStream; var ACookies: WideString; const AProxy: IProxy = nil; AConnectTimeout: Integer = 5000; AReadTimeout: Integer = 10000);
var
  LHTTPManager: IHTTPManager;
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
  LOleStream: TOleStream;
  LDummy: Int64;
begin
  LHTTPManager := THTTPManager.Instance();

  LHTTPRequest := THTTPRequest.Create(ACAPTCHA);
  with LHTTPRequest do
  begin
    Referer := ACAPTCHA; // i.e. for DLE
    Cookies.Text := ACookies;
  end;

  LHTTPOptions := THTTPOptions.Create(AProxy);
  with LHTTPOptions do
  begin
    ConnectTimeout := AConnectTimeout;
    ReadTimeout := AReadTimeout;
  end;

  LRequestID := LHTTPManager.Get(LHTTPRequest, LHTTPOptions);

  repeat
    sleep(75);
  until LHTTPManager.HasResult(LRequestID);

  LHTTPProcess := LHTTPManager.GetResult(LRequestID);

  ACookies := LHTTPProcess.HTTPResult.HTTPResponse.Cookies.Text;

  LOleStream := TOleStream.Create(LHTTPProcess.HTTPResult.HTTPResponse.ContentStream);
  try
    LHTTPProcess.HTTPResult.HTTPResponse.ContentStream.Seek(0, STREAM_SEEK_SET, LDummy);
    LOleStream.Seek(0, STREAM_SEEK_SET);
    ACAPTCHAMemoryStream.CopyFrom(LOleStream, LOleStream.Size);
  finally
    LOleStream.Free;
  end;

  ACAPTCHAMemoryStream.Position := 0;

  LHTTPProcess := nil;
  LHTTPOptions := nil;
  LHTTPRequest := nil;
  LHTTPManager := nil;
end;

class function TCAPTCHAClass.CAPTCHAInput(const ACAPTCHA: WideString; const AName: WideString; out AText: WideString; var ACookies: WideString): WordBool;
var
  LHandled: Boolean;
  LCAPTCHAPluginIndex: Integer;
  LCAPTCHAType: TCAPTCHAType;
  LCAPTCHAResult, LCookies: string;

  LCAPTCHAMemoryStream: TMemoryStream;
begin
  Result := False;

  LHandled := False;

  if (copy(ACAPTCHA, 1, length(HTTP)) = HTTP) then
    LCAPTCHAType := ctImage
  else
    LCAPTCHAType := ctText;

  for LCAPTCHAPluginIndex := 0 to SettingsManager.Settings.Plugins.CAPTCHA.Count - 1 do
    if TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[LCAPTCHAPluginIndex]).Enabled then
    begin
      LCookies := ACookies;
      LHandled := TApiPlugin.CAPTCHAExec(TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[LCAPTCHAPluginIndex]).Path, LCAPTCHAType, ACAPTCHA, AName, LCAPTCHAResult, LCookies);
      if LHandled then
      begin
        AText := LCAPTCHAResult;
        ACookies := LCookies;
        Result := True;
        break;
      end;
    end;

  if not LHandled then
  begin
    LCAPTCHAMemoryStream := TMemoryStream.Create;
    try
      if (LCAPTCHAType = ctImage) then
      begin
        TCAPTCHAClass.CAPTCHADownload(ACAPTCHA, LCAPTCHAMemoryStream, ACookies,
          // TODO: Read settings at a thread-safe position
          { . } SettingsManager.Settings.HTTP.GetProxy(psaMain), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
      end;

      with TCAPTCHA.Create(nil) do
        try
          if (LCAPTCHAType = ctImage) then
            DisplayCAPTCHAImage(LCAPTCHAMemoryStream)
          else
            DisplayCAPTCHAText(ACAPTCHA);

          if (ShowModal = mrOk) then
          begin
            AText := CAPTCHA;
            Result := True;
          end;
        finally
          Free;
        end;
    finally
      LCAPTCHAMemoryStream.Free;
    end;
  end;
end;

end.
