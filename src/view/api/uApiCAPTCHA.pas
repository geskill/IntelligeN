unit uApiCAPTCHA;

interface

uses
  // Delphi
  Controls,
  // API
  uApiConst, uApiSettings,
  // Plugin
  uPlugInConst;

type
  TCAPTCHAClass = class
    class function CAPTCHAInput(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean; stdcall;
  end;

implementation

uses
  uApiPlugins,
  uCAPTCHA;

class function TCAPTCHAClass.CAPTCHAInput(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean;
var
  _handled: Boolean;
  _CAPTCHAPluginIndex: Integer;
  _CAPTCHAType: TCAPTCHAType;
  _CAPTCHAResult, _Cookies: string;
begin
  Result := False;

  _handled := False;

  if (copy(AImageUrl, 1, length(http)) = http) then
    _CAPTCHAType := ctImage
  else
    _CAPTCHAType := ctText;

  for _CAPTCHAPluginIndex := 0 to SettingsManager.Settings.Plugins.CAPTCHA.Count - 1 do
    if TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[_CAPTCHAPluginIndex]).Enabled then
    begin
      _Cookies := ACookies;
      _handled := TApiPlugin.CAPTCHAExec(TPlugInCollectionItem(SettingsManager.Settings.Plugins.CAPTCHA.Items[_CAPTCHAPluginIndex]).Path, _CAPTCHAType,
        AImageUrl, AName, _CAPTCHAResult, _Cookies);
      if _handled then
      begin
        AText := _CAPTCHAResult;
        ACookies := _Cookies;
        Result := True;
        break;
      end;
    end;

  if not _handled then
  begin
    _Cookies := ACookies;
    with TCAPTCHA.Create(_CAPTCHAType, AImageUrl, _Cookies) do
      try
        if (ShowModal = mrOk) then
        begin
          AText := CAPTCHA;
          Result := True;
        end;
      finally
        Free;
      end;
    ACookies := _Cookies;
  end;
end;

end.
