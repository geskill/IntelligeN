unit uJoomla;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp, XMLDoc, XMLIntf, ActiveX, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInConst, uPlugInHTTPClasses;

type
  TJoomla = class abstract(TCMSPlugIn)
  private
    JoomlaSettings: TCMSPlugInSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function CMSType: TCMSType; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TJoomla }

function TJoomla.SettingsClass;
begin
  Result := TCMSPlugInSettings;
end;

function TJoomla.GetSettings;
begin
  Result := JoomlaSettings;
end;

procedure TJoomla.SetSettings;
begin
  JoomlaSettings := ACMSPlugInSettings as TCMSPlugInSettings;
end;

function TJoomla.NeedPreLogin;
begin
  Result := True;
  ARequestURL := Website;
end;

function TJoomla.DoBuildLoginRequest;
var
  _security_input_return, _security_input_1: string;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := APrevResponse;
      Expression := 'type="hidden" name="return" value="(.*?)"';

      if Exec(InputString) then
        _security_input_return := Match[1];

      Expression := 'type="hidden" name="(\w+)" value="1"';

      if Exec(InputString) then
        _security_input_1 := Match[1];
    finally
      Free;
    end;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?task=login');
  with AHTTPRequest do
  begin
    Referer := Website;
    // CharSet := <Settingsclass>.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    // ab Joomla 1.5 // < 1.2 : name="op2" value="login"
    AddFormField('username', (AccountName));
    AddFormField('passwd', (AccountPassword));
    AddFormField('remember', 'yes');
    AddFormField('Submit', '');
    AddFormField('option', 'com_user');
    AddFormField('task', 'login');
    AddFormField('return', (_security_input_return)); // Base64 decoded
    AddFormField(_security_input_1, '1');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
  begin
    RedirectMaximum := 1;
  end;
end;

function TJoomla.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('name="task" value="logout"', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'message.*?\s+<ul>\s+<li>(.*?)<\/li>';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]));
      finally
        Free;
      end;
end;

function TJoomla.DoBuildPostRequest;
begin
  { TODO : implement for you own needs }
  Result := True; // function not implemented, because there is no general system
end;

function TJoomla.DoAnalyzePost;
begin
  { TODO : implement for you own needs }
  Result := True; // function not implemented, because there is no general system
end;

function TJoomla.GetName;
begin
  Result := 'Joomla';
end;

function TJoomla.CMSType;
begin
  Result := cmsBlog;
end;

function TJoomla.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TJoomla.BelongsTo;
begin
  Result := (Pos('content="Joomla! 1.', string(AWebsiteSourceCode)) > 0)
end;

function TJoomla.GetIDs;
begin
  { TODO : implement for you own needs }
  Result := FCheckedIDsList.Count;
end;

function TJoomla.GetArticleLink;
begin
  { TODO : implement for you own needs }
  Result := Format('%s?id=%d', [AURL, AArticleID]);
end;

end.
