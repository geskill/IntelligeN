unit uphpbb2;

interface

uses
  // Delphi
  SysUtils, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  Tphpbb2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fdisable_bbcode, fdisable_smilies, fnotify: Boolean;
  public
    constructor Create; override;
  published
    property disable_bbcode: Boolean read fdisable_bbcode write fdisable_bbcode;
    property disable_smilies: Boolean read fdisable_smilies write fdisable_smilies;
    property notify: Boolean read fnotify write fnotify;

    property forums;
    property threads;
  end;

  Tphpbb2 = class(TCMSBoardPlugIn)
  private
    phpbb2Settings: Tphpbb2Settings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(const AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
  end;

implementation

{ Tphpbb2Settings }

constructor Tphpbb2Settings.Create;
begin
  inherited Create;

  // default setup
  disable_bbcode := False;
  disable_smilies := False;
  notify := False;
end;

{ Tphpbb2 }

function Tphpbb2.SettingsClass;
begin
  Result := Tphpbb2Settings;
end;

function Tphpbb2.GetSettings;
begin
  Result := phpbb2Settings;
end;

procedure Tphpbb2.SetSettings;
begin
  phpbb2Settings := ACMSPlugInSettings as Tphpbb2Settings;
end;

function Tphpbb2.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with phpbb2Settings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tphpbb2.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'login.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := phpbb2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('login', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tphpbb2.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('login.php?logout=true', AResponseStr) = 0) and (Pos('http-equiv="refresh" content="0;url', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<td align="center" class="row1">(.*?)<\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function Tphpbb2.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'posting.php?mode=reply&t=' + VarToStr(phpbb2Settings.threads)
  else
    // if (ArticleID = 0) then
    // Neues Thema erstellen
    ARequestURL := Website + 'posting.php?mode=newtopic&f=' + VarToStr(phpbb2Settings.forums);
end;

function Tphpbb2.DoAnalyzePrePost;
begin
  Result := True;
  // TODO: Überprüfung ob der User überhaupt Rechte hat, korrekt eingeloggt wurde
end;

function Tphpbb2.DoBuildPostRequest;
const
  security_inputs: array [0 .. 1] of string = ('sid', 'via');
var
  I: Integer;

  RequestID: Double;

  _captcha, _cookies: WideString;
  _captcha_confirm_hash, _captcha_confirm_id, _storecaptcha, _challenge: string;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'posting.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := phpbb2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
{$REGION 'CAPTCHA'}
    if not(Pos('name="confirm_id"', APrevResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="confirm_id" value="(.*?)"';

          if Exec(InputString) then
            _captcha_confirm_hash := Match[1];
        finally
          Free;
        end;

      with TRegExpr.Create do
        try
          ModifierS := False;
          InputString := APrevResponse;
          Expression := '<input type="text".*?name="(\w+)".*?maxlength="10"';

          if Exec(InputString) then
            _captcha_confirm_id := Match[1];
        finally
          Free;
        end;

      if not CAPTCHAInput(Website, Subject, Website + 'vc.php?id=' + _captcha_confirm_hash, GetName + ' VisualConfirmation', _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;

      AddFormField('confirm_id', _captcha_confirm_hash);
      AddFormField(_captcha_confirm_id, _captcha);
    end
    else if not(Pos('name="recaptcha_challenge_field"', APrevResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'challenge\?k=(.*?)"';

          if Exec(InputString) then
            _captcha_confirm_hash := Match[1];
        finally
          Free;
        end;

      RequestID := HTTPManager.Get(THTTPRequest.Create('http://www.google.com/recaptcha/api/challenge?k=' + _captcha_confirm_hash), TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID);

      _storecaptcha := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

      // _storecaptcha := Get('http://www.google.com/recaptcha/api/challenge?k=' + _captcha_confirm_hash);

      with TRegExpr.Create do
        try
          InputString := _storecaptcha;
          Expression := 'challenge : ''(.*?)''';

          if Exec(InputString) then
            _challenge := Match[1];

        finally
          Free;
        end;

      if not CAPTCHAInput(Website, Subject, 'http://www.google.com/recaptcha/api/image?c=' + _challenge, 'reCAPTCHA', _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;

      AddFormField('recaptcha_challenge_field', _challenge);
      AddFormField('recaptcha_response_field', _captcha);
    end
    else if not(Pos('name="captcha_code"', APrevResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="captcha_id" value="(.*?)"';

          if Exec(InputString) then
            _captcha_confirm_hash := Match[1];
        finally
          Free;
        end;

      if not CAPTCHAInput(Website, Subject, Website + 'profile.php?mode=confirm&id=' + _captcha_confirm_hash, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;

      AddFormField('captcha_id', _captcha_confirm_hash);
      AddFormField('captcha_code', _captcha);
    end;
{$ENDREGION}
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

          if Exec(InputString) then
            AddFormField(security_inputs[I], Match[1]);
        end;
      finally
        Free;
      end;

    AddFormField('subject', Subject);
    AddFormField('message', Message);

    with phpbb2Settings do
    begin
      if disable_bbcode then
        AddFormField('disable_bbcode', '');
      if disable_smilies then
        AddFormField('disable_smilies', '');
      if notify then
        AddFormField('notify', '');
    end;

    if PostReply then
    begin
      AddFormField('t', VarToStr(phpbb2Settings.threads));
      AddFormField('mode', 'reply');
    end
    else
    begin
      AddFormField('f', VarToStr(phpbb2Settings.forums));
      AddFormField('mode', 'newtopic');
    end;

    AddFormField('post', 'Submit');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tphpbb2.DoAnalyzePost;
begin
  Result := not(Pos('http-equiv="refresh" content="', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<table.*?width="100%" cellspacing="0" cellpadding="1" border="0">(.*?)<\/table>';
        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function Tphpbb2.DoAnalyzeIDsRequest;

  function CleanPathName(AName: string): string;
  begin
    Result := Trim(HTML2Text(AName));
  end;

begin
  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(AResponseStr, 'name="search_forum"', '</select>');
      Expression := '<option value="(\d+)">(.*?)<\/';

      if Exec(InputString) then
      begin
        repeat
          AddID(Match[1], CleanPathName(Match[2]));
        until not ExecNext;
      end;
    finally
      Free;
    end;
  Result := FCheckedIDsList.Count;
end;

function Tphpbb2.GetName;
begin
  Result := 'phpbb2';
end;

function Tphpbb2.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tphpbb2.BelongsTo;
begin
  Result := (Pos('profile.php?mode=register', string(AWebsiteSourceCode)) > 0) or (Pos('privmsg.php?folder=inbox', string(AWebsiteSourceCode)) > 0);
end;

end.
