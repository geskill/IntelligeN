unit uphpbb2;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, Variants,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  Tphpbb2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fdisable_bbcode, fdisable_smilies, fnotify: Boolean;
  published
    [AttrDefaultValue(False)]
    property disable_bbcode: Boolean read fdisable_bbcode write fdisable_bbcode;
    [AttrDefaultValue(False)]
    property disable_smilies: Boolean read fdisable_smilies write fdisable_smilies;
    [AttrDefaultValue(False)]
    property notify: Boolean read fnotify write fnotify;

    property forums;
    property threads;
  end;

  Tphpbb2 = class(TCMSBoardPlugIn)
  private
    phpbb2Settings: Tphpbb2Settings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean; override;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function Tphpbb2.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, phpbb2Settings, AComponentController);
  with phpbb2Settings do
  begin
    if SameStr('', Charset) then
      Charset := DefaultCharset;

    if Assigned(AComponentController) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tphpbb2.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        AddFormField('username', AccountName, phpbb2Settings.Charset).ContentTransfer := 'binary';
        AddFormField('password', AccountPassword, phpbb2Settings.Charset).ContentTransfer := 'binary';
        AddFormField('login', '', phpbb2Settings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'login.php', Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;
    finally
      Params.Free;
    end;

    if (Pos('login.php?logout=true', ResponseStr) = 0) and (Pos('http-equiv="refresh" content="0;url', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<td align="center" class="row1">(.*?)<\/td>';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
        finally
          Free;
        end;

      Exit;
    end;
  end;
  Result := True;
end;

function Tphpbb2.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'posting.php?mode=reply&t=' + VarToStr(phpbb2Settings.threads))
      else
        // if (ArticleID = 0) then
        // Neues Thema erstellen
        AResponse := Get(Website + 'posting.php?mode=newtopic&f=' + VarToStr(phpbb2Settings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    // TODO: Überprüfung ob der User überhaupt Rechte hat, korrekt eingeloggt wurde
  end;
  Result := True;
end;

function Tphpbb2.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('sid', 'via');
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I: Integer;

  _captcha, _cookies: WideString;
  _captcha_confirm_hash, _captcha_confirm_id, _storecaptcha, _challenge: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
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

        if not CAPTCHAInput(Website + 'vc.php?id=' + _captcha_confirm_hash, GetName + ' VisualConfirmation', _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;

        Params.AddFormField('confirm_id', _captcha_confirm_hash, phpbb2Settings.Charset).ContentTransfer := 'binary';
        Params.AddFormField(_captcha_confirm_id, _captcha, phpbb2Settings.Charset).ContentTransfer := 'binary';
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

        _storecaptcha := Get('http://www.google.com/recaptcha/api/challenge?k=' + _captcha_confirm_hash);

        with TRegExpr.Create do
          try
            InputString := _storecaptcha;
            Expression := 'challenge : ''(.*?)''';

            if Exec(InputString) then
              _challenge := Match[1];

          finally
            Free;
          end;

        if not CAPTCHAInput('http://www.google.com/recaptcha/api/image?c=' + _challenge, 'reCAPTCHA', _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;

        Params.AddFormField('recaptcha_challenge_field', _challenge, phpbb2Settings.Charset).ContentTransfer := 'binary';
        Params.AddFormField('recaptcha_response_field', _captcha, phpbb2Settings.Charset).ContentTransfer := 'binary';
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

        if not CAPTCHAInput(Website + 'profile.php?mode=confirm&id=' + _captcha_confirm_hash, GetName, _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;

        Params.AddFormField('captcha_id', _captcha_confirm_hash, phpbb2Settings.Charset).ContentTransfer := 'binary';
        Params.AddFormField('captcha_code', _captcha, phpbb2Settings.Charset).ContentTransfer := 'binary';
      end;
{$ENDREGION}
      with TRegExpr.Create do
        try
          for I := 0 to length(security_inputs) - 1 do
          begin
            InputString := APrevResponse;
            Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

            if Exec(InputString) then
              Params.AddFormField(security_inputs[I], Match[1], phpbb2Settings.Charset).ContentTransfer := 'binary';
          end;
        finally
          Free;
        end;

      Params.AddFormField('subject', Subject, phpbb2Settings.Charset).ContentTransfer := 'binary';
      Params.AddFormField('message', Message, phpbb2Settings.Charset).ContentTransfer := 'binary';

      with phpbb2Settings do
      begin
        if disable_bbcode then
          Params.AddFormField('disable_bbcode', '', phpbb2Settings.Charset);
        if disable_smilies then
          Params.AddFormField('disable_smilies', '', phpbb2Settings.Charset);
        if notify then
          Params.AddFormField('notify', '', phpbb2Settings.Charset);
      end;

      if PostReply then
      begin
        Params.AddFormField('t', VarToStr(phpbb2Settings.threads), phpbb2Settings.Charset).ContentTransfer := 'binary';
        Params.AddFormField('mode', 'reply', phpbb2Settings.Charset);
      end
      else
      begin
        Params.AddFormField('f', VarToStr(phpbb2Settings.forums), phpbb2Settings.Charset).ContentTransfer := 'binary';
        Params.AddFormField('mode', 'newtopic', phpbb2Settings.Charset);
      end;

      Params.AddFormField('post', 'Submit', phpbb2Settings.Charset);

      try
        // if (ArticleID = 0) then
        ResponseStr := Post(Website + 'posting.php', Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      if (Pos('http-equiv="refresh" content="', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := '<table.*?width="100%" cellspacing="0" cellpadding="1" border="0">(.*?)<\/table>';
            if Exec(InputString) then
              Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
          finally
            Free;
          end;
        Exit;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

function Tphpbb2.GetName;
begin
  Result := 'phpbb2';
end;

constructor Tphpbb2.Create;
begin
  inherited Create;
  phpbb2Settings := Tphpbb2Settings.Create;
end;

destructor Tphpbb2.Destroy;
begin
  phpbb2Settings.Free;
  inherited Destroy;
end;

function Tphpbb2.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tphpbb2.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('profile.php?mode=register', string(AWebsiteSourceCode)) > 0) or (Pos('privmsg.php?folder=inbox', string(AWebsiteSourceCode)) > 0);
end;

function Tphpbb2.GetIDs: Integer;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;

  function CleanPathName(AName: string): string;
  begin
    Result := Trim(HTML2Text(AName));
  end;

begin
  Result := 0;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    LoadSettings;

    if not(AccountName = '') then
      if not Login(IdHTTPHelper) then
        Exit;

    try
      ResponseStr := IdHTTPHelper.Get(Website + 'search.php');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;
  finally
    IdHTTPHelper.Free;
  end;

  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(ResponseStr, 'name="search_forum"', '</select>');
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

function Tphpbb2.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, Tphpbb2Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
