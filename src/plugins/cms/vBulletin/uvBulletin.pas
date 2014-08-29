unit uvBulletin;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, HTTPApp, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TvBulletinSettings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field, ficon_field: string;
    fuse_coverlink, fattach_sig, fparseurl, fdisablesmilies, fopenclose, fstickunstick, fintelligent_posting, fintelligent_posting_helper,
      fintelligent_posting_mask_search, fintelligent_posting_keepshortwords, fintelligent_posting_alternativesearch,
      fintelligent_posting_boundedsearch: Boolean;

    fprefix, ficon: Variant;
  public
    intelligent_posting_bounds: TIntegerArray;
  published
    [AttrDefaultValue('prefixid')]
    property prefix_field: string read fprefix_field write fprefix_field;
    [AttrDefaultValue('iconid')]
    property icon_field: string read ficon_field write ficon_field;
    [AttrDefaultValue(False)]
    property use_coverlink: Boolean read fuse_coverlink write fuse_coverlink;
    [AttrDefaultValue(True)]
    property attach_sig: Boolean read fattach_sig write fattach_sig;
    [AttrDefaultValue(True)]
    property parseurl: Boolean read fparseurl write fparseurl;
    [AttrDefaultValue(False)]
    property disablesmilies: Boolean read fdisablesmilies write fdisablesmilies;
    [AttrDefaultValue(False)]
    property openclose: Boolean read fopenclose write fopenclose;
    [AttrDefaultValue(False)]
    property stickunstick: Boolean read fstickunstick write fstickunstick;
    [AttrDefaultValue(False)]
    property intelligent_posting: Boolean read fintelligent_posting write fintelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper: Boolean read fintelligent_posting_helper write fintelligent_posting_helper;
    [AttrDefaultValue(False)]
    property intelligent_posting_mask_search: Boolean read fintelligent_posting_mask_search write fintelligent_posting_mask_search;
    [AttrDefaultValue(False)]
    property intelligent_posting_keepshortwords: Boolean read fintelligent_posting_keepshortwords write fintelligent_posting_keepshortwords;
    [AttrDefaultValue(False)]
    property intelligent_posting_alternativesearch: Boolean read fintelligent_posting_alternativesearch write
      fintelligent_posting_alternativesearch;
    [AttrDefaultValue(False)]
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  TvBulletin = class(TCMSBoardIPPlugIn)
  private
    vBulletinSettings: TvBulletinSettings;
    FSessionID: string;

  const
    security_inputs: array [0 .. 3] of string = ('securitytoken', 'posthash', 'poststarttime', 'loggedinuser');
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    procedure PreSearchPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
    function IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
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

function TvBulletin.LoadSettings;
begin
  Result := True;
  vBulletinSettings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, vBulletinSettings,
    AComponentController);
  with vBulletinSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AComponentController) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TvBulletin.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    RedirectMaximum := 3; // need 3 for http://forums.shooshtime.com/
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('vb_login_username=' + AccountName);
        Add('vb_login_password=' + AccountPassword);
        Add('securitytoken=guest');
        Add('do=login');
        Add('vb_login_md5password=');
        Add('vb_login_md5password_utf=');
        Add('cookieuser=1');
        Add('s=');
      end;

      Request.CharSet := vBulletinSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'login.php?do=login', Params, Enc);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;
      finally
        Enc.Free;
      end;
    finally
      Params.Free;
    end;

    if (Pos('<blockquote>', ResponseStr) = 0) and (Pos('http-equiv="Refresh" content="2', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
          Expression := '<div style="margin: 10px">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              Self.ErrorMsg := HTML2Text(Trim(Match[1]));
            until not ExecNext;
            Exit;
          end;
        finally
          Free;
        end;
      end;
    end;

    with TRegExpr.Create do
      try
        InputString := ResponseStr;
        Expression := 's=(\w+)(&|")';

        if Exec(InputString) then
          FSessionID := Match[1];
      finally
        Free;
      end;

    RedirectMaximum := 1; // if >1 problems with search
  end;
  Result := True;
end;

procedure TvBulletin.PreSearchPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
begin
  with AIdHTTPHelper do
    try
      AResponse := Get(Website + 'search.php?search_type=1&s=' + FSessionID);
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;
end;

function TvBulletin.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

  function GetSearchTitle(ATitle: string; AKeepShortWords: Boolean = False; AMaskSearch: Boolean = False;
    AAlternativeSearch: Boolean = False): string;
  var
    X, _current_length: Integer;
  begin
    Result := ATitle;
    for X := length(Result) downto 1 do
    begin
      if CharInSet(Result[X], ['+', '_', '.', ':', '(', ')', '[', ']', '/', '\']) then
        Result[X] := ' ';
      if (Result[X] = '-') and not AAlternativeSearch then
        Result[X] := ' ';
    end;
    _current_length := 0;
    if not AKeepShortWords and not AMaskSearch then
    begin
      Result := ' ' + Result;
      for X := length(Result) downto 1 do
      begin
        if (not(Result[X] = ' ')) then
          Inc(_current_length)
        else
        begin
          if not(_current_length = 0) and (_current_length < 4) then
            Delete(Result, X + 1, _current_length + 1);
          _current_length := 0;
        end;
      end;
    end;
    {
      if (length(result) > 35) then
      begin
      result := copy(result, 1, 35);
      result := copy(result, 1, LastDelimiter(' ', result) - 1);
      end;
      }
    Result := Trim(ReduceWhitespace(Result));
    if AMaskSearch then
      Result := '"' + Result + '"';
  end;

var
  ResponseStr: string;
  Params: TStringList;
  Enc: TEncoding;

  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if vBulletinSettings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject, vBulletinSettings.intelligent_posting_keepshortwords,
      vBulletinSettings.intelligent_posting_mask_search, vBulletinSettings.intelligent_posting_alternativesearch);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      PreSearchPage(AIdHTTPHelper, ResponseStr);

      with AIdHTTPHelper do
      begin
        Params := TStringList.Create;
        try
          with TRegExpr.Create do
            try
              for I := 0 to length(security_inputs) - 1 do
              begin
                InputString := ResponseStr;
                Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

                if Exec(InputString) then
                  Params.Add(security_inputs[I] + '=' + Match[1]);
              end;
            finally
              Free;
            end;

          with Params do
          begin
            Add('query=' + SearchValue);
            Add('searchuser=');
            Add('searchthreadid=');
            Add('do=process');
            Add('titleonly=1');
            Add('starteronly=0');
            Add('replyless=0');
            Add('replylimit=0');
            Add('searchdate=0');
            Add('beforeafter=after');
            Add('sortby=lastpost');
            Add('order=descending');
            // WriteString('quicksearch=1&');
            if vBulletinSettings.intelligent_posting_boundedsearch then
            begin
              for I := 0 to length(vBulletinSettings.intelligent_posting_bounds) - 1 do
                Add('forumchoice[]=' + IntToStr(vBulletinSettings.intelligent_posting_bounds[I]))
            end
            else
              Add('forumchoice[]=0');
            Add('childforums=0');
            Add('exactname=1');
            Add('showposts=0');
            Add('dosearch=');
            Add('s=' + FSessionID);
          end;

          Request.CharSet := vBulletinSettings.CharSet;
          Enc := CharsetToEncoding(Request.CharSet);
          try
            try
              ResponseStr := Post(Website + 'search.php?do=process', Params, Enc);
            except
              on E: Exception do
              begin
                ErrorMsg := E.message;
                Exit;
              end;
            end;
          finally
            Enc.Free;
          end;
        finally
          Params.Free;
        end;
      end;

      SearchResults := TStringList.Create;
      try
        SearchResults.Add('0=Create new Thread');
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := 'id="thread_title_(\d+)"(.*?)>(.*?)<\/a>';

            if Exec(InputString) then
            begin
              repeat
                _found_thread_id := Match[1];
                _found_thread_name := Match[3];

                SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);

                if not PostReply then
                  with TRegExpr.Create do
                    try
                      ModifierI := True;
                      InputString := _found_thread_name;
                      Expression := StringReplace(GetSearchTitle(Subject, vBulletinSettings.intelligent_posting_keepshortwords), ' ',
                        '.*?', [rfReplaceAll, rfIgnoreCase]);

                      if Exec(InputString) then
                      begin
                        PostReply := True;
                        vBulletinSettings.threads := _found_thread_id;
                        SearchIndex := SearchResults.Count - 1;
                      end;
                    finally
                      Free;
                    end;

              until not ExecNext;
            end;
          finally
            Free;
          end;

        if vBulletinSettings.intelligent_posting_helper then
        begin
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
          begin
            ErrorMsg := StrAbortedThrougthInt;
            Result := False;
          end;
          PostReply := (SearchIndex > 0);
          if PostReply then
            vBulletinSettings.threads := SearchResults.Names[SearchIndex];
        end;
      finally
        SearchResults.Free;
      end;
    until not RedoSearch;
  end;
end;

function TvBulletin.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'newreply.php?do=newreply&t=' + VarToStr(vBulletinSettings.threads) + '&s=' + FSessionID)
      else
        AResponse := Get(Website + 'newthread.php?do=newthread&f=' + VarToStr(vBulletinSettings.forums) + '&s=' + FSessionID);
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('posthash', AResponse) = 0) then
    begin
      if not(Pos('<!-- main error message -->', AResponse) = 0) then
        with TRegExpr.Create do
        begin
          try
            InputString := AResponse;
            Expression := '<div style="margin: 10px">(.*?)<\/div>';

            if Exec(InputString) then
            begin
              repeat
                Self.ErrorMsg := HTML2Text(Trim(Match[1]));
              until not ExecNext;
              Exit;
            end;
          finally
            Free;
          end;
        end
        else
          with TRegExpr.Create do
          begin
            try
              InputString := AResponse;
              Expression := '<ol>(.*?)<\/ol>';

              if Exec(InputString) then
              begin
                Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
                Exit;
              end
              else
              begin
                Expression := 'standard_error">(.*?)<\/div>';

                if Exec(InputString) then
                begin
                  Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
                  Exit;
                end;
              end;
            finally
              Free;
            end;
          end;
    end;
  end;
  Result := True;
end;

function TvBulletin.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController;
  AMirrorController: IMirrorController; APrevResponse: string = ''): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  _captcha, _cookies: WideString;
  _captcha_confirm_hash, _captcha_text, _storecaptcha, _challenge: string;

  I: Integer;

  postaddress: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
{$REGION 'CAPTCHA'}
      if not(Pos('name="humanverify[hash]"', APrevResponse) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := APrevResponse;
            Expression := 'name="humanverify\[hash\]" value="(.*?)"';

            if Exec(InputString) then
              _captcha_confirm_hash := Match[1];

            Params.Add('humanverify[hash]=' + _captcha_confirm_hash);
          finally
            Free;
          end;
      end;
      if not(Pos('name="humanverify[input]"', APrevResponse) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := APrevResponse;
            Expression := 'for="humanverify">(.*?)<\/label>';

            if Exec(InputString) then
              _captcha_text := Match[1];

            Expression := 'for="humanverify">.*?class="description">(.*?)<\/.*?humanverify';

            if Exec(InputString) then
              _captcha_text := Match[1];

            if (_captcha_text = '') then
            begin
              Expression := 'id="imagereg" src="(.*?)"';

              if Exec(InputString) then
                _captcha_text := Website + HTMLDecode(Match[1]);
            end;
          finally
            Free;
          end;

        if not CAPTCHAInput(_captcha_text, GetName + ' RandomQuestion', _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;

        Params.Add('humanverify[input]=' + _captcha);
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

        Params.Add('recaptcha_challenge_field=' + _challenge);
        Params.Add('recaptcha_response_field=' + _captcha);
      end
      else if not(Pos('name="nospamnumber"', APrevResponse) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := APrevResponse;
            Expression := 'name="nospamnumber" value="(.*?)"';

            if Exec(InputString) then
              Params.Add('nospamnumber=' + Match[1]);
          finally
            Free;
          end;

        with TRegExpr.Create do
          try
            InputString := APrevResponse;
            Expression := '<legend>NoSpam!.*?<\/legend>.*?<td>(.*?)<\/td>';

            if Exec(InputString) then
              _captcha_text := Match[1];
          finally
            Free;
          end;

        if not CAPTCHAInput(_captcha_text, GetName + ' NoSpam!', _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;

        Params.Add('nospam=' + _captcha);
      end;
{$ENDREGION}
      with TRegExpr.Create do
        try
          ModifierS := False;
          InputString := APrevResponse;
          Expression := 'action="([\w\.\?\/\-:;=&]+)" name="vbform" method="post"';
          // Expression := '<form .*?action="([\w\.\?;=&]+)" name="vbform" method="post"';

          if Exec(InputString) then
            postaddress := HTMLDecode(Match[1]);
        finally
          Free;
        end;

      if SameText(postaddress, '') then
        with TRegExpr.Create do
          try
            ModifierS := False;
            InputString := APrevResponse;
            Expression := 'action="([\w\.\?\/\-:;=&]+)" method="post" name="vbform"';
            // Expression := '<form .*?action="([\w\.\?;=&]+)" method="post" name="vbform"';

            if Exec(InputString) then
              postaddress := HTMLDecode(Match[1]);
          finally
            Free;
          end;

      if SameText(postaddress, '') then
        if PostReply then
          postaddress := 'newreply.php?do=postreply&t=' + VarToStr(vBulletinSettings.threads)
        else
          postaddress := 'newthread.php?do=postthread&f=' + VarToStr(vBulletinSettings.forums);

      if not SameText(copy(postaddress, 1, 4), 'http') then
        postaddress := Website + postaddress;

      with TRegExpr.Create do
        try
          for I := 0 to length(security_inputs) - 1 do
          begin
            InputString := APrevResponse;
            Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

            if Exec(InputString) then
              if (security_inputs[I] = 'securitytoken') then
              begin
                repeat
                  if not(Match[1] = 'guest') then
                    Params.Add(security_inputs[I] + '=' + Match[1]);
                until not ExecNext;
              end
              else
                Params.Add(security_inputs[I] + '=' + Match[1]);
          end;
        finally
          Free;
        end;

      Params.Add(vBulletinSettings.prefix_field + '=' + VarToStr(vBulletinSettings.prefix));
      Params.Add(vBulletinSettings.icon_field + '=' + VarToStr(vBulletinSettings.icon));

      if vBulletinSettings.use_coverlink and Assigned(AComponentController.FindControl(cPicture)) then
        Params.Add('threadjaq=' + AComponentController.FindControl(cPicture).Value);

      if PostReply then
      begin
        Params.Add('t=' + VarToStr(vBulletinSettings.threads));
        Params.Add('f=');
        Params.Add('specifiedpost=0');
      end
      else
        Params.Add('f=' + VarToStr(vBulletinSettings.forums));

      if not SameStr('', FSessionID) then
        Params.Add('s=' + FSessionID);

      if PostReply then
        Params.Add('title=' + Subject)
      else
        Params.Add('subject=' + Subject);
      Params.Add('message=' + message);

      Params.Add('taglist=' + Tags);

      with vBulletinSettings do
      begin
        if attach_sig then
          Params.Add('signature=1')
        else
          Params.Add('signature=0');
        if parseurl then
          Params.Add('parseurl=1')
        else
          Params.Add('parseurl=0');
        if disablesmilies then
          Params.Add('disablesmilies=1')
        else
          Params.Add('disablesmilies=0');
        if openclose then
          Params.Add('openclose=1')
        else
          Params.Add('openclose=0');
        if stickunstick then
          Params.Add('stickunstick=1')
        else
          Params.Add('stickunstick=0');
      end;

      Params.Add('sbutton=');

      if PostReply then
        Params.Add('do=postreply')
      else if (ArticleID = 0) then
        Params.Add('do=postthread');

      Request.CharSet := vBulletinSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(postaddress, Params, Enc);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;
      finally
        Enc.Free;
      end;

      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '(<ol>\s?<li>|<ul class="blockrow error">|standard_error">)(.*?)(<\/li>\s?<\/ol>|<\/ul>|<\/div>)';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[2])));
            Exit;
          end;
        finally
          Free;
        end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor TvBulletin.Create;
begin
  inherited Create;
  vBulletinSettings := TvBulletinSettings.Create;
end;

destructor TvBulletin.Destroy;
begin
  vBulletinSettings.Free;
  inherited Destroy;
end;

function TvBulletin.GetName;
begin
  Result := 'vBulletin';
end;

function TvBulletin.DefaultCharset: WideString;
begin
  Result := 'ISO-8859-1';
end;

function TvBulletin.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('login.php?do=login', string(AWebsiteSourceCode)) > 0) or (Pos('vb_login_username', string(AWebsiteSourceCode)) > 0);
end;

function TvBulletin.GetIDs: Integer;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;

  BoardLevel: TStringList;
  BoardLevelIndex: Integer;

  function IDPath(AStringList: TStringList): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to AStringList.Count - 1 do
    begin
      if not SameStr('', Result) then
        Result := Result + ' -> ';
      Result := Result + AStringList.Strings[I];
    end;
  end;

  function CleanPathName(AName: string): string;
  const
    spacebridge = '&nbsp; ';
  begin
    Result := AName;
    while copy(Result, 1, length(spacebridge)) = spacebridge do
      Delete(Result, 1, length(spacebridge));
    Result := Trim(HTML2Text(Result));
  end;

begin
  Result := 0;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    LoadSettings;

    if not(AccountName = '') then
      if not Login(IdHTTPHelper) then
        Exit;

    PreSearchPage(IdHTTPHelper, ResponseStr);
  finally
    IdHTTPHelper.Free;
  end;

  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ResponseStr;
        Expression := 'option value="(\d+)" class="\D+(\d+)" >(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := StrToIntDef(Match[2], 0);

            if (BoardLevelIndex = BoardLevel.Count) then
              BoardLevel.Add(CleanPathName(Match[3]))
            else
            begin
              repeat
                BoardLevel.Delete(BoardLevel.Count - 1);
              until (BoardLevelIndex = BoardLevel.Count);
              BoardLevel.Add(CleanPathName(Match[3]));
            end;

            AddID(Match[1], IDPath(BoardLevel));
          until not ExecNext;
        end;
      finally
        Free;
      end;
  finally
    BoardLevel.Free;
  end;
  Result := FCheckedIDsList.Count;
end;

function TvBulletin.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TvBulletinSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
