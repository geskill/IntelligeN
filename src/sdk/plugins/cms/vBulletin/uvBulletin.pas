unit uvBulletin;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp, Variants,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils,
  uHTMLUtils, uStringUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses, uPlugInCMSSettingsHelper;

type
  TvBulletinSettings = class(TCMSBoardIPPlugInSettings)
  strict private
    fprefix_field, ficon_field: string;
    fuse_coverlink, fattach_sig, fparseurl, fdisablesmilies, fopenclose, fstickunstick, fintelligent_posting_mask_search, fintelligent_posting_keepshortwords, fintelligent_posting_alternativesearch, fintelligent_posting_boundedsearch: Boolean;
  public
    intelligent_posting_bounds: TIntegerArray;
    constructor Create; override;
  published
    property prefix_field: string read fprefix_field write fprefix_field;
    property icon_field: string read ficon_field write ficon_field;
    property use_coverlink: Boolean read fuse_coverlink write fuse_coverlink;
    property attach_sig: Boolean read fattach_sig write fattach_sig;
    property parseurl: Boolean read fparseurl write fparseurl;
    property disablesmilies: Boolean read fdisablesmilies write fdisablesmilies;
    property openclose: Boolean read fopenclose write fopenclose;
    property stickunstick: Boolean read fstickunstick write fstickunstick;

    property intelligent_posting;
    property intelligent_posting_helper;
    property intelligent_posting_mask_search: Boolean read fintelligent_posting_mask_search write fintelligent_posting_mask_search;
    property intelligent_posting_keepshortwords: Boolean read fintelligent_posting_keepshortwords write fintelligent_posting_keepshortwords;
    property intelligent_posting_alternativesearch: Boolean read fintelligent_posting_alternativesearch write fintelligent_posting_alternativesearch;
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix;
    property icon;
  end;

  TvBulletin = class(TCMSBoardIPPlugIn)
  private
    vBulletinSettings: TvBulletinSettings;
    FSessionID: string;

  const
    security_inputs: array [0 .. 3] of string = ('securitytoken', 'posthash', 'poststarttime', 'loggedinuser');
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;
    procedure DoHandleSessionID(AHTTPProcess: IHTTPProcess); override;

    function IntelligentPosting(var ARequestID: Double): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(const AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
  end;

implementation

{ TvBulletinSettings }

constructor TvBulletinSettings.Create;
begin
  inherited Create;

  // default setup
  prefix_field := 'prefixid';
  icon_field := 'iconid';
  use_coverlink := False;
  attach_sig := True;
  parseurl := True;
  disablesmilies := False;
  openclose := False;
  stickunstick := False;

  intelligent_posting_mask_search := False;
  intelligent_posting_keepshortwords := False;
  intelligent_posting_alternativesearch := False;
  intelligent_posting_boundedsearch := False;
end;

{ TvBulletin }

function TvBulletin.SettingsClass;
begin
  Result := TvBulletinSettings;
end;

function TvBulletin.GetSettings;
begin
  Result := vBulletinSettings;
end;

procedure TvBulletin.SetSettings;
begin
  vBulletinSettings := ACMSPlugInSettings as TvBulletinSettings;
end;

function TvBulletin.LoadSettings;
begin
  Result := True;
  vBulletinSettings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, vBulletinSettings, AData);
  with vBulletinSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TvBulletin.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'login.php?do=login');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := vBulletinSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('vb_login_username', AccountName);
    AddFormField('vb_login_password', AccountPassword);
    AddFormField('securitytoken', 'guest');
    AddFormField('do', 'login');
    AddFormField('vb_login_md5password', '');
    AddFormField('vb_login_md5password_utf', '');
    AddFormField('cookieuser', '1');
    AddFormField('s', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
  begin
    RedirectMaximum := 3; // need 3 for http://forums.shooshtime.com/
  end;
end;

function TvBulletin.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('<blockquote>', AResponseStr) = 0) and (Pos('http-equiv="Refresh" content="2', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<div style="margin: 10px">(.*?)<\/div>';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := Trim(HTML2Text(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

procedure TvBulletin.DoHandleSessionID;
begin
  with TRegExpr.Create do
    try
      InputString := AHTTPProcess.HTTPResult.SourceCode;
      Expression := 's=(\w+)(&|")';

      if Exec(InputString) then
        FSessionID := Match[1];
    finally
      Free;
    end;

  /// if >1 problems with search
  AHTTPProcess.HTTPData.HTTPOptions.RedirectMaximum := 1;
end;

function TvBulletin.IntelligentPosting;

  function GetSearchTitle(ATitle: string; AKeepShortWords: Boolean = False; AMaskSearch: Boolean = False; AAlternativeSearch: Boolean = False): string;
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
  HTTPOptions: IHTTPOptions;
  ResponseStr: string;
  HTTPParams: IHTTPParams;
  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: WordBool;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if vBulletinSettings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject, vBulletinSettings.intelligent_posting_keepshortwords, vBulletinSettings.intelligent_posting_mask_search, vBulletinSettings.intelligent_posting_alternativesearch);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HTTPOptions := TPlugInHTTPOptions.Create(Self);
      with HTTPOptions do
        RedirectMaximum := 1;

      ARequestID := HTTPManager.Get(GetSearchRequestURL, ARequestID, HTTPOptions);

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      ResponseStr := HTTPManager.GetResult(ARequestID).HTTPResult.SourceCode;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        with TRegExpr.Create do
          try
            for I := 0 to length(security_inputs) - 1 do
            begin
              InputString := ResponseStr;
              Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

              if Exec(InputString) then
                AddFormField(security_inputs[I], Match[1]);
            end;
          finally
            Free;
          end;

        AddFormField('query', SearchValue);
        AddFormField('searchuser', '');
        AddFormField('searchthreadid', '');
        AddFormField('do', 'process');
        AddFormField('titleonly', '1');
        AddFormField('starteronly', '0');
        AddFormField('replyless', '0');
        AddFormField('replylimit', '0');
        AddFormField('searchdate', '0');
        AddFormField('beforeafter', 'after');
        AddFormField('sortby', 'lastpost');
        AddFormField('order', 'descending');
        // WriteString('quicksearch=1&');
        if vBulletinSettings.intelligent_posting_boundedsearch then
        begin
          for I := 0 to length(vBulletinSettings.intelligent_posting_bounds) - 1 do
            AddFormField('forumchoice[]', IntToStr(vBulletinSettings.intelligent_posting_bounds[I]))
        end
        else
          AddFormField('forumchoice[]', '0');
        AddFormField('childforums', '0');
        AddFormField('exactname', '1');
        AddFormField('showposts', '0');
        AddFormField('dosearch', '');
        AddFormField('s', FSessionID);
      end;

      ARequestID := HTTPManager.Post(Website + 'search.php?do=process', ARequestID, HTTPParams, HTTPOptions);

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      ResponseStr := HTTPManager.GetResult(ARequestID).HTTPResult.SourceCode;

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
                      Expression := StringReplace(GetSearchTitle(Subject, True), ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

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
          if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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

function TvBulletin.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'newreply.php?do=newreply&t=' + VarToStr(vBulletinSettings.threads) + '&s=' + FSessionID
  else
    ARequestURL := Website + 'newthread.php?do=newthread&f=' + VarToStr(vBulletinSettings.forums) + '&s=' + FSessionID;
end;

function TvBulletin.DoAnalyzePrePost;
begin
  Result := not(Pos('posthash', AResponseStr) = 0);
  if not Result then
    if not(Pos('<!-- main error message -->', AResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := AResponseStr;
          Expression := '<div style="margin: 10px">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              Self.ErrorMsg := Trim(HTML2Text(Match[1]));
            until not ExecNext;
            Exit;
          end;
        finally
          Free;
        end;
      end
    end
    else
      with TRegExpr.Create do
      begin
        try
          InputString := AResponseStr;
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

function TvBulletin.DoBuildPostRequest;
var
  _captcha, _cookies: WideString;
  _captcha_confirm_hash, _captcha_text, _storecaptcha, _challenge: string;

  I: Integer;

  RequestID: Double;

  postaddress: string;
begin
  Result := True;

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

  AHTTPRequest := THTTPRequest.Create(postaddress);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := vBulletinSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
{$REGION 'CAPTCHA'}
    if not(Pos('name="humanverify[hash]"', APrevResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="humanverify\[hash\]" value="(.*?)"';

          if Exec(InputString) then
            _captcha_confirm_hash := Match[1];

          AddFormField('humanverify[hash]', _captcha_confirm_hash);
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

      if not CAPTCHAInput(Website, Subject, _captcha_text, GetName + ' RandomQuestion', _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;

      AddFormField('humanverify[input]', _captcha);
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
    else if not(Pos('name="nospamnumber"', APrevResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="nospamnumber" value="(.*?)"';

          if Exec(InputString) then
            AddFormField('nospamnumber', Match[1]);
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

      if not CAPTCHAInput(Website, Subject, _captcha_text, GetName + ' NoSpam!', _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;

      AddFormField('nospam', _captcha);
    end;
{$ENDREGION}
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
                  AddFormField(security_inputs[I], Match[1]);
              until not ExecNext;
            end
            else
              AddFormField(security_inputs[I], Match[1]);
        end;
      finally
        Free;
      end;

    AddFormField(vBulletinSettings.prefix_field, VarToStr(vBulletinSettings.prefix));
    AddFormField(vBulletinSettings.icon_field, VarToStr(vBulletinSettings.icon));

    if vBulletinSettings.use_coverlink and Assigned(AData.FindControl(cPicture)) then
      AddFormField('threadjaq', AData.FindControl(cPicture).Value);

    if PostReply then
    begin
      AddFormField('t', VarToStr(vBulletinSettings.threads));
      AddFormField('f', '');
      AddFormField('specifiedpost', '0');
    end
    else
      AddFormField('f', VarToStr(vBulletinSettings.forums));

    if not SameStr('', FSessionID) then
      AddFormField('s', FSessionID);

    if PostReply then
      AddFormField('title', Subject)
    else
      AddFormField('subject', Subject);

    AddFormField('message', message);

    AddFormField('taglist', Tags);

    with vBulletinSettings do
    begin
      if attach_sig then
        AddFormField('signature', '1')
      else
        AddFormField('signature', '0');
      if parseurl then
        AddFormField('parseurl', '1')
      else
        AddFormField('parseurl', '0');
      if disablesmilies then
        AddFormField('disablesmilies', '1')
      else
        AddFormField('disablesmilies', '0');
      if openclose then
        AddFormField('openclose', '1')
      else
        AddFormField('openclose', '0');
      if stickunstick then
        AddFormField('stickunstick', '1')
      else
        AddFormField('stickunstick', '0');
    end;

    AddFormField('sbutton', '');

    if PostReply then
      AddFormField('do', 'postreply')
    else if (ArticleID = 0) then
      AddFormField('do', 'postthread');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TvBulletin.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := '(<ol>\s?<li>|<ul class="blockrow error">|standard_error">)(.*?)(<\/li>\s?<\/ol>|<\/ul>|<\/div>)';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[2])));
        Result := False;
      end;
    finally
      Free;
    end;
end;

function TvBulletin.GetIDsRequestURL;
begin
  Result := Website + 'search.php?search_type=1&s=' + FSessionID;
end;

function TvBulletin.DoAnalyzeIDsRequest;
var
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
  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
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

function TvBulletin.GetName;
begin
  Result := 'vBulletin';
end;

function TvBulletin.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TvBulletin.BelongsTo;
begin
  Result := (Pos('login.php?do=login', string(AWebsiteSourceCode)) > 0) or (Pos('vb_login_username', string(AWebsiteSourceCode)) > 0);
end;

end.
