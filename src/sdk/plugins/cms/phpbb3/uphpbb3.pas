unit uphpbb3;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses, uPlugInCMSSettingsHelper;

type
  Tphpbb3Settings = class(TCMSBoardIPPlugInSettings)
  strict private
    fprefix_field: string;

    fsleeptime: Integer;

    fdisable_bbcode, fdisable_smilies, fdisable_magic_url, fattach_sig, fnotify, flock_topic, fintelligent_posting_boundedsearch: Boolean;
  public
    intelligent_posting_bounds: TIntegerArray;
    constructor Create; override;
  published
    property prefix_field: string read fprefix_field write fprefix_field;
    property sleeptime: Integer read fsleeptime write fsleeptime;
    property disable_bbcode: Boolean read fdisable_bbcode write fdisable_bbcode;
    property disable_smilies: Boolean read fdisable_smilies write fdisable_smilies;
    property disable_magic_url: Boolean read fdisable_magic_url write fdisable_magic_url;
    property attach_sig: Boolean read fattach_sig write fattach_sig;
    property notify: Boolean read fnotify write fnotify;
    property lock_topic: Boolean read flock_topic write flock_topic;

    property intelligent_posting;
    property intelligent_posting_helper;
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix;
    property icon;
  end;

  Tphpbb3 = class(TCMSBoardIPPlugIn)
  private
    phpbb3Settings: Tphpbb3Settings;
    FSessionID: string;
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

    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ Tphpbb3Settings }

constructor Tphpbb3Settings.Create;
begin
  inherited Create;

  // default setup
  prefix_field := 'attr_id';
  sleeptime := 3;
  disable_bbcode := False;
  disable_smilies := False;
  disable_magic_url := False;
  attach_sig := True;
  notify := False;
  lock_topic := False;
  intelligent_posting_boundedsearch := False;
end;

{ Tphpbb3 }

function Tphpbb3.SettingsClass;
begin
  Result := Tphpbb3Settings;
end;

function Tphpbb3.GetSettings;
begin
  Result := phpbb3Settings;
end;

procedure Tphpbb3.SetSettings;
begin
  phpbb3Settings := ACMSPlugInSettings as Tphpbb3Settings;
end;

function Tphpbb3.LoadSettings;
begin
  Result := True;
  phpbb3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, phpbb3Settings, AData);
  with phpbb3Settings do
  begin
    if SameStr('', Charset) then
      Charset := DefaultCharset;

    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tphpbb3.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'ucp.php?mode=login');
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := phpbb3Settings.Charset;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('autologin', 'on');
    AddFormField('viewonline', 'on');
    AddFormField('sid', '');
    AddFormField('redirect', '');
    AddFormField('login', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tphpbb3.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('/ucp.php?mode=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'class="error">(.*?)<\/(span|div)';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := HTML2Text(Match[1], False);
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

procedure Tphpbb3.DoHandleSessionID;
var
  I: Integer;
begin
  with AHTTPProcess.HTTPResult.HTTPResponse.Cookies do
    for I := 0 to Count - 1 do
      if MatchTextMask('*_sid', Names[I] { Cookies[I].CookieName } , False) then
      begin
        FSessionID := copy(ValueFromIndex[I], 1, Pos(';', ValueFromIndex[I]) - 1) { Cookies[I].Value } ;
        break;
      end;

  if SameStr('', FSessionID) then
    with TRegExpr.Create do
      try
        InputString := AHTTPProcess.HTTPResult.SourceCode;
        Expression := 'sid=(.*?)"';

        if Exec(InputString) then
          FSessionID := Match[1];
      finally
        Free;
      end;
end;

function Tphpbb3.IntelligentPosting;

  function GetSearchTitle(ATitle: string): string;

    function GetWordCount(s: string): Integer;
    var
      I: Integer;
    begin
      Result := 1;
      for I := 1 to length(s) - 1 do
        if (s[I] = ' ') then
          Inc(Result);
    end;

    procedure RemoveLastWord(var s: string);
    var
      I: Integer;
    begin
      for I := length(s) - 1 downto 0 do
      begin
        Delete(s, I + 1, 1);
        if (s[I] = ' ') then
        begin
          Delete(s, I, 1);
          break;
        end;
      end;
    end;

  var
    X: Integer;
  begin
    Result := ATitle;
    for X := length(Result) downto 1 do
      if (Result[X] in ['+', '-', '_', '.', ':', '(', ')', '[', ']', '/', '\']) then
        Result[X] := ' ';

    Result := Trim(ReduceWhitespace(Result));
    // max 10 words
    while GetWordCount(Result) > 10 do
      RemoveLastWord(Result);
    // max 60 chars
    if length(Result) > 60 then
      Delete(Result, 61, length(Result) - 60);
  end;

const
  REQUEST_LIMIT = 5;
var
  ResponseStr: string;

  I: Integer;

  HTTPProcess: IHTTPProcess;
  HasSearchResult: Boolean;

  SearchValue: WideString;
  SearchResults, SearchResultsForumID: TStringList;
  SearchIndex: Integer;
  RedoSearch: WordBool;

  _found_forum_id, _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if phpbb3Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);
    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HasSearchResult := False;
      I := 0;
      repeat
        Inc(I);

        ARequestID := HTTPManager.Get(GetSearchRequestURL + '?keywords=' + SearchValue + '&terms=all&author=&sc=1&sf=all&sk=t&sd=d&sr=topics&st=0&ch=300&t=0&submit=', ARequestID, TPlugInHTTPOptions.Create(Self));

        repeat
          sleep(50);
        until HTTPManager.HasResult(ARequestID);

        HTTPProcess := HTTPManager.GetResult(ARequestID);

        if HTTPProcess.HTTPResult.HasError then
        begin
          ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
          Result := False;
        end
        else
        begin
          HasSearchResult := True;
          Result := True;
        end;

      until HasSearchResult or (I > REQUEST_LIMIT);

      if HasSearchResult then
      begin
        ResponseStr := HTTPProcess.HTTPResult.SourceCode;

        SearchResults := TStringList.Create;
        SearchResultsForumID := TStringList.Create;
        try
          SearchResults.Add('0=Create new Thread');
          with TRegExpr.Create do
            try
              ModifierS := False;
              InputString := ResponseStr;
              Expression := 'viewtopic\.php\?f=(\d+)&amp;t=(\d+).*?class="topictitle".*?>(.*?)<\/a>';

              if Exec(InputString) then
              begin
                repeat
                  _found_forum_id := Match[1];
                  _found_thread_id := Match[2];
                  _found_thread_name := Match[3];

                  if phpbb3Settings.intelligent_posting_boundedsearch and not InArray(_found_forum_id, phpbb3Settings.intelligent_posting_bounds) then
                    Continue;

                  SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);
                  SearchResultsForumID.Add(_found_forum_id);

                  if not PostReply then
                    with TRegExpr.Create do
                      try
                        ModifierI := True;
                        InputString := _found_thread_name;
                        Expression := StringReplace(' ' + GetSearchTitle(Subject) + ' ', ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

                        if Exec(InputString) then
                        begin
                          PostReply := True;
                          phpbb3Settings.forums := _found_forum_id;
                          phpbb3Settings.threads := _found_thread_id;
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

          if phpbb3Settings.intelligent_posting_helper then
          begin
            if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
            begin
              ErrorMsg := StrAbortedThrougthInt;
              Result := False;
            end;
            PostReply := (SearchIndex > 0);
            if PostReply then
            begin
              phpbb3Settings.forums := SearchResultsForumID.Strings[SearchIndex];
              phpbb3Settings.threads := SearchResults.Names[SearchIndex];
            end;
          end;
        finally
          SearchResultsForumID.Free;
          SearchResults.Free;
        end;
      end;

    until not RedoSearch;
  end;
end;

function Tphpbb3.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'posting.php?mode=reply&f=' + VarToStr(phpbb3Settings.forums) + '&t=' + VarToStr(phpbb3Settings.threads) + '&sid=' + FSessionID
  else
  begin
    if (ArticleID = 0) then
      // Neues Thema erstellen
      ARequestURL := Website + 'posting.php?mode=post&f=' + VarToStr(phpbb3Settings.forums) + '&sid=' + FSessionID
    else
      // Ein bestehenden Beitrag bearbeiten
      ARequestURL := Website + 'posting.php?mode=edit&f=' + VarToStr(phpbb3Settings.forums) + '&p=' + VarToStr(ArticleID) + '&sid=' + FSessionID;
  end;
end;

function Tphpbb3.DoAnalyzePrePost;
begin
  Result := not(Pos('form_token', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '(error">|class="gen">|<\/h2>\s+<p>|class="row3" colspan="2" align="center">)(.*?)<\/';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[2]));
      finally
        Free;
      end;
end;

function Tphpbb3.DoBuildPostRequest;
const
  security_inputs: array [0 .. 2] of string = ('lastclick', 'creation_time', 'form_token');
var
  RequestURL: string;

  I: Integer;

  topic_cur_post_id: string;
begin
  Result := True;

  sleep(phpbb3Settings.sleeptime * 1000);

  if PostReply then
    // posting.php?mode=reply&f=417&sid=5bfa3ef7fd36e912ea85960c35126d50&t=43478
    RequestURL := Website + 'posting.php?mode=reply&f=' + VarToStr(phpbb3Settings.forums) + '&t=' + VarToStr(phpbb3Settings.threads) + '&sid=' + FSessionID
  else
  begin
    if (ArticleID = 0) then
      // Neues Thema erstellen
      RequestURL := Website + 'posting.php?mode=post&f=' + VarToStr(phpbb3Settings.forums) + '&sid=' + FSessionID
    else
      // Ein bestehenden Beitrag bearbeiten
      RequestURL := Website + 'posting.php?mode=edit&f=' + VarToStr(phpbb3Settings.forums) + '&p=' + IntToStr(ArticleID) + '&sid=' + FSessionID;
  end;

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := phpbb3Settings.Charset;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
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

    if PostReply then
    begin
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="topic_cur_post_id" value="(.*?)"';

          if Exec(InputString) then
            topic_cur_post_id := Match[1];
        finally
          Free;
        end;

      AddFormField('topic_cur_post_id', topic_cur_post_id);
    end;

    AddFormField('icon', VarToStr(phpbb3Settings.icon));

    AddFormField('subject', Subject);

    AddFormField(phpbb3Settings.prefix_field, VarToStr(phpbb3Settings.prefix));

    AddFormField('message', Message);

    with phpbb3Settings do
    begin
      if disable_bbcode then
        AddFormField('disable_bbcode', 'on');
      if disable_smilies then
        AddFormField('disable_smilies', 'on');
      if disable_magic_url then
        AddFormField('disable_magic_url', 'on');
      if attach_sig then
        AddFormField('attach_sig', 'on');
      if notify then
        AddFormField('notify', 'on');
      if lock_topic then
        AddFormField('lock_topic', 'on');
    end;

    AddFormField('post', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tphpbb3.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := 'error">(.*?)<\/';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(Match[1]);
        Result := False;
      end;

      if Result then
      begin
        Expression := '>(.*?)<br \/><br \/><a href="\.\/viewtopic\.php\?(.*?)p=(.*?)">';

        if Exec(InputString) then
          ArticleID := StrToIntDef(copy(Match[3], 1, Pos('#', Match[3]) - 1), 0);
      end;

    finally
      Free;
    end;
end;

function Tphpbb3.DoAnalyzeIDsRequest;
var
  First: Boolean;

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
  begin
    Result := Trim(HTML2Text(AName));
  end;

begin
  First := True;

  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(AResponseStr, 'name="fid[]"', '</select>');
        Expression := 'option.*? value="(\d+)">(&nbsp; &nbsp;)*(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('&nbsp;', Match[0]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 2;

            // Required if no category is defined
            if First and not(BoardLevelIndex = 0) then
              BoardLevel.Add('Index');
            First := False;

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

function Tphpbb3.GetName;
begin
  Result := 'phpbb3';
end;

function Tphpbb3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tphpbb3.BelongsTo;
begin
  Result := (Pos('ucp.php?mode=login', string(AWebsiteSourceCode)) > 0);
end;

function Tphpbb3.GetArticleLink;
begin
  Result := Format('%sviewtopic.php?p=%d#p%1:d', [AURL, AArticleID]);
end;

end.
