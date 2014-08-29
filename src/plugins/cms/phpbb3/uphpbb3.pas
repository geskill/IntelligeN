unit uphpbb3;

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
  Tphpbb3Settings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field: string;

    fsleeptime: Integer;

    fdisable_bbcode, fdisable_smilies, fdisable_magic_url, fattach_sig, fnotify, flock_topic, fintelligent_posting,
      fintelligent_posting_helper, fintelligent_posting_boundedsearch: Boolean;

    fprefix, ficon: Variant;
  public
    intelligent_posting_bounds: TIntegerArray;

  published
    [AttrDefaultValue('attr_id')]
    property prefix_field: string read fprefix_field write fprefix_field;
    [AttrDefaultValue(3)]
    property sleeptime: Integer read fsleeptime write fsleeptime;
    [AttrDefaultValue(False)]
    property disable_bbcode: Boolean read fdisable_bbcode write fdisable_bbcode;
    [AttrDefaultValue(False)]
    property disable_smilies: Boolean read fdisable_smilies write fdisable_smilies;
    [AttrDefaultValue(False)]
    property disable_magic_url: Boolean read fdisable_magic_url write fdisable_magic_url;
    [AttrDefaultValue(True)]
    property attach_sig: Boolean read fattach_sig write fattach_sig;
    [AttrDefaultValue(False)]
    property notify: Boolean read fnotify write fnotify;
    [AttrDefaultValue(False)]
    property lock_topic: Boolean read flock_topic write flock_topic;

    [AttrDefaultValue(False)]
    property intelligent_posting: Boolean read fintelligent_posting write fintelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper: Boolean read fintelligent_posting_helper write fintelligent_posting_helper;
    [AttrDefaultValue(False)]
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  Tphpbb3 = class(TCMSBoardIPPlugIn)
  private
    phpbb3Settings: Tphpbb3Settings;
    FSessionID: string;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
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

function Tphpbb3.LoadSettings;
begin
  Result := True;
  phpbb3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, phpbb3Settings,
    AComponentController);
  with phpbb3Settings do
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

function Tphpbb3.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        AddFormField('username', AccountName, phpbb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('password', AccountPassword, phpbb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('autologin', 'on', phpbb3Settings.Charset);
        AddFormField('viewonline', 'on', phpbb3Settings.Charset);
        AddFormField('sid', '', phpbb3Settings.Charset);
        AddFormField('redirect', '', phpbb3Settings.Charset);
        AddFormField('login', '', phpbb3Settings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'ucp.php?mode=login', Params);
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

    if (Pos('/ucp.php?mode=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<span class="error">(.*?)<\/span>';

          if Exec(InputString) then
          begin
            repeat
              Self.ErrorMsg := HTML2Text(Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

      Exit;
    end;

    with CookieManager.CookieCollection do
      for I := 0 to Count - 1 do
        if MatchText('*_sid', Cookies[I].CookieName, False) then
        begin
          FSessionID := Cookies[I].Value;
          break;
        end;

    if SameStr('', FSessionID) then
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := 'sid=(.*?)"';

          if Exec(InputString) then
            FSessionID := Match[1];
        finally
          Free;
        end;
  end;

  Result := True;
end;

function Tphpbb3.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

  function InArray(AInteger: Integer; AArray: TIntegerArray): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := low(AArray) to high(AArray) do
      if (AArray[I] = AInteger) then
        Exit(True);
  end;

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

var
  ResponseStr: string;

  SearchValue: WideString;
  SearchResults, SearchResultsForumID: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_forum_id, _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if phpbb3Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);
    RedoSearch := False;
    repeat
      SearchIndex := 0;

      with AIdHTTPHelper do
      begin
        try
          ResponseStr := Get(Website + 'search.php?keywords=' + SearchValue +
              '&terms=all&author=&sc=1&sf=all&sk=t&sd=d&sr=topics&st=0&ch=300&t=0&submit=');
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;
      end;

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

                if phpbb3Settings.intelligent_posting_boundedsearch and not InArray(_found_forum_id,
                  phpbb3Settings.intelligent_posting_bounds) then
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
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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
    until not RedoSearch;
  end;
end;

function Tphpbb3.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'posting.php?mode=reply&f=' + VarToStr(phpbb3Settings.forums) + '&t=' + VarToStr(phpbb3Settings.threads)
            + '&sid=' + FSessionID)
      else
      begin
        if (ArticleID = 0) then
          // Neues Thema erstellen
          AResponse := Get(Website + 'posting.php?mode=post&f=' + VarToStr(phpbb3Settings.forums) + '&sid=' + FSessionID)
        else
          // Ein bestehenden Beitrag bearbeiten
          AResponse := Get(Website + 'posting.php?mode=edit&f=' + VarToStr(phpbb3Settings.forums) + '&p=' + VarToStr(ArticleID)
              + '&sid=' + FSessionID);
      end;
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('form_token', AResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AResponse;
          Expression := '(error">|class="gen">|<\/h2>\s+<p>|class="row3" colspan="2" align="center">)(.*?)<\/';

          if Exec(InputString) then
            Self.ErrorMsg := Trim(HTML2Text(Match[2]));
        finally
          Free;
        end;
      Exit;
    end;
  end;
  Result := True;
end;

function Tphpbb3.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 2] of string = ('lastclick', 'creation_time', 'form_token');
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I: Integer;

  topic_cur_post_id: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with TRegExpr.Create do
        try
          for I := 0 to length(security_inputs) - 1 do
          begin
            InputString := APrevResponse;
            Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

            if Exec(InputString) then
              Params.AddFormField(security_inputs[I], Match[1], phpbb3Settings.Charset).ContentTransfer := 'binary';
          end;
        finally
          Free;
        end;

      if PostReply then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := APrevResponse;
            Expression := 'name="topic_cur_post_id" value="(.*?)"';

            if Exec(InputString) then
              topic_cur_post_id := Match[1];
          finally
            Free;
          end;
        end;

        Params.AddFormField('topic_cur_post_id', topic_cur_post_id, phpbb3Settings.Charset).ContentTransfer := 'binary';
      end;

      Params.AddFormField('icon', VarToStr(phpbb3Settings.icon), phpbb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('subject', Subject, phpbb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField(phpbb3Settings.prefix_field, VarToStr(phpbb3Settings.prefix), phpbb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('message', Message, phpbb3Settings.Charset).ContentTransfer := 'binary';

      with phpbb3Settings do
      begin
        if disable_bbcode then
          Params.AddFormField('disable_bbcode', 'on', phpbb3Settings.Charset);
        if disable_smilies then
          Params.AddFormField('disable_smilies', 'on', phpbb3Settings.Charset);
        if disable_magic_url then
          Params.AddFormField('disable_magic_url', 'on', phpbb3Settings.Charset);
        if attach_sig then
          Params.AddFormField('attach_sig', 'on', phpbb3Settings.Charset);
        if notify then
          Params.AddFormField('notify', 'on', phpbb3Settings.Charset);
        if lock_topic then
          Params.AddFormField('lock_topic', 'on', phpbb3Settings.Charset);
      end;

      Params.AddFormField('post', '', phpbb3Settings.Charset);

      Sleep(phpbb3Settings.sleeptime * 1000);

      if PostReply then
        // posting.php?mode=reply&f=417&sid=5bfa3ef7fd36e912ea85960c35126d50&t=43478
        ResponseStr := Post(Website + 'posting.php?mode=reply&f=' + VarToStr(phpbb3Settings.forums) + '&t=' + VarToStr
            (phpbb3Settings.threads) + '&sid=' + FSessionID, Params)
      else
      begin
        if (ArticleID = 0) then
          // Neues Thema erstellen
          ResponseStr := Post(Website + 'posting.php?mode=post&f=' + VarToStr(phpbb3Settings.forums) + '&sid=' + FSessionID, Params)
        else
          // Ein bestehenden Beitrag bearbeiten
          ResponseStr := Post(Website + 'posting.php?mode=edit&f=' + VarToStr(phpbb3Settings.forums) + '&p=' + IntToStr(ArticleID)
              + '&sid=' + FSessionID, Params);
      end;

      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
          Expression := 'error">(.*?)<\/';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
            Exit;
          end;
        finally
          Free;
        end;
      end;

      Result := True;

      with TRegExpr.Create do
      begin
        InputString := ResponseStr;
        Expression := '>(.*?)<br \/><br \/><a href="\.\/viewtopic\.php\?(.*?)p=(.*?)">';

        if Exec(InputString) then
        begin
          Result := True;
          ArticleID := StrToIntDef(copy(Match[3], 1, Pos('#', Match[3]) - 1), 0)
        end;
      end;
    finally
      Params.Free;
    end;
  end;
end;

constructor Tphpbb3.Create;
begin
  inherited Create;
  phpbb3Settings := Tphpbb3Settings.Create;
end;

destructor Tphpbb3.Destroy;
begin
  phpbb3Settings.Free;
  inherited Destroy;
end;

function Tphpbb3.GetName;
begin
  Result := 'phpbb3';
end;

function Tphpbb3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tphpbb3.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('ucp.php?mode=login', string(AWebsiteSourceCode)) > 0);
end;

function Tphpbb3.GetIDs: Integer;
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

  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(ResponseStr, 'name="fid[]"', '</select>');
        Expression := 'option.*? value="(\d+)">([&nbsp; ]*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('&nbsp;', Match[2]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 2;

            BoardLevelIndex := BoardLevelIndex - 1;

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

function Tphpbb3.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, Tphpbb3Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
