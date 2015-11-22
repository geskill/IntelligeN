unit uipb3;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
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
  Tipb3Settings = class(TCMSBoardIPPlugInSettings)
  strict private
    fprefix_field: string;

    fenableemo, fenablesig, fintelligent_posting_boundedsearch: Boolean;
  public
    intelligent_posting_bounds: TIntegerArray;
  published
    [AttrDefaultValue('topic_prefix')]
    property prefix_field: string read fprefix_field write fprefix_field;

    [AttrDefaultValue(False)]
    property enableemo: Boolean read fenableemo write fenableemo;
    [AttrDefaultValue(True)]
    property enablesig: Boolean read fenablesig write fenablesig;
    [AttrDefaultValue(False)]
    property intelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper;
    [AttrDefaultValue(False)]
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix;
    property icon;
  end;

  Tipb3 = class(TCMSBoardIPPlugIn)
  private
    ipb3Settings: Tipb3Settings;

  const
    error_reg_ex: string = 'class=''message error''>(.*?)<\/';
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function GetSearchRequestURL: string; override;
    function IntelligentPosting(var ARequestID: Double): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override; safecall;
  end;

implementation

function Tipb3.SettingsClass;
begin
  Result := Tipb3Settings;
end;

function Tipb3.GetSettings;
begin
  Result := ipb3Settings;
end;

procedure Tipb3.SetSettings;
begin
  ipb3Settings := ACMSPlugInSettings as Tipb3Settings;
end;

function Tipb3.LoadSettings;
begin
  Result := True;
  ipb3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, ipb3Settings, AData);
  with ipb3Settings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tipb3.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?app=core&module=global&section=login&do=process');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := ipb3Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('auth_key', '880ea6a14ea49e853634fbdc5015a024');
    AddFormField('username', AccountName);
    AddFormField('ips_username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('ips_password', AccountPassword);
    AddFormField('referer', Website);
    AddFormField('rememberMe', '1');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tipb3.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('''redirector''', AResponseStr) = 0) and (Pos('do=logout', AResponseStr) = 0) and (Pos('id=''user_navigation'' class=''logged_in''', AResponseStr) = 0) and (Pos('http-equiv="refresh" content="2', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := error_reg_ex;

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function Tipb3.GetSearchRequestURL;
begin
  Result := GetIDsRequestURL + '&section=search&fromsearch=1';
end;

function Tipb3.IntelligentPosting;

  function GetSearchTitle(ATitle: string): string;
  var
    X, _current_length: Integer;
  begin
    Result := ATitle;
    for X := length(Result) downto 1 do
      if (Result[X] in ['+', '-', '_', '.', ':', '(', ')', '[', ']', '/', '\']) then
        Result[X] := ' ';
    _current_length := 0;
    Result := ' ' + Result;
    for X := length(Result) downto 1 do
    begin
      if (not(Result[X] = ' ')) then
        Inc(_current_length)
      else
      begin
        // alle Wörter mit weniger als 3 Buchstaben entfernen
        if not(_current_length = 0) and (_current_length < 3) then
          Delete(Result, X + 1, _current_length + 1);
        _current_length := 0;
      end;
    end;
    Result := Trim(ReduceWhitespace(Result));
  end;

var
  HTTPParams: IHTTPParams;
  HTTPProcess: IHTTPProcess;
  ResponseStr: string;

  I: Integer;

  SearchValue: WideString;
  SearchResults, SearchResultsForumID: TStringList;
  SearchIndex: Integer;
  RedoSearch: WordBool;

  _found_forum_id, _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if ipb3Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);
    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HTTPParams := THTTPParams.Create(ptMultipartFormData);
      with HTTPParams do
      begin
        AddFormField('search_filter_app[all]', '1');
        AddFormField('search_app', 'forums');
        AddFormField('search_term', SearchValue);
        AddFormField('andor_type', 'and');
        AddFormField('content_title_only', '1');
        AddFormField('search_author', '');
        AddFormField('search_date_start', '');
        AddFormField('search_date_end', '');
        AddFormField('search_app', 'forums');
        AddFormField('search_app_filters[core][sortKey]', 'date');
        AddFormField('search_app_filters[core][sortDir]', '0');
        AddFormField('search_app_filters[forums][forums][]', '');
        AddFormField('search_app_filters[forums][contentOnly]', '0');
        AddFormField('search_app_filters[forums][noPreview]', '1');
        AddFormField('search_app_filters[forums][pCount]', '');
        AddFormField('search_app_filters[forums][pViews]', '');
        AddFormField('search_app_filters[forums][sortKey]', 'date');
        AddFormField('search_app_filters[forums][sortDir]', '0');

        if ipb3Settings.intelligent_posting_boundedsearch then
        begin
          for I := 0 to length(ipb3Settings.intelligent_posting_bounds) - 1 do
            AddFormField('search_app_filters[forums][forums][]', IntToStr(ipb3Settings.intelligent_posting_bounds[I]), ipb3Settings.CharSet);
        end;

        AddFormField('submit', '');
      end;

      ARequestID := HTTPManager.Post(GetSearchRequestURL, ARequestID, HTTPParams, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      HTTPProcess := HTTPManager.GetResult(ARequestID);

      if HTTPProcess.HTTPResult.HasError then
      begin
        ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
        Exit;
      end;

      ResponseStr := HTTPProcess.HTTPResult.SourceCode;

      SearchResults := TStringList.Create;
      SearchResultsForumID := TStringList.Create;
      try
        SearchResults.Add('0=Create new Thread');
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := 'id=''tidPop_(\d+)''(.|\s)*?result''>(.*?)<\/a>(.|\s)*?showforum=(\d+)''>';

            if Exec(InputString) then
            begin
              repeat
                _found_forum_id := Match[5];
                _found_thread_id := Match[1];
                _found_thread_name := Match[3];

                SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);
                SearchResultsForumID.Add(_found_forum_id);

                if not PostReply then
                  with TRegExpr.Create do
                  begin
                    try
                      ModifierI := True;
                      InputString := _found_thread_name;
                      Expression := StringReplace(' ' + GetSearchTitle(Subject) + ' ', ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

                      if Exec(InputString) then
                      begin
                        PostReply := True;
                        ipb3Settings.forums := _found_forum_id;
                        ipb3Settings.threads := _found_thread_id;
                        SearchIndex := SearchResults.Count - 1;
                      end;
                    finally
                      Free;
                    end;
                  end;

              until not ExecNext;
            end
            else
            begin
              Expression := 'data-tid="(\d+)".*?result''>(.*?)<\/a>.*?forum\/(\d+)-';

              if Exec(InputString) then
              begin
                repeat
                  _found_forum_id := Match[3];
                  _found_thread_id := Match[1];
                  _found_thread_name := Match[2];

                  SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);
                  SearchResultsForumID.Add(_found_forum_id);

                  if not PostReply then
                    with TRegExpr.Create do
                    begin
                      try
                        ModifierI := True;
                        InputString := _found_thread_name;
                        Expression := StringReplace(' ' + GetSearchTitle(Subject) + ' ', ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

                        if Exec(InputString) then
                        begin
                          PostReply := True;
                          ipb3Settings.forums := _found_forum_id;
                          ipb3Settings.threads := _found_thread_id;
                          SearchIndex := SearchResults.Count - 1;
                        end;
                      finally
                        Free;
                      end;
                    end;

                until not ExecNext;
              end;
            end;
          finally
            Free;
          end;

        if ipb3Settings.intelligent_posting_helper then
        begin
          if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
          begin
            ErrorMsg := StrAbortedThrougthInt;
            Result := False;
          end;
          PostReply := (SearchIndex > 0);
          if PostReply then
          begin
            ipb3Settings.forums := SearchResultsForumID.Strings[SearchIndex];
            ipb3Settings.threads := SearchResults.Names[SearchIndex];
          end;
        end;
      finally
        SearchResultsForumID.Free;
        SearchResults.Free;
      end;
    until not RedoSearch;
  end;
end;

function Tipb3.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'index.php?app=forums&module=post&section=post&do=reply_post&f=' + VarToStr(ipb3Settings.forums) + '&t=' + VarToStr(ipb3Settings.threads)
  else
    ARequestURL := Website + 'index.php?app=forums&module=post&section=post&do=new_post&f=' + VarToStr(ipb3Settings.forums);
end;

function Tipb3.DoAnalyzePrePost;
begin
  Result := not(Pos('attach_post_key', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := error_reg_ex;

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function Tipb3.DoBuildPostRequest;
const
  security_inputs: array [0 .. 2] of string = ('s', 'attach_post_key', 'auth_key');
var
  I: Integer;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := ipb3Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name=''' + security_inputs[I] + ''' value=''(.*?)''';

          if Exec(InputString) then
          begin
            repeat
              AddFormField(security_inputs[I], Match[1]);
            until not ExecNext;
          end;
        end;
      finally
        Free;
      end;

    AddFormField('f', VarToStr(ipb3Settings.forums));

    if PostReply then
      AddFormField('t', VarToStr(ipb3Settings.threads));

    AddFormField('ipsTags_prefix', '1');

    AddFormField(ipb3Settings.prefix_field, VarToStr(ipb3Settings.prefix));

    AddFormField('iconid', VarToStr(ipb3Settings.icon));

    AddFormField('TopicTitle', Subject);
    // AddFormField('TopicDesc', '');

    AddFormField('Post', Message);

    AddFormField('ipsTags', Tags);

    AddFormField('st', '0');
    AddFormField('app', 'forums');
    AddFormField('module', 'post');
    AddFormField('section', 'post');

    if PostReply then
      AddFormField('do', 'reply_post_do')
    else
      AddFormField('do', 'new_post_do');

    AddFormField('dosubmit', '');

    if ipb3Settings.enableemo then
      AddFormField('enableemo', 'yes');
    if ipb3Settings.enablesig then
      AddFormField('enablesig', 'yes');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tipb3.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := RemoveTextBetween(AResponseStr, '<noscript>', '</noscript>');
      Expression := error_reg_ex;

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
        Result := False;
      end;
    finally
      Free;
    end;
end;

function Tipb3.GetIDsRequestURL;
begin
  Result := Website + 'index.php?app=core&module=search&do=search';
end;

function Tipb3.DoAnalyzeIDsRequest;
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
  begin
    Result := Trim(HTML2Text(AName));
  end;

begin
  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(AResponseStr, 'name=''search_app_filters[forums][forums][]''', '</select>');
        Expression := 'option.*? value="(\d+)">([&nbsp;#0124]*)(-*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('-', Match[3]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 2;

            if (BoardLevelIndex = BoardLevel.Count) then
              BoardLevel.Add(CleanPathName(Match[4]))
            else
            begin
              repeat
                BoardLevel.Delete(BoardLevel.Count - 1);
              until (BoardLevelIndex = BoardLevel.Count);
              BoardLevel.Add(CleanPathName(Match[4]));
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

function Tipb3.GetName;
begin
  Result := 'ipb3';
end;

function Tipb3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tipb3.BelongsTo;
begin
  Result := (Pos('app=core&amp;module=global&amp;section=login', string(AWebsiteSourceCode)) > 0) or (Pos('ipb.vars[''board_url'']', string(AWebsiteSourceCode)) > 0);
end;

end.
