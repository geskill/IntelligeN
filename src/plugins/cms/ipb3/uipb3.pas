unit uipb3;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
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
  Tipb3Settings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field: string;

    fenableemo, fenablesig, fintelligent_posting, fintelligent_posting_helper, fintelligent_posting_boundedsearch: Boolean;

    fprefix, ficon: Variant;
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

  Tipb3 = class(TCMSBoardIPPlugIn)
  private
    ipb3Settings: Tipb3Settings;

  const
    error_reg_ex: string = 'class=''message error''>(.*?)<\/';
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean; override;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string = ''): Boolean; override;
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

function Tipb3.LoadSettings;
begin
  Result := True;
  ipb3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, ipb3Settings, AComponentController);
  with ipb3Settings do
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

function Tipb3.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        AddFormField('auth_key', '880ea6a14ea49e853634fbdc5015a024', ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('username', AccountName, ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('ips_username', AccountName, ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('password', AccountPassword, ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('ips_password', AccountPassword, ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('referer', Website, ipb3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('rememberMe', '1', ipb3Settings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'index.php?app=core&module=global&section=login&do=process', Params);
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

    if (Pos('''redirector''', ResponseStr) = 0) and (Pos('do=logout', ResponseStr) = 0) and (Pos('id=''user_navigation'' class=''logged_in''', ResponseStr) = 0) and (Pos('http-equiv="refresh" content="2', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := error_reg_ex;

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

function Tipb3.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

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
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  SearchValue: WideString;
  SearchResults, SearchResultsForumID: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  I: Integer;

  _found_forum_id, _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if ipb3Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);
    RedoSearch := False;
    repeat
      SearchIndex := 0;

      with AIdHTTPHelper do
      begin
        Params := TIdMultiPartFormDataStream.Create;
        try
          with Params do
          begin
            AddFormField('search_filter_app[all]', '1', ipb3Settings.Charset);
            AddFormField('search_app', 'forums', ipb3Settings.Charset);
            AddFormField('search_term', SearchValue, ipb3Settings.Charset).ContentTransfer := 'binary';
            AddFormField('andor_type', 'and', ipb3Settings.Charset);
            AddFormField('content_title_only', '1', ipb3Settings.Charset);
            AddFormField('search_author', '', ipb3Settings.Charset);
            AddFormField('search_date_start', '', ipb3Settings.Charset);
            AddFormField('search_date_end', '', ipb3Settings.Charset);
            AddFormField('search_app', 'forums', ipb3Settings.Charset);
            AddFormField('search_app_filters[core][sortKey]', 'date', ipb3Settings.Charset);
            AddFormField('search_app_filters[core][sortDir]', '0', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][forums][]', '', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][contentOnly]', '0', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][noPreview]', '1', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][pCount]', '', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][pViews]', '', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][sortKey]', 'date', ipb3Settings.Charset);
            AddFormField('search_app_filters[forums][sortDir]', '0', ipb3Settings.Charset);

            if ipb3Settings.intelligent_posting_boundedsearch then
            begin
              for I := 0 to length(ipb3Settings.intelligent_posting_bounds) - 1 do
                AddFormField('search_app_filters[forums][forums][]', IntToStr(ipb3Settings.intelligent_posting_bounds[I]), ipb3Settings.Charset);
            end;

            //
            AddFormField('submit', '');
          end;

          try
            ResponseStr := Post(Website + 'index.php?app=core&module=search&section=search&do=search&fromsearch=1', Params);
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
      end;

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
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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

function Tipb3.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'index.php?app=forums&module=post&section=post&do=reply_post&f=' + VarToStr(ipb3Settings.forums) + '&t=' + VarToStr(ipb3Settings.threads))
      else
        AResponse := Get(Website + 'index.php?app=forums&module=post&section=post&do=new_post&f=' + VarToStr(ipb3Settings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('attach_post_key', AResponse) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := AResponse;
          Expression := error_reg_ex;

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
        finally
          Free;
        end;
      end;

      Exit;
    end;
  end;
  Result := True;
end;

function Tipb3.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 2] of string = ('s', 'attach_post_key', 'auth_key');
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
      with TRegExpr.Create do
        try
          for I := 0 to length(security_inputs) - 1 do
          begin
            InputString := APrevResponse;
            Expression := 'name=''' + security_inputs[I] + ''' value=''(.*?)''';

            if Exec(InputString) then
            begin
              repeat
                Params.AddFormField(security_inputs[I], Match[1], ipb3Settings.Charset).ContentTransfer := 'binary';
              until not ExecNext;
            end;
          end;
        finally
          Free;
        end;

      Params.AddFormField('f', VarToStr(ipb3Settings.forums), ipb3Settings.Charset).ContentTransfer := 'binary';

      if PostReply then
        Params.AddFormField('t', VarToStr(ipb3Settings.threads), ipb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField(ipb3Settings.prefix_field, VarToStr(ipb3Settings.prefix), ipb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('iconid', VarToStr(ipb3Settings.icon), ipb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('TopicTitle', Subject, ipb3Settings.Charset).ContentTransfer := 'binary';
      // Params.AddFormField('TopicDesc', '');

      Params.AddFormField('Post', Message, ipb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('ipsTags', Tags, ipb3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('st', '0', ipb3Settings.Charset);
      Params.AddFormField('app', 'forums', ipb3Settings.Charset);
      Params.AddFormField('module', 'post', ipb3Settings.Charset);
      Params.AddFormField('section', 'post', ipb3Settings.Charset);
      if PostReply then
        Params.AddFormField('do', 'reply_post_do', ipb3Settings.Charset)
      else
        Params.AddFormField('do', 'new_post_do', ipb3Settings.Charset);
      Params.AddFormField('dosubmit', '', ipb3Settings.Charset);

      if ipb3Settings.enableemo then
        Params.AddFormField('enableemo', 'yes', ipb3Settings.Charset);
      if ipb3Settings.enablesig then
        Params.AddFormField('enablesig', 'yes', ipb3Settings.Charset);

      try
        ResponseStr := Post(Website + 'index.php', Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := RemoveTextBetween(ResponseStr, '<noscript>', '</noscript>');
          Expression := error_reg_ex;

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
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

constructor Tipb3.Create;
begin
  inherited Create;
  ipb3Settings := Tipb3Settings.Create;
end;

destructor Tipb3.Destroy;
begin
  ipb3Settings.Free;
  inherited Destroy;
end;

function Tipb3.GetName;
begin
  Result := 'ipb3';
end;

function Tipb3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function Tipb3.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('app=core&amp;module=global&amp;section=login', string(AWebsiteSourceCode)) > 0) or (Pos('ipb.vars[''board_url'']', string(AWebsiteSourceCode)) > 0);
end;

function Tipb3.GetIDs: Integer;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'index.php?app=core&module=search&do=search');
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
        InputString := ExtractTextBetween(ResponseStr, 'name=''search_app_filters[forums][forums][]''', '</select>');
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

function Tipb3.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, Tipb3Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
