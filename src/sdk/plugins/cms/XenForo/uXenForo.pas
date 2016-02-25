unit uXenForo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, HTTPApp,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses, uPlugInCMSSettingsHelper;

type
  TXenForoSettings = class(TCMSBoardIPPlugInSettings)
  strict private
    fprefix_field, fredirect: string;
    fintelligent_posting_boundedsearch: Boolean;
    fprefix: Variant;
  public
    intelligent_posting_bounds: TIntegerArray;
    constructor Create; override;
  published
    property prefix_field: string read fprefix_field write fprefix_field;

    property redirect: string read fredirect write fredirect;

    property intelligent_posting;
    property intelligent_posting_helper;
    property intelligent_posting_boundedsearch: Boolean read fintelligent_posting_boundedsearch write fintelligent_posting_boundedsearch;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
  end;

  TXenForo = class(TCMSBoardIPPlugIn)
  private
    XenForoSettings: TXenForoSettings;
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

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID, AArticlePathID: Integer): WideString; override; safecall;
  end;

implementation

{ TXenForoSettings }

constructor TXenForoSettings.Create;
begin
  inherited Create;

  // default setup
  prefix_field := 'prefix_id';
  redirect := '/';
  intelligent_posting_boundedsearch := False;
end;

{ TXenForo }

function TXenForo.SettingsClass;
begin
  Result := TXenForoSettings;
end;

function TXenForo.GetSettings;
begin
  Result := XenForoSettings;
end;

procedure TXenForo.SetSettings;
begin
  XenForoSettings := ACMSPlugInSettings as TXenForoSettings;
end;

function TXenForo.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with XenForoSettings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TXenForo.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?login/login');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := XenForoSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('login', AccountName);
    AddFormField('register', '0');
    AddFormField('password', AccountPassword);
    AddFormField('remember', '1');
    AddFormField('cookie_check', '0');
    AddFormField('redirect', XenForoSettings.redirect);
    AddFormField('_xfToken', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TXenForo.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('logout/?_xfToken=', AResponseStr) = 0) or not(Pos('index.php?logout/&amp;_xfToken=', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'class="errors">(.*?)<';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        else
        begin
          Expression := 'class="OverlayCloser">(.*?)<';

          if Exec(InputString) then
            Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        end;
      finally
        Free;
      end;
end;

procedure TXenForo.DoHandleSessionID;
begin
  with TRegExpr.Create do
    try
      InputString := AHTTPProcess.HTTPResult.SourceCode;
      Expression := '_xfToken=(.*?)"';

      if Exec(InputString) then
        FSessionID := HTTPDecode(Match[1]);
    finally
      Free;
    end;
end;

function TXenForo.IntelligentPosting(var ARequestID: Double): Boolean;

  function GetSearchTitle(ATitle: string): string;
  var
    X: Integer;
  begin
    Result := ATitle;
    for X := length(Result) downto 1 do
      if (Result[X] in ['+', '-', '_', '.', ':', '(', ')', '[', ']', '/', '\']) then
        Result[X] := ' ';

    Result := Trim(Result);
  end;

const
  REQUEST_LIMIT = 5;
var
  HTTPOptions: IHTTPOptions;
  ResponseStr: string;
  HTTPParams: IHTTPParams;

  I: Integer;

  HTTPProcess: IHTTPProcess;
  HasSearchResult: Boolean;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: WordBool;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if XenForoSettings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('keywords', SearchValue);
        AddFormField('title_only', '1');
        AddFormField('users', '');
        AddFormField('date', '');
        AddFormField('reply_count', '0');
        AddFormField('prefixes[]', '0');
        if XenForoSettings.intelligent_posting_boundedsearch then
        begin
          for I := 0 to length(XenForoSettings.intelligent_posting_bounds) - 1 do
            AddFormField('nodes[]', IntToStr(XenForoSettings.intelligent_posting_bounds[I]))
        end;
        AddFormField('child_nodes', '1');
        AddFormField('order', 'date');
        AddFormField('type', 'post');
        AddFormField('_xfToken', FSessionID);
        AddFormField('_xfRequestUri', '/search/?type=post');
        AddFormField('_xfNoRedirect', '1');
        AddFormField('_xfToken', FSessionID);
        // AddFormField('_xfResponseType', 'json');
      end;

      HTTPOptions := TPlugInHTTPOptions.Create(Self);
      with HTTPOptions do
        RedirectMaximum := 1;

      HasSearchResult := False;
      I := 0;
      repeat
        Inc(I);

        ARequestID := HTTPManager.Post(Website + 'search/search', ARequestID, HTTPParams, HTTPOptions);

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
        try
          SearchResults.Add('0=Create new Thread');
          with TRegExpr.Create do
            try
              InputString := ResponseStr;
              Expression := 'thread\-(\d+)"(.*?)<h3(.*?)"><(.*?)>(.*?)<\/a>';

              if Exec(InputString) then
              begin
                repeat
                  _found_thread_id := Match[1];
                  _found_thread_name := HTML2Text(Match[5]);

                  SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);

                  if not PostReply then
                    with TRegExpr.Create do
                      try
                        ModifierI := True;
                        InputString := _found_thread_name;
                        Expression := StringReplace(GetSearchTitle(Subject), ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

                        if Exec(InputString) then
                        begin
                          PostReply := True;
                          XenForoSettings.threads := _found_thread_id;
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

          if XenForoSettings.intelligent_posting_helper then
          begin
            if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
            begin
              ErrorMsg := StrAbortedThrougthInt;
              Result := False;
            end;
            PostReply := (SearchIndex > 0);
            if PostReply then
              XenForoSettings.threads := SearchResults.Names[SearchIndex];
          end;
        finally
          SearchResults.Free;
        end;
      end;

    until not RedoSearch;
  end;
end;

function TXenForo.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'threads/' + VarToStr(XenForoSettings.threads) + '/' // add-reply URI not allowed here
  else
    ARequestURL := Website + 'forums/' + VarToStr(XenForoSettings.forums) + '/create-thread';
end;

function TXenForo.DoAnalyzePrePost;
begin
  Result := not(Pos('name="message', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'class="errors">(.*?)<';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        else
        begin
          Expression := 'class="OverlayCloser">(.*?)<';

          if Exec(InputString) then
            Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        end;
      finally
        Free;
      end;
end;

function TXenForo.DoBuildPostRequest;
const
  security_inputs: array [0 .. 2] of string = ('attachment_hash', 'last_date', '_xfToken');
var
  RequestURL: string;

  I: Integer;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'threads/' + VarToStr(XenForoSettings.threads) + '/add-reply'
  else
    RequestURL := Website + 'forums/' + VarToStr(XenForoSettings.forums) + '/add-thread';

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := XenForoSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
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

    AddFormField(XenForoSettings.prefix_field, VarToStr(XenForoSettings.prefix));

    if not PostReply then
      AddFormField('title', Subject);

    AddFormField('message', Message);

    AddFormField('watch_thread', '0');
    AddFormField('watch_thread_email', '0');
    AddFormField('watch_thread_state', '0');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TXenForo.DoAnalyzePost;
var
  LArticleID, LCurrentTime, LMaxTime: Integer;
begin
  LArticleID := -1;
  LMaxTime := -1;

  Result := not(Pos('class="privateControls"', AResponseStr) = 0);

  if Result then
  begin
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '"privateControls".*?data\-time="(\d+)".*?posts\/(\d+)\/permalink';

        if Exec(InputString) then
        begin
          repeat
            LCurrentTime := StrToInt(Match[1]);

            if (LCurrentTime >= LMaxTime) then
            begin
              LArticleID := StrToInt(Match[2]);
            end;
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

  if not Result then
  begin
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'class="errors">(.*?)<';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        else
        begin
          Expression := 'class="OverlayCloser">(.*?)<';

          if Exec(InputString) then
            Self.ErrorMsg := Trim(HTML2Text(Match[1]))
        end;
      finally
        Free;
      end;
  end;
end;

function TXenForo.GetIDsRequestURL;
begin
  Result := Website + 'search/?type=post';
end;

function TXenForo.DoAnalyzeIDsRequest;
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
        InputString := ExtractTextBetween(AResponseStr, 'name="nodes[]"', '</select>');
        Expression := 'option.*? value="(\d+)">([&nbsp; ]*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('&nbsp;', Match[2]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 2;

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

function TXenForo.GetName;
begin
  Result := 'XenForo';
end;

function TXenForo.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TXenForo.BelongsTo;
begin
  Result := (Pos('login/login', string(AWebsiteSourceCode)) > 0) and (Pos('_xfToken', string(AWebsiteSourceCode)) > 0);
end;

function TXenForo.GetArticleLink;
begin
  Result := Format('%sindex.php?threads/.19%d/', [AURL, AArticleID]);
end;

end.
