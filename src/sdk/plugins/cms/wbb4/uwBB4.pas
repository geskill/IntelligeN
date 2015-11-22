unit uwBB4;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
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
  TwBB4Settings = class(TCMSBoardIPPlugInSettings)
  strict private
    ficon_field: string;

    fpreParse, fenableSmilies, fenableBBCodes, fenableHtml, fshowSignature, fhasThank, fcloseThread, fdisableThread, fintelligent_posting, fintelligent_posting_helper, fintelligent_posting_boundedsearch: Boolean;

    fprefix, ficon: Variant;
  public
    intelligent_posting_bounds: TIntegerArray;
  published
    [AttrDefaultValue('threadIconID')]
    property icon_field: string read ficon_field write ficon_field;
    [AttrDefaultValue(True)]
    property preParse: Boolean read fpreParse write fpreParse;
    [AttrDefaultValue(True)]
    property enableSmilies: Boolean read fenableSmilies write fenableSmilies;
    [AttrDefaultValue(True)]
    property enableBBCodes: Boolean read fenableBBCodes write fenableBBCodes;
    [AttrDefaultValue(False)]
    property enableHtml: Boolean read fenableHtml write fenableHtml;
    [AttrDefaultValue(True)]
    property showSignature: Boolean read fshowSignature write fshowSignature;
    [AttrDefaultValue(True)]
    property hasThank: Boolean read fhasThank write fhasThank;
    [AttrDefaultValue(False)]
    property closeThread: Boolean read fcloseThread write fcloseThread;
    [AttrDefaultValue(False)]
    property disableThread: Boolean read fdisableThread write fdisableThread;
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

  { TODO : use blog/forum/formbased class }
  TwBB4 = class(TCMSBoardIPPlugIn)
  private
    wBB4Settings: TwBB4Settings;
    FSessionID, FSecurityToken: string;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;
    procedure DoHandleSessionID(AHTTPProcess: IHTTPProcess); override;

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

{ TwBB4 }

function TwBB4.SettingsClass;
begin
  Result := TwBB4Settings;
end;

function TwBB4.GetSettings;
begin
  Result := wBB4Settings;
end;

procedure TwBB4.SetSettings;
begin
  wBB4Settings := ACMSPlugInSettings as TwBB4Settings;
end;

function TwBB4.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with wBB4Settings do
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

function TwBB4.NeedPreLogin;
begin
  Result := True;
  ARequestURL := Website + 'index.php?login';
end;

function TwBB4.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?login/&s=' + FSessionID);
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := wBB4Settings.Charset;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('username', AccountName);
    AddFormField('action', 'login');
    AddFormField('password', AccountPassword);
    AddFormField('useCookies', '1');
    AddFormField('url', '');
    AddFormField('t', FSecurityToken);
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TwBB4.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;

  Result := not(Pos('class="success"', AResponseStr) = 0) or not(Pos('logout/', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<p class="error">(.*?)<\/p>';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := HTML2Text(Match[1]);
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

procedure TwBB4.DoHandleSessionID;
begin
  with TRegExpr.Create do
    try
      InputString := AHTTPProcess.HTTPResult.SourceCode;

      Expression := '''&s=(\w+)''';
      if Exec(InputString) then
        FSessionID := Match[1];

      Expression := 'SECURITY_TOKEN = ''(\w+)''';
      if Exec(InputString) then
        FSecurityToken := Match[1];
    finally
      Free;
    end;
end;

function TwBB4.IntelligentPosting;

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

var
  HTTPParams: IHTTPParams;
  ResponseStr: string;

  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: WordBool;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if wBB4Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('q', SearchValue);
        AddFormField('subjectOnly', '1');
        AddFormField('findThreads', '1');
        if wBB4Settings.intelligent_posting_boundedsearch then
        begin
          for I := 0 to length(wBB4Settings.intelligent_posting_bounds) - 1 do
            AddFormField('boardIDs[]', IntToStr(wBB4Settings.intelligent_posting_bounds[I]))
        end
        else
          AddFormField('boardIDs[]', '*');

        AddFormField('t', FSecurityToken);
      end;

      ARequestID := HTTPManager.Post(GetSearchRequestURL, ARequestID, HTTPParams, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(ARequestID);

      ResponseStr := HTTPManager.GetResult(ARequestID).HTTPResult.SourceCode;

      SearchResults := TStringList.Create;
      try
        SearchResults.Add('0=Create new Thread');
        with TRegExpr.Create do
          try
            ModifierS := False;
            InputString := ResponseStr;
            Expression := 'data-thread-id="(\d+)">(.*?)<';

            if Exec(InputString) then
            begin
              repeat
                _found_thread_id := Match[1];
                _found_thread_name := HTML2Text(Match[2]);

                SearchResults.Add(_found_thread_id + SearchResults.NameValueSeparator + _found_thread_name);

                if not PostReply then
                  with TRegExpr.Create do
                    try
                      ModifierI := True;
                      InputString := _found_thread_name;
                      Expression := StringReplace(' ' + GetSearchTitle(Subject) + ' ', ' ', '.*?', [rfReplaceAll, rfIgnoreCase]);

                      if Exec(InputString) then
                      begin
                        PostReply := True;
                        wBB4Settings.threads := _found_thread_id;
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

        if wBB4Settings.intelligent_posting_helper then
        begin
          if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
          begin
            ErrorMsg := StrAbortedThrougthInt;
            Result := False;
          end;
          PostReply := (SearchIndex > 0);
          if PostReply then
            wBB4Settings.threads := SearchResults.Names[SearchIndex];
        end;
      finally
        SearchResults.Free;
      end;
    until not RedoSearch;
  end;
end;

function TwBB4.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'index.php?post-add/' + VarToStr(wBB4Settings.threads) + '/'
  else
    ARequestURL := Website + 'index.php?thread-add/' + VarToStr(wBB4Settings.forums) + '/'
end;

function TwBB4.DoAnalyzePrePost;
begin
  Result := not(Pos('name="tmpHash"', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<p class="error">(.*?)<\/p>';

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

function TwBB4.DoBuildPostRequest;
const
  security_inputs: array [0 .. 1] of string = ('tmpHash', 't');
var
  RequestURL: string;
  I: Integer;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'index.php?post-add/' + VarToStr(wBB4Settings.threads) + '/&s=' + FSessionID
  else
    RequestURL := Website + 'index.php?thread-add/' + VarToStr(wBB4Settings.forums) + '/&s=' + FSessionID;

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := wBB4Settings.Charset;
  end;

  AHTTPParams := THTTPParams.Create();
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

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

    AddFormField('prefix', VarToStr(wBB4Settings.prefix));

    AddFormField('threadIconActive', '1');
    AddFormField(wBB4Settings.icon_field, VarToStr(wBB4Settings.icon));

    AddFormField('subject', Subject);
    AddFormField('text', Message);

    AddFormField('tags', Tags);

    if PostReply then
      AddFormField('postID', '0');

    with wBB4Settings do
    begin
      if preParse then
        AddFormField('preParse', '1');
      if enableSmilies then
        AddFormField('enableSmilies', '1');
      if enableBBCodes then
        AddFormField('enableBBCodes', '1');
      if enableHtml then
        AddFormField('enableHtml', '1');
      if showSignature then
        AddFormField('showSignature', '1');
      if hasThank then
        AddFormField('hasThank', '1');
      if closeThread then
        AddFormField('closeThread', '1');
      if disableThread then
        AddFormField('disableThread', '1');
    end;

    AddFormField('send', 'Absenden');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
    RedirectMaximum := 1;
end;

function TwBB4.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := '<p class="error">(.*?)<\/p>';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(Match[1]);
        Result := False;
      end;
    finally
      Free;
    end;
end;

function TwBB4.GetIDsRequestURL;
begin
  Result := Website + 'index.php?search';
end;

function TwBB4.DoAnalyzeIDsRequest;
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
        InputString := ExtractTextBetween(AResponseStr, 'name="boardIDs[]"', '</select>');
        Expression := 'option.*? value="(\d+)">([&nbsp;]*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('&nbsp;', Match[2]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 4;

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

function TwBB4.GetName;
begin
  Result := 'wBB4';
end;

function TwBB4.DefaultCharset;
begin
  Result := 'UTF-8';
end;
{$REGION 'Documentation'}
/// <param name="AWebsiteSourceCode">
/// contains the sourcode from any website of the webpage
/// </param>
/// <returns>
/// The method returns True if the sourcode of a website
/// matches to this CMS and False otherwise.
/// </returns>
{$ENDREGION}

function TwBB4.BelongsTo;
begin
  Result := (Pos('com.woltlab.wbb.post', string(AWebsiteSourceCode)) > 0);
end;

end.
