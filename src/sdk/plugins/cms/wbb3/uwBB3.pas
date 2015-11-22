unit uwBB3;

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
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses, uPlugInCMSSettingsHelper;

type
  TwBB3Settings = class(TCMSBoardIPPlugInSettings)
  strict private
    ficon_field: string;

    fparseURL, fenableSmilies, fenableBBCodes, fshowSignature, fhasThank, fcloseThread, fintelligent_posting_boundedsearch: Boolean;
  public
    intelligent_posting_bounds: TIntegerArray;
  published
    [AttrDefaultValue('threadIconID')]
    property icon_field: string read ficon_field write ficon_field;
    [AttrDefaultValue(True)]
    property parseURL: Boolean read fparseURL write fparseURL;
    [AttrDefaultValue(True)]
    property enableSmilies: Boolean read fenableSmilies write fenableSmilies;
    [AttrDefaultValue(True)]
    property enableBBCodes: Boolean read fenableBBCodes write fenableBBCodes;
    [AttrDefaultValue(True)]
    property showSignature: Boolean read fshowSignature write fshowSignature;
    [AttrDefaultValue(True)]
    property hasThank: Boolean read fhasThank write fhasThank;
    [AttrDefaultValue(False)]
    property closeThread: Boolean read fcloseThread write fcloseThread;

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

  TwBB3 = class(TCMSBoardIPPlugIn)
  private
    wBB3Settings: TwBB3Settings;
    FSessionID: string;

  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

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

function TwBB3.SettingsClass;
begin
  Result := TwBB3Settings;
end;

function TwBB3.GetSettings;
begin
  Result := wBB3Settings;
end;

procedure TwBB3.SetSettings;
begin
  wBB3Settings := ACMSPlugInSettings as TwBB3Settings;
end;

function TwBB3.LoadSettings;
begin
  Result := True;
  wBB3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, wBB3Settings, AData);
  with wBB3Settings do
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

function TwBB3.DoBuildLoginRequest;
var
  _captchaID: string;
  _captcha, _cookies: WideString;
begin
  Result := True;

  if ACAPTCHALogin then
  begin
    with TRegExpr.Create do
      try
        InputString := APrevResponse;
        Expression := 'captchaID=(\d+)';

        if Exec(InputString) then
          _captchaID := Match[1];
      finally
        Free;
      end;

    if not CAPTCHAInput(Website, Subject, Website + 'index.php?page=Captcha&captchaID=' + _captchaID, GetName, _captcha, _cookies) then
    begin
      ErrorMsg := StrAbortedThrougthCAP;
      Result := False;
    end;
  end;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?form=UserLogin');
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := wBB3Settings.Charset;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('loginUsername', AccountName);
    AddFormField('loginPassword', AccountPassword);
    AddFormField('useCookies', '1');

    if ACAPTCHALogin then
    begin
      AddFormField('captchaString', _captcha);
      AddFormField('captchaID', _captchaID);
    end;

    AddFormField('url', '');
    AddFormField('s', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TwBB3.DoAnalyzeLogin;
begin
  ACAPTCHALogin := (Pos('name="captchaString"', AResponseStr) > 0);
  Result := not(Pos('class="success"', AResponseStr) = 0);
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

procedure TwBB3.DoHandleSessionID;
begin
  with TRegExpr.Create do
    try
      InputString := AHTTPProcess.HTTPResult.SourceCode;
      Expression := 's=(\w+)"';

      if Exec(InputString) then
        FSessionID := Match[1];
    finally
      Free;
    end;
end;

function TwBB3.IntelligentPosting;

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
  if wBB3Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('q', SearchValue);
        AddFormField('s', FSessionID);
        AddFormField('findThreads', '1');
        if wBB3Settings.intelligent_posting_boundedsearch then
        begin
          for I := 0 to length(wBB3Settings.intelligent_posting_bounds) - 1 do
            AddFormField('boardIDs[]', IntToStr(wBB3Settings.intelligent_posting_bounds[I]))
        end
        else
          AddFormField('boardIDs[]', '*');
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
            // (\/\d+-.*?\/(\d+)-.*?|page=Thread&amp;threadID=(\d+)&amp;)highlight.*?">(.*?)<\/a>
            Expression := '(page=Thread&amp;threadID=|\/)(\d+).*?highlight.*?">(.*?)<\/a>';

            if Exec(InputString) then
            begin
              repeat
                _found_thread_id := Match[2];
                _found_thread_name := HTML2Text(Match[3]);

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
                        wBB3Settings.threads := _found_thread_id;
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

        if wBB3Settings.intelligent_posting_helper then
        begin
          if not IntelligentPostingHelper(Website, Subject, SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
          begin
            ErrorMsg := StrAbortedThrougthInt;
            Result := False;
          end;
          PostReply := (SearchIndex > 0);
          if PostReply then
            wBB3Settings.threads := SearchResults.Names[SearchIndex];
        end;
      finally
        SearchResults.Free;
      end;
    until not RedoSearch;
  end;
end;

function TwBB3.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'index.php?form=PostAdd&threadID=' + VarToStr(wBB3Settings.threads) + '&s=' + FSessionID
  else
    ARequestURL := Website + 'index.php?form=ThreadAdd&boardID=' + VarToStr(wBB3Settings.forums) + '&s=' + FSessionID;
end;

function TwBB3.DoAnalyzePrePost;
begin
  Result := not(Pos('name="idHash"', AResponseStr) = 0);
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

function TwBB3.DoBuildPostRequest;
const
  security_inputs: array [0 .. 1] of string = ('s', 'idHash');
var
  RequestURL: string;
  I: Integer;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'index.php?form=PostAdd&threadID=' + VarToStr(wBB3Settings.threads)
  else
    RequestURL := Website + 'index.php?form=ThreadAdd&boardID=' + VarToStr(wBB3Settings.forums);

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    Charset := wBB3Settings.Charset;
  end;

  /// Request.ContentType := 'application/x-www-form-urlencoded';

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
          begin
            repeat
              AddFormField(security_inputs[I], Match[1]);
            until not ExecNext;
          end;
        end;
      finally
        Free;
      end;

    AddFormField('prefix', VarToStr(wBB3Settings.prefix));

    AddFormField('threadIconActive', '1');
    AddFormField(wBB3Settings.icon_field, VarToStr(wBB3Settings.icon));

    AddFormField('subject', Subject);
    AddFormField('text', Message);

    AddFormField('tags', Tags);

    if PostReply then
      AddFormField('postID', '0');

    with wBB3Settings do
    begin
      if parseURL then
        AddFormField('parseURL', '1');
      if enableSmilies then
        AddFormField('enableSmilies', '1');
      if enableBBCodes then
        AddFormField('enableBBCodes', '1');
      if showSignature then
        AddFormField('showSignature', '1');
      if hasThank then
        AddFormField('hasThank', '1');
      if closeThread then
        AddFormField('closeThread', '1');
    end;

    AddFormField('send', 'Absenden');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
    RedirectMaximum := 1;
end;

function TwBB3.DoAnalyzePost;
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

function TwBB3.GetIDsRequestURL;
begin
  Result := Website + 'index.php?form=Search';
end;

function TwBB3.DoAnalyzeIDsRequest;
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

function TwBB3.GetName;
begin
  Result := 'wBB3';
end;

function TwBB3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TwBB3.BelongsTo;
begin
  Result := (Pos('index.php?form=UserLogin', string(AWebsiteSourceCode)) > 0);
end;

end.
