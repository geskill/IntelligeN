unit uwBB3;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols, IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TwBB3Settings = class(TCMSBoardPlugInSettings)
  strict private
    ficon_field: string;

    fparseURL, fenableSmilies, fenableBBCodes, fshowSignature, fhasThank, fcloseThread, fintelligent_posting, fintelligent_posting_helper,
      fintelligent_posting_boundedsearch: Boolean;

    fprefix, ficon: Variant;
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

  TwBB3 = class(TCMSBoardIPPlugIn)
  private
    wBB3Settings: TwBB3Settings;
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

function TwBB3.LoadSettings;
begin
  Result := True;
  wBB3Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, wBB3Settings,
    AComponentController);
  with wBB3Settings do
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

function TwBB3.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  _captchaID: string;
  _captcha, _cookies: WideString;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        // http://stackoverflow.com/questions/7189794/indy-adds-at-every-72nd-char-with-multi-part-form-data-post
        AddFormField('loginUsername', AccountName, wBB3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('loginPassword', AccountPassword, wBB3Settings.Charset).ContentTransfer := 'binary';
        AddFormField('useCookies', '1', wBB3Settings.Charset);
        AddFormField('url', '', wBB3Settings.Charset);
        AddFormField('s', '', wBB3Settings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'index.php?form=UserLogin', Params);
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

    if (Pos('name="captchaString"', ResponseStr) > 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := 'captchaID=(\d+)';

          if Exec(InputString) then
            _captchaID := Match[1];
        finally
          Free;
        end;

      if not CAPTCHAInput(Website + 'index.php?page=Captcha&captchaID=' + _captchaID, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Exit;
      end;

      Params := TIdMultiPartFormDataStream.Create;
      try
        with Params do
        begin
          AddFormField('loginUsername', AccountName, wBB3Settings.Charset).ContentTransfer := 'binary';
          AddFormField('loginPassword', AccountPassword, wBB3Settings.Charset).ContentTransfer := 'binary';

          AddFormField('useCookies', '1');

          AddFormField('captchaString', _captcha, wBB3Settings.Charset).ContentTransfer := 'binary';
          AddFormField('captchaID', _captchaID, wBB3Settings.Charset).ContentTransfer := 'binary';

          AddFormField('url', '', wBB3Settings.Charset);
          AddFormField('s', '', wBB3Settings.Charset);
        end;

        try
          ResponseStr := Post(Website + 'index.php?form=UserLogin', Params);
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

    if (Pos('<div class="success">', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
          Expression := '<p class="error">(.*?)<\/p>';

          if Exec(InputString) then
          begin
            repeat
              Self.ErrorMsg := HTML2Text(Match[1]);
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
        Expression := 's=(\w+)"';

        if Exec(InputString) then
          FSessionID := Match[1];
      finally
        Free;
      end;
  end;
  Result := True;
end;

function TwBB3.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

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
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if wBB3Settings.intelligent_posting then
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
            AddFormField('q', SearchValue, wBB3Settings.Charset).ContentTransfer := 'binary';
            AddFormField('s', FSessionID, wBB3Settings.Charset).ContentTransfer := 'binary';
            AddFormField('findThreads', '1', wBB3Settings.Charset);
            if wBB3Settings.intelligent_posting_boundedsearch then
            begin
              for I := 0 to length(wBB3Settings.intelligent_posting_bounds) - 1 do
                AddFormField('boardIDs[]', IntToStr(wBB3Settings.intelligent_posting_bounds[I]), wBB3Settings.Charset)
                  .ContentTransfer := 'binary'
            end
            else
              AddFormField('boardIDs[]', '*', wBB3Settings.Charset);
          end;

          try
            ResponseStr := Post(Website + 'index.php?form=Search', Params);
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
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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

function TwBB3.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'index.php?form=PostAdd&threadID=' + VarToStr(wBB3Settings.threads) + '&s=' + FSessionID)
      else
        AResponse := Get(Website + 'index.php?form=ThreadAdd&boardID=' + VarToStr(wBB3Settings.forums) + '&s=' + FSessionID);
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('name="idHash"', AResponse) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := AResponse;
          Expression := '<p class="error">(.*?)<\/p>';

          if Exec(InputString) then
          begin
            repeat
              Self.ErrorMsg := HTML2Text(Trim(Match[1]));
            until not ExecNext;
          end;
        finally
          Free;
        end;
      end;
      Exit;
    end;
  end;
  Result := True;
end;

function TwBB3.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('s', 'idHash');
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
            Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

            if Exec(InputString) then
            begin
              repeat
                Params.AddFormField(security_inputs[I], Match[1], wBB3Settings.Charset).ContentTransfer := 'binary';
              until not ExecNext;
            end;
          end;
        finally
          Free;
        end;

      Params.AddFormField('prefix', VarToStr(wBB3Settings.prefix), wBB3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('threadIconActive', '1');
      Params.AddFormField(wBB3Settings.icon_field, VarToStr(wBB3Settings.icon), wBB3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('subject', Subject, wBB3Settings.Charset).ContentTransfer := 'binary';
      Params.AddFormField('text', Message, wBB3Settings.Charset).ContentTransfer := 'binary';

      Params.AddFormField('tags', Tags, wBB3Settings.Charset).ContentTransfer := 'binary';

      if PostReply then
        Params.AddFormField('postID', '0', wBB3Settings.Charset);

      with wBB3Settings do
      begin
        if parseURL then
          Params.AddFormField('parseURL', '1', wBB3Settings.Charset);
        if enableSmilies then
          Params.AddFormField('enableSmilies', '1', wBB3Settings.Charset);
        if enableBBCodes then
          Params.AddFormField('enableBBCodes', '1', wBB3Settings.Charset);
        if showSignature then
          Params.AddFormField('showSignature', '1', wBB3Settings.Charset);
        if hasThank then
          Params.AddFormField('hasThank', '1', wBB3Settings.Charset);
        if closeThread then
          Params.AddFormField('closeThread', '1', wBB3Settings.Charset);
      end;

      Params.AddFormField('send', 'Absenden', wBB3Settings.Charset);

      RedirectMaximum := 1;
      Request.ContentType := 'application/x-www-form-urlencoded';
      try
        if PostReply then
          ResponseStr := Post(Website + 'index.php?form=PostAdd&threadID=' + VarToStr(wBB3Settings.threads), Params)
        else
          ResponseStr := Post(Website + 'index.php?form=ThreadAdd&boardID=' + VarToStr(wBB3Settings.forums), Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<p class="error">(.*?)<\/p>';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
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

constructor TwBB3.Create;
begin
  inherited Create;
  wBB3Settings := TwBB3Settings.Create;
end;

destructor TwBB3.Destroy;
begin
  wBB3Settings.Free;
  inherited Destroy;
end;

function TwBB3.GetName;
begin
  Result := 'wBB3';
end;

function TwBB3.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TwBB3.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('index.php?form=UserLogin', string(AWebsiteSourceCode)) > 0);
end;

function TwBB3.GetIDs: Integer;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'index.php?form=Search');
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
        InputString := ExtractTextBetween(ResponseStr, 'name="boardIDs[]"', '</select>');
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

function TwBB3.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TwBB3Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
