unit uwBB4;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TwBB4Settings = class(TCMSBoardPlugInSettings)
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

  TwBB4 = class(TCMSBoardIPPlugIn)
  private
    wBB4Settings: TwBB4Settings;
    FSessionID: string;

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

function TwBB4.LoadSettings;
begin
  Result := True;
  wBB4Settings.intelligent_posting_bounds := TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, wBB4Settings, AComponentController);
  with wBB4Settings do
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

function TwBB4.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    ResponseStr := Get(Website + 'index.php/Login/');

    with TRegExpr.Create do
      try
        InputString := ResponseStr;
        Expression := 'SECURITY_TOKEN = ''(\w+)''';

        if Exec(InputString) then
          FSessionID := Match[1];
      finally
        Free;
      end;

    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('username=' + AccountName);
        Add('action=login');
        Add('password=' + AccountPassword);
        Add('useCookies=1');
        Add('url=');
        Add('t=' + FSessionID);
      end;

      Request.Charset := wBB4Settings.Charset;
      Enc := CharsetToEncoding(Request.Charset);
      try
        try
          ResponseStr := Post(Website + 'index.php/Login/', Params, Enc);
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

    if (Pos('<div class="success">', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
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
      end;
    end;
  end;
  Result := True;
end;

function TwBB4.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

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
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if wBB4Settings.intelligent_posting then
  begin
    SearchValue := GetSearchTitle(Subject);

    RedoSearch := False;
    repeat
      SearchIndex := 0;

      with AIdHTTPHelper do
      begin
        Params := TStringList.Create;
        try
          with Params do
          begin
            Add('q=' + SearchValue);
            Add('subjectOnly=1');
            Add('findThreads=1');

            if wBB4Settings.intelligent_posting_boundedsearch then
            begin
              for I := 0 to length(wBB4Settings.intelligent_posting_bounds) - 1 do
                Add('boardIDs[]=' + IntToStr(wBB4Settings.intelligent_posting_bounds[I]));
            end
            else
              Add('boardIDs[]=*');

            Add('t=' + FSessionID);
          end;

          Request.Charset := wBB4Settings.Charset;
          Enc := CharsetToEncoding(Request.Charset);
          try
            try
              ResponseStr := Post(Website + 'index.php/Search/', Params, Enc);
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
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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

function TwBB4.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'index.php/PostAdd/' + VarToStr(wBB4Settings.threads) + '/')
      else
        AResponse := Get(Website + 'index.php/ThreadAdd/' + VarToStr(wBB4Settings.forums) + '/');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('name="tmpHash"', AResponse) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := AResponse;
          Expression := '<p class="error">(.*?)<\/p>';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Trim(Match[1]));
        finally
          Free;
        end;
      end;
      Exit;
    end;
  end;
  Result := True;
end;

function TwBB4.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('tmpHash', 't');
var
  ResponseStr: string;
  Params: TStringList;
  Enc: TEncoding;

  I: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
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
                Params.Add(security_inputs[I] + '=' + Match[1]);
              until not ExecNext;
            end;
          end;
        finally
          Free;
        end;

      Params.Add('prefix=' + VarToStr(wBB4Settings.prefix));

      Params.Add('threadIconActive=1');
      Params.Add(wBB4Settings.icon_field + '=' + VarToStr(wBB4Settings.icon));

      Params.Add('subject=' + Subject);
      Params.Add('text=' + Message);

      Params.Add('tags=' + Tags);

      if PostReply then
        Params.Add('postID=0');

      with wBB4Settings do
      begin
        if preParse then
          Params.Add('preParse=1');
        if enableSmilies then
          Params.Add('enableSmilies=1');
        if enableBBCodes then
          Params.Add('enableBBCodes=1');
        if enableHtml then
          Params.Add('enableHtml=1');
        if showSignature then
          Params.Add('showSignature=1');
        if hasThank then
          Params.Add('hasThank=1');
        if closeThread then
          Params.Add('closeThread=1');
        if disableThread then
          Params.Add('disableThread=1');
      end;

      Params.Add('send=Absenden');

      RedirectMaximum := 1;

      Request.Charset := wBB4Settings.Charset;
      Enc := CharsetToEncoding(Request.Charset);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'index.php/PostAdd/' + VarToStr(wBB4Settings.threads) + '/', Params, Enc)
          else
            ResponseStr := Post(Website + 'index.php/ThreadAdd/' + VarToStr(wBB4Settings.forums) + '/', Params, Enc);
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

constructor TwBB4.Create;
begin
  inherited Create;
  wBB4Settings := TwBB4Settings.Create;
end;

destructor TwBB4.Destroy;
begin
  wBB4Settings.Free;
  inherited Destroy;
end;

function TwBB4.GetName;
begin
  Result := 'wBB4';
end;

function TwBB4.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TwBB4.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('com.woltlab.wbb.post', string(AWebsiteSourceCode)) > 0);
end;

function TwBB4.GetIDs: Integer;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'index.php/Search/');
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

function TwBB4.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TwBB4Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
