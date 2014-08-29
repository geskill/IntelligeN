unit uXenForo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants, HTTPApp,
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
  TXenForoSettings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field: string;
    fintelligent_posting, fintelligent_posting_helper, fintelligent_posting_boundedsearch: Boolean;
    fprefix: Variant;
  public
    intelligent_posting_bounds: TIntegerArray;
  published
    [AttrDefaultValue('prefix_id')]
    property prefix_field: string read fprefix_field write fprefix_field;
    [AttrDefaultValue(False)]
    property intelligent_posting: Boolean read fintelligent_posting write fintelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper: Boolean read fintelligent_posting_helper write fintelligent_posting_helper;
    [AttrDefaultValue(False)]
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

{ TXenForo }

function TXenForo.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, XenForoSettings, AComponentController);
  with XenForoSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AComponentController) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TXenForo.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('login=' + AccountName);
        Add('register=0');
        Add('password=' + AccountPassword);
        Add('remember=1');
        Add('cookie_check=0');
        Add('redirect=');
        Add('_xfToken=');
      end;

      Request.CharSet := XenForoSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'login/login', Params, Enc);
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

    if (Pos('logout/?_xfToken=', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
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

      Exit;
    end
    else
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '_xfToken=(.*?)"';

          if Exec(InputString) then
            FSessionID := HTTPDecode(Match[1]);
        finally
          Free;
        end;
    end;
  end;
  Result := True;
end;

function TXenForo.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

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
  ResponseStr: string;
  Params: TStringList;
  Enc: TEncoding;

  I: Integer;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_thread_id, _found_thread_name: Variant;
begin
  Result := True;
  if XenForoSettings.intelligent_posting then
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
            Add('keywords=' + SearchValue);
            Add('title_only=1');
            Add('users=');
            Add('date=');
            Add('reply_count=0');
            Add('prefixes[]=0');
            if XenForoSettings.intelligent_posting_boundedsearch then
            begin
              for I := 0 to length(XenForoSettings.intelligent_posting_bounds) - 1 do
                Add('nodes[]=' + IntToStr(XenForoSettings.intelligent_posting_bounds[I]))
            end;
            Add('child_nodes=1');
            Add('order=date');
            Add('type=post');
            Add('_xfToken=' + FSessionID);
            Add('_xfRequestUri=/search/?type=post');
            Add('_xfNoRedirect=1');
            Add('_xfToken=' + FSessionID);
            // Add('_xfResponseType=json');
          end;

          Request.CharSet := XenForoSettings.CharSet;
          Enc := CharsetToEncoding(Request.CharSet);
          try
            try
              ResponseStr := Post(Website + 'search/search', Params, Enc);
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
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
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
    until not RedoSearch;
  end;
end;

function TXenForo.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'threads/' + VarToStr(XenForoSettings.threads) + '/')
      else
        AResponse := Get(Website + 'forums/' + VarToStr(XenForoSettings.forums) + '/create-thread');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('name="message_html"', AResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AResponse;
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

      Exit;
    end;
  end;
  Result := True;
end;

function TXenForo.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string = ''): Boolean;
const
  security_inputs: array [0 .. 2] of string = ('attachment_hash', 'last_date', '_xfToken');
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        with TRegExpr.Create do
          try
            for I := 0 to length(security_inputs) - 1 do
            begin
              InputString := APrevResponse;
              Expression := 'name="' + security_inputs[I] + '" value="(.*?)"';

              if Exec(InputString) then
                Add(security_inputs[I] + '=' + Match[1]);
            end;
          finally
            Free;
          end;

        Add(XenForoSettings.prefix_field + '=' + VarToStr(XenForoSettings.prefix));

        if not PostReply then
          Add('title=' + Subject);

        Add('message=' + Message);

        Add('watch_thread=0&');
        Add('watch_thread_email=0&');
        Add('watch_thread_state=0&');
      end;

      Request.CharSet := XenForoSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'threads/' + VarToStr(XenForoSettings.threads) + '/add-reply', Params, Enc)
          else
            ResponseStr := Post(Website + 'forums/' + VarToStr(XenForoSettings.forums) + '/add-thread', Params, Enc);
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

      if (Pos('name="message_html"', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
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

        Exit;
      end;
    finally
      Params.Free;
    end;
  end;
  Result := True;
end;

constructor TXenForo.Create;
begin
  inherited Create;
  XenForoSettings := TXenForoSettings.Create;
end;

destructor TXenForo.Destroy;
begin
  XenForoSettings.Free;
  inherited Destroy;
end;

function TXenForo.GetName: WideString;
begin
  Result := 'XenForo';
end;

function TXenForo.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TXenForo.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('login/login', string(AWebsiteSourceCode)) > 0) and (Pos('_xfToken', string(AWebsiteSourceCode)) > 0);
end;

function TXenForo.GetIDs;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'search/?type=post');
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
        InputString := ExtractTextBetween(ResponseStr, 'name="nodes[]"', '</select>');
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

function TXenForo.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TXenForoSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
