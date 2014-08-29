unit uMyBB;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Controls, HTTPApp, XMLIntf, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TMyBBSettings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field: string;
    fuse_coverlink, fsignature, fdisablesmilies, fintelligent_posting, fintelligent_posting_helper: Boolean;

    fprefix, ficon: Variant;
  protected
    xthreads: array of array of Variant;
  published
    [AttrDefaultValue('threadprefix')]
    property prefix_field: string read fprefix_field write fprefix_field;
    [AttrDefaultValue(False)]
    property use_coverlink: Boolean read fuse_coverlink write fuse_coverlink;
    [AttrDefaultValue(True)]
    property signature: Boolean read fsignature write fsignature;
    [AttrDefaultValue(False)]
    property disablesmilies: Boolean read fdisablesmilies write fdisablesmilies;
    [AttrDefaultValue(False)]
    property intelligent_posting: Boolean read fintelligent_posting write fintelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper: Boolean read fintelligent_posting_helper write fintelligent_posting_helper;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  TMyBB = class(TCMSBoardIPPlugIn)
  private
    MyBBSettings: TMyBBSettings;
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

function TrueXMLNodeName(ANodeName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to length(ANodeName) do
    if (ANodeName[I] in ['a' .. 'z', 'A' .. 'Z', '_']) then
      Result := Result + ANodeName[I];
end;

function TMyBB.LoadSettings;
begin
  Result := True;
  with MyBBSettings do
  begin
    TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, MyBBSettings, AComponentController,
      { } procedure(AXMLNode: IXMLNode)
      { } var
      { . } Y, K: Integer;
      { } begin
      { . } if Assigned(AComponentController) and Assigned(AXMLNode.ChildNodes.FindNode('xthreads')) then
      { . } begin
      { ... } with AXMLNode.ChildNodes.Nodes['xthreads'] do
      { ..... } for Y := 0 to ChildNodes.Count - 1 do
      { ..... } begin
      { ....... } SetLength(xthreads, Y + 1, 2);
      { ....... } xthreads[Y][0] := ChildNodes.Nodes[Y].NodeValue
      { ..... } end;
      { ... } for K := 0 to length(xthreads) - 1 do
      { ..... } with AXMLNode.ChildNodes.Nodes[TrueXMLNodeName(xthreads[K][0])] do
      { ..... } begin
      { ....... } for Y := 0 to ChildNodes.Count - 1 do
      { ......... } if SameText(VarToStr(ChildNodes.Nodes[Y].Attributes['name']),
        TTemplateTypeIDToString(AComponentController.TemplateTypeID)) then
      { ......... } begin
      { ........... } xthreads[K][1] := TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[Y]);

      { ........... } TPlugInCMSSettingsHelper.SubSearch(ChildNodes.Nodes[Y], AComponentController, xthreads[K][1]);
      { ........... } break;
      { ......... } end;
      { ..... } end;
      { . } end;
      { } end);
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

function TMyBB.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  _captcha, _cookies: WideString;
  _imagehash: string;
begin
  Result := True;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('username=' + AccountName);
        Add('password=' + AccountPassword);
        Add('url=');
        Add('action=do_login');
        Add('submit=');
      end;
      Request.CharSet := MyBBSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'member.php', Params, Enc);
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

    if not(Pos('name="imagehash"', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := 'name="imagehash" value="(.*?)"';

          if Exec(InputString) then
            _imagehash := Match[1];
        finally
          Free;
        end;

      if not CAPTCHAInput(Website + 'captcha.php?imagehash=' + _imagehash, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Exit;
      end;

      Params := TStringList.Create;
      try
        with Params do
        begin
          Add('username=' + AccountName);
          Add('password=' + AccountPassword);
          Add('imagehash=' + _imagehash);
          Add('imagestring=' + _captcha);
          Add('url=');
          Add('action=do_login');
          Add('submit=');
        end;
        Request.CharSet := MyBBSettings.CharSet;
        Enc := CharsetToEncoding(Request.CharSet);
        try
          try
            ResponseStr := Post(Website + 'member.php', Params, Enc);
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
  end;

  if (Pos('http-equiv="refresh"', ResponseStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ResponseStr;
        if (Pos('<div class="error">', ResponseStr) = 0) then
          Expression := '<td class="trow1">(.*?)<\/td>'
        else
          Expression := '<div class="error">(.*?)<\/div>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Trim(Match[1]));
      finally
        Free;
      end;
    Exit;
  end;
  Result := True;
end;

function TMyBB.IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean;

  function GetSearchTitle(ATitle: string): string;
  var
    X: Integer;
  begin
    Result := ATitle;
    for X := length(Result) downto 1 do
      if (Result[X] in ['+', '_', '.', ':', '(', ')', '[', ']', '/', '\']) then
        Result[X] := ' ';
    Result := Trim(ReduceWhitespace(Result));
  end;

var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  SearchValue: WideString;
  SearchResults: TStringList;
  SearchIndex: Integer;
  RedoSearch: Boolean;

  _found_thread_id, _found_thread_name: string;
begin
  Result := True;
  if MyBBSettings.intelligent_posting then
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
            Add('action=do_search');
            Add('keywords=' + SearchValue);
            Add('postthread=1');
            Add('forums[]=');
            Add('sortordr=desc');
            Add('showresults=threads');
            Add('submit=');
          end;

          Request.CharSet := MyBBSettings.CharSet;
          Enc := CharsetToEncoding(Request.CharSet);
          try
            try
              RedirectMaximum := 1;
              ResponseStr := Post(Website + 'search.php', Params, Enc);
              RedirectMaximum := 0;
            except
              // ignore exceptions ????
              (*
                on E: Exception do
                begin
                ErrorMsg := E.message;
                Exit;
                end;
                *)
            end;
          finally
            Enc.Free;
          end;
        finally
          Params.Free;
        end;

        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := '"2;URL=(.*?)"';

            if Exec(InputString) then
            begin
              try
                ResponseStr := Get(Website + HTMLDecode(Match[1]));
              except
                on E: Exception do
                begin
                  Self.ErrorMsg := E.message;
                  Exit;
                end;
              end;
            end;
          finally
            Free;
          end;
      end;

      SearchResults := TStringList.Create;
      try
        SearchResults.Add('0=Create new Thread');
        with TRegExpr.Create do
          try
            ModifierS := False;
            InputString := ResponseStr;
            Expression := 'showthread\.php\?tid=(\d+).*?id="tid_\d+".*?>([^>]*?)<\/a>';

            if Exec(InputString) then
            begin
              repeat
                _found_thread_id := Match[1];
                _found_thread_name := Match[2];

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
                        MyBBSettings.threads := _found_thread_id;
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

        if MyBBSettings.intelligent_posting_helper then
        begin
          if not IntelligentPostingHelper(SearchValue, SearchResults.Text, SearchIndex, RedoSearch) then
          begin
            ErrorMsg := StrAbortedThrougthInt;
            Result := False;
          end;
          PostReply := (SearchIndex > 0);
          if PostReply then
            MyBBSettings.threads := SearchResults.Names[SearchIndex];
        end;
      finally
        SearchResults.Free;
      end;
    until not RedoSearch;
  end;
end;

function TMyBB.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'newreply.php?tid=' + VarToStr(MyBBSettings.threads))
      else
        AResponse := Get(Website + 'newthread.php?fid=' + VarToStr(MyBBSettings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    // TODO: Überprüfung ob der User überhaupt Rechte hat, korrekt eingeloggt wurde
  end;
  Result := True;
end;

function TMyBB.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('my_post_key', 'posthash');
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

      for I := 0 to length(MyBBSettings.xthreads) - 1 do
        if not(MyBBSettings.xthreads[I][1] = null) then
          Params.Add(VarToStr(MyBBSettings.xthreads[I][0]) + '=' + VarToStr(MyBBSettings.xthreads[I][1]));

      Params.Add(MyBBSettings.prefix_field + '=' + VarToStr(MyBBSettings.prefix));

      if MyBBSettings.use_coverlink and Assigned(AComponentController.FindControl(cPicture)) then
        Params.Add('xthreads_pref_pic2=' + AComponentController.FindControl(cPicture).Value);

      Params.Add('icon=' + VarToStr(MyBBSettings.icon));
      Params.Add('subject=' + Subject);
      Params.Add('message=' + Message);

      with MyBBSettings do
      begin
        if signature then
          Params.Add('postoptions[signature]=1');
        if disablesmilies then
          Params.Add('postoptions[disablesmilies]=1');
      end;

      Params.Add('submit=');

      if PostReply then
      begin
        Params.Add('tid=' + VarToStr(MyBBSettings.threads));
        Params.Add('action=do_newreply');
      end
      else if (ArticleID = 0) then
        Params.Add('action=do_newthread');

      Request.CharSet := MyBBSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'newreply.php?tid=' + VarToStr(MyBBSettings.threads) + '&processed=1', Params, Enc)
          else
            ResponseStr := Post(Website + 'newthread.php?fid=' + VarToStr(MyBBSettings.forums) + '&processed=1', Params, Enc);
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
          Expression := '<div class="error">(.*?)<\/div>';

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

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor TMyBB.Create;
begin
  inherited Create;
  MyBBSettings := TMyBBSettings.Create;
end;

destructor TMyBB.Destroy;
begin
  MyBBSettings.Free;
  inherited Destroy;
end;

function TMyBB.GetName;
begin
  Result := 'MyBB';
end;

function TMyBB.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TMyBB.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('member.php?action=register', string(AWebsiteSourceCode)) > 0) or (Pos('MyBB', string(AWebsiteSourceCode)) > 0);
end;

function TMyBB.GetIDs: Integer;
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
        InputString := ExtractTextBetween(ResponseStr, 'name="forums[]"', '</select>');
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

function TMyBB.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TMyBBSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
