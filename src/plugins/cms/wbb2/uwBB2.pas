unit uwBB2;

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
  TwBB2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fthxbot, fparseurl, femailnotify, fdisablesmilies, fdisablehtml, fdisablebbcode, fdisableimages, fshowsignature: Boolean;

    fprefix, ficon: Variant;
  published
    [AttrDefaultValue(True)]
    property thxbot: Boolean read fthxbot write fthxbot;
    [AttrDefaultValue(True)]
    property parseurl: Boolean read fparseurl write fparseurl;
    [AttrDefaultValue(False)]
    property emailnotify: Boolean read femailnotify write femailnotify;
    [AttrDefaultValue(False)]
    property disablesmilies: Boolean read fdisablesmilies write fdisablesmilies;
    [AttrDefaultValue(False)]
    property disablehtml: Boolean read fdisablehtml write fdisablehtml;
    [AttrDefaultValue(False)]
    property disablebbcode: Boolean read fdisablebbcode write fdisablebbcode;
    [AttrDefaultValue(False)]
    property disableimages: Boolean read fdisableimages write fdisableimages;
    [AttrDefaultValue(True)]
    property showsignature: Boolean read fshowsignature write fshowsignature;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  TwBB2 = class(TCMSBoardPlugIn)
  private
    wBB2Settings: TwBB2Settings;

  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    procedure PreSearchPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
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

function TwBB2.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, wBB2Settings, AComponentController);
  with wBB2Settings do
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

function TwBB2.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        Add('l_username=' + AccountName);
        Add('l_password=' + AccountPassword);
        Add('send=send');
        Add('url=index.php');
        Add('s=');
        Add('submit=');
      end;

      Request.CharSet := wBB2Settings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'login.php', Params, Enc);
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

    if (Pos('http-equiv="refresh"', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
          Expression := '<td>(.*?)<\/td>';

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

procedure TwBB2.PreSearchPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
begin
  with AIdHTTPHelper do
  begin
    try
      AResponse := Get(Website + 'search.php');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;
  end;
end;

function TwBB2.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'addreply.php?threadid=' + VarToStr(wBB2Settings.threads))
      else
        AResponse := Get(Website + 'newthread.php?boardid=' + VarToStr(wBB2Settings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

  end;
  Result := True;
end;

function TwBB2.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 0] of string = ('idHash');

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
          for I := 0 to Length(security_inputs) - 1 do
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

      Params.Add('prefix=' + VarToStr(wBB2Settings.prefix));
      Params.Add('iconid=' + VarToStr(wBB2Settings.icon));

      Params.Add('topic=' + Subject);
      Params.Add('message=' + Message);

      with wBB2Settings do
      begin
        if thxbot then
          Params.Add('thxbot=1');
        if parseurl then
          Params.Add('parseurl=1');
        if emailnotify then
          Params.Add('emailnotify=1');
        if disablesmilies then
          Params.Add('disablesmilies=1');
        if disablehtml then
          Params.Add('disablehtml=1');
        if disablebbcode then
          Params.Add('disablebbcode=1');
        if disableimages then
          Params.Add('disableimages=1');
        if showsignature then
          Params.Add('showsignature=1');
      end;

      if PostReply then
        Params.Add('threadid=' + VarToStr(wBB2Settings.threads))
      else
        Params.Add('boardid=' + VarToStr(wBB2Settings.forums));
      Params.Add('attachmentids=');
      Params.Add('change_editor=');
      Params.Add('poll_id=');
      Params.Add('send=send');

      Request.CharSet := wBB2Settings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'addreply.php', Params, Enc)
          else
            ResponseStr := Post(Website + 'newthread.php', Params, Enc);
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
      begin
        try
          InputString := ResponseStr;
          Expression := '<ul><li>(.*?)<\/li><\/ul>';

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
    finally
      Params.Free;
    end;
  end;
end;

function TwBB2.GetName;
begin
  Result := 'wBB2';
end;

constructor TwBB2.Create;
begin
  inherited Create;
  wBB2Settings := TwBB2Settings.Create;
end;

function TwBB2.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

destructor TwBB2.Destroy;
begin
  wBB2Settings.Free;
  inherited Destroy;
end;

function TwBB2.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('name="l_username"', string(AWebsiteSourceCode)) > 0);
end;

function TwBB2.GetIDs: Integer;
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

    PreSearchPage(IdHTTPHelper, ResponseStr);
  finally
    IdHTTPHelper.Free;
  end;

  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(ResponseStr, 'name="boardids[]"', '</select>');
        Expression := 'option value="(\d+)">(-*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('-', Match[2]);

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

function TwBB2.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TwBB2Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
