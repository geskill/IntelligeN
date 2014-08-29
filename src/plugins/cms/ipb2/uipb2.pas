unit uipb2;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, Variants,
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
  Tipb2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fenableemo, fenablesig: Boolean;

    ficon: Variant;
  published
    [AttrDefaultValue(False)]
    property enableemo: Boolean read fenableemo write fenableemo;
    [AttrDefaultValue(True)]
    property enablesig: Boolean read fenablesig write fenablesig;

    property forums;
    property threads;
    property icon: Variant read ficon write ficon;
  end;

  Tipb2 = class(TCMSBoardPlugIn)
  private
    ipb2Settings: Tipb2Settings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
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

function Tipb2.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, ipb2Settings, AComponentController);
  with ipb2Settings do
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

function Tipb2.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        Add('UserName=' + AccountName);
        Add('PassWord=' + AccountPassword);
        Add('referer= ' + Website + 'index.php');
        Add('act=Login');
        Add('CODE=01');
        Add('CookieDate=1');
        Add('submit=Log+me+in');
      end;

      Request.CharSet := ipb2Settings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php?act=Login&CODE=01', Params, Enc);
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

    if (Pos('<div id="redirectwrap">', ResponseStr) = 0) and (Pos('act=Login&amp;CODE=03', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<div class="errorwrap"(>| style=''margin-bottom:0px;padding-bottom:0px''>)(.*?)<\/div>';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[2]);
          end;
        finally
          Free;
        end;
    end;
  end;

  Result := True;
end;

function Tipb2.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'index.php?act=post&do=reply_post&f=' + VarToStr(ipb2Settings.forums) + '&t=' + VarToStr(ipb2Settings.threads))
      else
        AResponse := Get(Website + 'index.php?act=post&do=new_post&f=' + VarToStr(ipb2Settings.forums));
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
        try
          InputString := AResponse;
          Expression := '<div class="errorwrap"(>| style=''margin-bottom:0px;padding-bottom:0px''>)(.*?)<\/div>';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[2]);
          end;
        finally
          Free;
        end;

      Exit;
    end;
  end;
  Result := True;
end;

function Tipb2.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 2] of string = ('s', 'attach_post_key', 'auth_key');
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
            Expression := 'name=''' + security_inputs[I] + ''' value=''(.*?)''';

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

      with Params do
      begin
        Add('f=' + VarToStr(ipb2Settings.forums));

        Add('TopicTitle=' + Subject);

        Add('TopicDesc=');

        Add('Post=' + Message);

        Add('iconid=' + VarToStr(ipb2Settings.icon));

        Add('ed-0_wysiwyg_used=0');

        Add('st=0');

        Add('act=Post');

        if PostReply then
        begin
          Add('CODE=03&');
          Add('t=' + VarToStr(ipb2Settings.threads));
        end
        else
          Add('CODE=01&');

        if ipb2Settings.enableemo then
          Add('enableemo=yes');
        if ipb2Settings.enablesig then
          Add('enablesig=yes');

        Add('dosubmit=');
      end;

      Request.CharSet := ipb2Settings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php', Params, Enc);
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

      if (Pos('<div id="redirectwrap">', ResponseStr) = 0) and (Pos('topicsread=', Response.RawHeaders.Text) = 0) and
        (Pos('showtopic=', Response.RawHeaders.Text) = 0) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStr;
            Expression := '<div class="(tablepad|errorwrap)">(.*?)<\/div>';

            if Exec(InputString) then
              Self.ErrorMsg := HTML2Text(Match[2]);
          finally
            Free;
          end;
        end;

        Exit;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor Tipb2.Create;
begin
  inherited Create;
  ipb2Settings := Tipb2Settings.Create;
end;

destructor Tipb2.Destroy;
begin
  ipb2Settings.Free;
  inherited Destroy;
end;

function Tipb2.GetName;
begin
  Result := 'ipb2';
end;

function Tipb2.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function Tipb2.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('act=Login&amp;CODE=00', string(AWebsiteSourceCode)) > 0);
end;

function Tipb2.GetIDs: Integer;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'index.php?s=&act=Search&mode=simple&f=0');
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
        InputString := ExtractTextBetween(ResponseStr, 'name=''forums[]''', '</select>');
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

function Tipb2.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, Tipb2Settings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
