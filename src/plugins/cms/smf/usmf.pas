unit usmf;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Controls, HTTPApp, Variants,
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
  TSMFSettings = class(TCMSBoardPlugInSettings)
  strict private
    fprefix_field: string;

    fsleeptime: Integer;
    fshowsig, fns: Boolean;

    fprefix, ficon: Variant;
  published
    [AttrDefaultValue('post_prefix')]
    property prefix_field: string read fprefix_field write fprefix_field;
    [AttrDefaultValue(0)]
    property sleeptime: Integer read fsleeptime write fsleeptime;
    [AttrDefaultValue(True)]
    property showsig: Boolean read fshowsig write fshowsig;
    [AttrDefaultValue(False)]
    property ns: Boolean read fns write fns;

    property forums;
    property threads;
    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  TSMF = class(TCMSBoardPlugIn)
  private
    SMFSettings: TSMFSettings;
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

function TSMF.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, SMFSettings, AComponentController);
  with SMFSettings do
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

function TSMF.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    // for getting a PHPSESSID
    Get(Website);

    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('user=' + AccountName);
        Add('passwrd=' + AccountPassword);
        Add('cookielength=60');
        Add('cookieneverexp=on');
        Add('hash_passwrd=');
        Add('login=');
      end;

      Request.CharSet := SMFSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php?action=login2', Params, Enc);
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

    if (Pos('action=logout', ResponseStr) = 0) and (Pos('/logout/', ResponseStr) = 0) and (Pos('/action,logout/', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;

          Expression := '<b style="color: red;">(.*?)<\/b>';
          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));

          Expression := 'class="error">(.*?)<\/';
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

procedure TSMF.PreSearchPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
begin
  with AIdHTTPHelper do
    try
      AResponse := Get(Website + 'index.php?action=search;advanced;search');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;
end;

function TSMF.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'index.php?action=post;topic=' + VarToStr(SMFSettings.threads))
      else
        AResponse := Get(Website + 'index.php?action=post;board=' + VarToStr(SMFSettings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('seqnum', AResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AResponse;
          Expression := '<td class="windowbg" style="padding-top: 2ex; padding-bottom: 2ex;">(.*?)<\/td>';

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

function TSMF.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('sc', 'seqnum');
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I: Integer;

  _captcha, _cookies: WideString;
  _captcha_text: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
{$REGION 'CAPTCHA'}
      if not(Pos('name="post_vv[code]"', APrevResponse) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := APrevResponse;
            Expression := 'new smfCaptcha\("(.*?)",';

            if Exec(InputString) then
            begin
              _captcha_text := Match[1];

              _cookies := CookieList;
              if not CAPTCHAInput(_captcha_text, GetName, _captcha, _cookies) then
              begin
                Self.ErrorMsg := StrAbortedThrougthCAP;
                Exit;
              end;
              CookieList := _cookies;
            end;

            Params.Add('post_vv[code]=' + _captcha);
          finally
            Free;
          end;
      end;
{$ENDREGION}
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

      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'name="(\w+)" value="(\w+)"';

          if Exec(InputString) then
          begin
            repeat
              if (IndexText(Match[1], security_inputs) = -1) and (IndexText(Match[1], ['topic', 'icon', 'subject', 'message', 'submit', 'advanced', 'preview'])
                  = -1) then
                Params.Add(Match[1] + '=' + Match[2]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

      if PostReply then
        Params.Add('topic=' + VarToStr(SMFSettings.threads));

      Params.Add(SMFSettings.prefix_field + '=' + VarToStr(SMFSettings.prefix));
      Params.Add('icon=' + VarToStr(SMFSettings.icon));

      Params.Add('subject=' + Subject);
      Params.Add('message=' + Message);

      Params.Add('additional_options=0');

      Params.Add('goback=1');
      if SMFSettings.showsig then
        Params.Add('showsig=1')
      else
        Params.Add('showsig=0');

      if SMFSettings.ns then
        Params.Add('ns=NS');

      Params.Add('post=Post');

      Sleep(SMFSettings.sleeptime * 1000);

      Request.CharSet := SMFSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php?action=post2;start=0;board=' + VarToStr(SMFSettings.forums), Params, Enc);
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
          Expression := 'id="errors">(.*?)(<\/tr>|<\/div>)';

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

constructor TSMF.Create;
begin
  inherited Create;
  SMFSettings := TSMFSettings.Create;
end;

destructor TSMF.Destroy;
begin
  SMFSettings.Free;
  inherited Destroy;
end;

function TSMF.GetName;
begin
  Result := 'SMF';
end;

function TSMF.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TSMF.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  // (Pos('type="hidden" name="hash_passwrd"', string(AWebsiteSourceCode)) > 0) or (Pos('action=login2', string(AWebsiteSourceCode)) > 0) or
  Result := (Pos('var smf_scripturl', string(AWebsiteSourceCode)) > 0);
end;

function TSMF.GetIDs: Integer;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;

  BoardLevel: TStringList;
  BoardLevelIndex: Integer;

  CategoryName, CategoryIDList, CategoryID: string;

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

  with TRegExpr.Create do
    try
      InputString := ResponseStr;
      Expression := 'onclick="selectBoards\(\[(.*?)\]\);.*?">([^<>]*?)<\/';

      if Exec(InputString) then
      begin
        repeat
          CategoryName := Match[2];
          CategoryIDList := Match[1];

          BoardLevel := TStringList.Create;
          try
            // adding TOP-category
            BoardLevel.Add(CategoryName);

            with TRegExpr.Create do
              try
                InputString := CategoryIDList;
                Expression := '(\d+)';

                if Exec(InputString) then
                begin
                  repeat
                    CategoryID := Match[1];

                    with TRegExpr.Create do
                      try
                        InputString := ResponseStr;
                        Expression := 'margin-left: (\d+)[^\/]*?name="brd\[' + CategoryID + '\]" value="' + CategoryID + '".*?>(.*?)<\/';

                        if Exec(InputString) then
                        begin
                          // need +1 because the TOP-category has been added already
                          BoardLevelIndex := StrToIntDef(Match[1], 0) + 1;

                          if (BoardLevelIndex = BoardLevel.Count) then
                            BoardLevel.Add(CleanPathName(Match[2]))
                          else
                          begin
                            repeat
                              BoardLevel.Delete(BoardLevel.Count - 1);
                            until (BoardLevelIndex = BoardLevel.Count);
                            BoardLevel.Add(CleanPathName(Match[2]));
                          end;

                          AddID(CategoryID, IDPath(BoardLevel));
                        end;
                      finally
                        Free;
                      end;

                  until not ExecNext;
                end;
              finally
                Free;
              end;

          finally
            BoardLevel.Free;
          end;
        until not ExecNext;
      end;
    finally
      Free;
    end;
  Result := FCheckedIDsList.Count;
end;

function TSMF.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TSMFSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
