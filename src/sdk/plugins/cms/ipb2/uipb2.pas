unit uipb2;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  Tipb2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fenableemo, fenablesig: Boolean;

    ficon: Variant;
  public
    constructor Create; override;
  published
    property enableemo: Boolean read fenableemo write fenableemo;
    property enablesig: Boolean read fenablesig write fenablesig;

    property forums;
    property threads;
    property icon: Variant read ficon write ficon;
  end;

  Tipb2 = class(TCMSBoardPlugIn)
  private
    ipb2Settings: Tipb2Settings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

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
  end;

implementation

{ Tipb2Settings }

constructor Tipb2Settings.Create;
begin
  inherited Create;

  // default setup
    enableemo := False;
    enablesig := True;
end;

{ Tipb2 }

function Tipb2.SettingsClass;
begin
  Result := Tipb2Settings;
end;

function Tipb2.GetSettings;
begin
  Result := ipb2Settings;
end;

procedure Tipb2.SetSettings;
begin
  ipb2Settings := ACMSPlugInSettings as Tipb2Settings;
end;

function Tipb2.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with ipb2Settings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tipb2.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?act=Login&CODE=01');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := ipb2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('UserName', AccountName);
    AddFormField('PassWord', AccountPassword);
    AddFormField('referer', Website + 'index.php');
    AddFormField('act', 'Login');
    AddFormField('CODE', '01');
    AddFormField('CookieDate', '1');
    AddFormField('submit', 'Log+me+in');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tipb2.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('<div id="redirectwrap">', AResponseStr) = 0) and (Pos('act=Login&amp;CODE=03', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<div class="errorwrap"(>| style=''margin-bottom:0px;padding-bottom:0px''>)(.*?)<\/div>';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Trim(ReduceWhitespace(HTML2Text(Match[2])));
        end;
      finally
        Free;
      end;
end;

function Tipb2.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'index.php?act=post&do=reply_post&f=' + VarToStr(ipb2Settings.forums) + '&t=' + VarToStr(ipb2Settings.threads)
  else
    ARequestURL := Website + 'index.php?act=post&do=new_post&f=' + VarToStr(ipb2Settings.forums);
end;

function Tipb2.DoAnalyzePrePost;
begin
  Result := not(Pos('attach_post_key', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<div class="errorwrap"(>| style=''margin-bottom:0px;padding-bottom:0px''>)(.*?)<\/div>';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(ReduceWhitespace(HTML2Text(Match[2])));
      finally
        Free;
      end;
end;

function Tipb2.DoBuildPostRequest;
const
  security_inputs: array [0 .. 2] of string = ('s', 'attach_post_key', 'auth_key');
var
  I: Integer;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := ipb2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name=''' + security_inputs[I] + ''' value=''(.*?)''';

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

    AddFormField('f', VarToStr(ipb2Settings.forums));

    AddFormField('TopicTitle', Subject);

    AddFormField('TopicDesc', '');

    AddFormField('Post', Message);

    AddFormField('iconid', VarToStr(ipb2Settings.icon));

    AddFormField('ed-0_wysiwyg_used', '0');

    AddFormField('st', '0');

    AddFormField('act', 'Post');

    if PostReply then
    begin
      AddFormField('CODE', '03');
      AddFormField('t', VarToStr(ipb2Settings.threads));
    end
    else
      AddFormField('CODE', '01');

    if ipb2Settings.enableemo then
      AddFormField('enableemo', 'yes');
    if ipb2Settings.enablesig then
      AddFormField('enablesig', 'yes');

    AddFormField('dosubmit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tipb2.DoAnalyzePost;
var
  Headers: string;
begin
  /// used Response.RawHeaders.Text
  Headers := AHTTPProcess.HTTPResult.HTTPResponse.CustomHeaders.Text;

  Result := (not(Pos('<div id="redirectwrap">', AResponseStr) = 0) and (Pos('topicsread=', Headers) = 0) and (Pos('showtopic=', Headers) = 0)) or
  { . } (AHTTPProcess.HTTPResult.HTTPResponse.Cookies.IndexOf('topicsread') >= 0) or not(Pos('"topicsread"', AResponseStr) = 0);

  if not Result then
  begin
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<div class="(tablepad|errorwrap)">(.*?)<\/div>';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(ReduceWhitespace(HTML2Text(Match[2])));
      finally
        Free;
      end;
  end
  else
  begin
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := ';t=(\d+)';

        if Exec(InputString) then
          ArticleID := StrToIntDef(Match[1], 0);
      finally
        Free;
      end;
  end;
end;

function Tipb2.GetIDsRequestURL;
begin
  Result := Website + 'index.php?s=&act=Search&mode=simple&f=0';
end;

function Tipb2.DoAnalyzeIDsRequest;
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
        InputString := ExtractTextBetween(AResponseStr, 'name=''forums[]''', '</select>');
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

function Tipb2.GetName;
begin
  Result := 'ipb2';
end;

function Tipb2.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function Tipb2.BelongsTo;
begin
  Result := (Pos('act=Login&amp;CODE=00', string(AWebsiteSourceCode)) > 0) or
  { . } (Pos('act=Reg&amp;CODE=00', string(AWebsiteSourceCode)) > 0) or
  // for not escaped &amp; bug
  { . } (Pos('act=Login&CODE=00', string(AWebsiteSourceCode)) > 0) or
  { . } (Pos('act=Reg&CODE=00', string(AWebsiteSourceCode)) > 0);
end;

end.
