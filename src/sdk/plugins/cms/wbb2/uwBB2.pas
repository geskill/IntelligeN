unit uwBB2;

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
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  TwBB2Settings = class(TCMSBoardPlugInSettings)
  strict private
    fthxbot, fparseurl, femailnotify, fdisablesmilies, fdisablehtml, fdisablebbcode, fdisableimages, fshowsignature: Boolean;

    fprefix, ficon: Variant;
  public
    constructor Create; override;
  published
    property thxbot: Boolean read fthxbot write fthxbot;
    property parseurl: Boolean read fparseurl write fparseurl;
    property emailnotify: Boolean read femailnotify write femailnotify;
    property disablesmilies: Boolean read fdisablesmilies write fdisablesmilies;
    property disablehtml: Boolean read fdisablehtml write fdisablehtml;
    property disablebbcode: Boolean read fdisablebbcode write fdisablebbcode;
    property disableimages: Boolean read fdisableimages write fdisableimages;
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
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function DoAnalyzeIDsRequest(AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override; safecall;
  end;

implementation

{ TwBB2Settings }

constructor TwBB2Settings.Create;
begin
  inherited Create;

  // default setup
  thxbot := True;
  parseurl := True;
  emailnotify := False;
  disablesmilies := False;
  disablehtml := False;
  disablebbcode := False;
  disableimages := False;
  showsignature := True;
end;

{ TwBB2 }

function TwBB2.SettingsClass;
begin
  Result := TwBB2Settings;
end;

function TwBB2.GetSettings;
begin
  Result := wBB2Settings;
end;

procedure TwBB2.SetSettings;
begin
  wBB2Settings := ACMSPlugInSettings as TwBB2Settings;
end;

function TwBB2.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with wBB2Settings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TwBB2.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'login.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := wBB2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('l_username', AccountName);
    AddFormField('l_password', AccountPassword);
    AddFormField('send', 'send');
    AddFormField('url', 'index.php');
    AddFormField('s', '');
    AddFormField('submit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TwBB2.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('http-equiv="refresh"', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<td>(.*?)<\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TwBB2.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'addreply.php?threadid=' + VarToStr(wBB2Settings.threads)
  else
    ARequestURL := Website + 'newthread.php?boardid=' + VarToStr(wBB2Settings.forums);
end;

function TwBB2.DoAnalyzePrePost;
begin
  Result := True;
end;

function TwBB2.DoBuildPostRequest;
const
  security_inputs: array [0 .. 0] of string = ('idHash');
var
  RequestURL: string;
  I: Integer;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'addreply.php'
  else
    RequestURL := Website + 'newthread.php';

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := wBB2Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to Length(security_inputs) - 1 do
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

    AddFormField('prefix', VarToStr(wBB2Settings.prefix));
    AddFormField('iconid', VarToStr(wBB2Settings.icon));

    AddFormField('topic', Subject);
    AddFormField('message', Message);

    with wBB2Settings do
    begin
      if thxbot then
        AddFormField('thxbot', '1');
      if parseurl then
        AddFormField('parseurl', '1');
      if emailnotify then
        AddFormField('emailnotify', '1');
      if disablesmilies then
        AddFormField('disablesmilies', '1');
      if disablehtml then
        AddFormField('disablehtml', '1');
      if disablebbcode then
        AddFormField('disablebbcode', '1');
      if disableimages then
        AddFormField('disableimages', '1');
      if showsignature then
        AddFormField('showsignature', '1');
    end;

    if PostReply then
      AddFormField('threadid', VarToStr(wBB2Settings.threads))
    else
      AddFormField('boardid', VarToStr(wBB2Settings.forums));
    AddFormField('attachmentids', '');
    AddFormField('change_editor', '');
    AddFormField('poll_id', '');
    AddFormField('send', 'send');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TwBB2.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := '<ul><li>(.*?)<\/li><\/ul>';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(Match[1]);
        Result := False;
      end;
    finally
      Free;
    end;
end;

function TwBB2.DoAnalyzeIDsRequest;
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
        InputString := ExtractTextBetween(AResponseStr, 'name="boardids[]"', '</select>');
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

function TwBB2.GetName;
begin
  Result := 'wBB2';
end;

function TwBB2.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TwBB2.BelongsTo;
begin
  Result := (Pos('name="l_username"', string(AWebsiteSourceCode)) > 0);
end;

end.
