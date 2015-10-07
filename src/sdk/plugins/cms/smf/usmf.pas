unit usmf;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Variants,
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
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

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

function TSMF.SettingsClass;
begin
  Result := TSMFSettings;
end;

function TSMF.GetSettings;
begin
  Result := SMFSettings;
end;

procedure TSMF.SetSettings;
begin
  SMFSettings := ACMSPlugInSettings as TSMFSettings;
end;

function TSMF.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with SMFSettings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TSMF.NeedPreLogin;
begin
  Result := True;
  ARequestURL := Website;
end;

function TSMF.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?action=login2');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := SMFSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('user', AccountName);
    AddFormField('passwrd', AccountPassword);
    AddFormField('cookielength', '60');
    AddFormField('cookieneverexp', 'on');
    AddFormField('hash_passwrd', '');
    AddFormField('login', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TSMF.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('action=logout', AResponseStr) = 0) and (Pos('/logout/', AResponseStr) = 0) and (Pos('/action,logout/', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;

        Expression := '<b style="color: red;">(.*?)<\/b>';
        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));

        Expression := 'class="error">(.*?)<\/';
        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function TSMF.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'index.php?action=post;topic=' + VarToStr(SMFSettings.threads)
  else
    ARequestURL := Website + 'index.php?action=post;board=' + VarToStr(SMFSettings.forums);
end;

function TSMF.DoAnalyzePrePost;
begin
  Result := not(Pos('seqnum', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<td class="windowbg" style="padding-top: 2ex; padding-bottom: 2ex;">(.*?)<\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
      finally
        Free;
      end;
end;

function TSMF.DoBuildPostRequest;
const
  security_inputs: array [0 .. 1] of string = ('sc', 'seqnum');
var
  I: Integer;

  _captcha, _cookies: WideString;
  _captcha_text: string;
begin
  Result := True;

  Sleep(SMFSettings.sleeptime * 1000);

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?action=post2;start=0;board=' + VarToStr(SMFSettings.forums));
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := SMFSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
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

            _cookies := HTTPManager.GetResult(APrevRequest).HTTPResult.HTTPResponse.Cookies.Text;
            if not CAPTCHAInput(_captcha_text, GetName, _captcha, _cookies) then
            begin
              Self.ErrorMsg := StrAbortedThrougthCAP;
              Result := False;
            end;
            // CookieList := _cookies;
            AHTTPRequest.Cookies.Text := _cookies;
          end;

          AddFormField('post_vv[code]', _captcha);
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
              AddFormField(security_inputs[I], Match[1]);
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
            if (IndexText(Match[1], security_inputs) = -1) and (IndexText(Match[1], ['topic', 'icon', 'subject', 'message', 'submit', 'advanced', 'preview']) = -1) then
              AddFormField(Match[1], Match[2]);
          until not ExecNext;
        end;
      finally
        Free;
      end;

    if PostReply then
      AddFormField('topic', VarToStr(SMFSettings.threads));

    AddFormField(SMFSettings.prefix_field, VarToStr(SMFSettings.prefix));
    AddFormField('icon=', VarToStr(SMFSettings.icon));

    AddFormField('subject', Subject);
    AddFormField('message', Message);

    AddFormField('additional_options', '0');

    AddFormField('goback', '1');
    if SMFSettings.showsig then
      AddFormField('showsig', '1')
    else
      AddFormField('showsig', '0');

    if SMFSettings.ns then
      AddFormField('ns', 'NS');

    AddFormField('post', 'Post');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TSMF.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := 'id="errors">(.*?)(<\/tr>|<\/div>)';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(ReduceWhitespace(Trim(Match[1])));
        Result := False;
      end;
    finally
      Free;
    end;
end;

function TSMF.GetIDsRequestURL;
begin
  Result := Website + 'index.php?action=search;advanced;search';
end;

function TSMF.DoAnalyzeIDsRequest;
var
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
  with TRegExpr.Create do
    try
      InputString := AResponseStr;
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
                        InputString := AResponseStr;
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

function TSMF.GetName;
begin
  Result := 'SMF';
end;

function TSMF.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TSMF.BelongsTo;
begin
  // (Pos('type="hidden" name="hash_passwrd"', string(AWebsiteSourceCode)) > 0) or (Pos('action=login2', string(AWebsiteSourceCode)) > 0) or
  Result := (Pos('var smf_scripturl', string(AWebsiteSourceCode)) > 0);
end;

end.
