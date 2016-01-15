unit uDiscuzX;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  TDiscuzXSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhtmlon, fallowimgcode, fallowimgurl, fparseurloff, fsmileyoff, fbbcodeoff, fusesig, fordertype, fallownoticeauthor: Boolean;
  public
    constructor Create; override;
  published
    property htmlon: Boolean read fhtmlon write fhtmlon;
    property allowimgcode: Boolean read fallowimgcode write fallowimgcode;
    property allowimgurl: Boolean read fallowimgurl write fallowimgurl;
    property parseurloff: Boolean read fparseurloff write fparseurloff;
    property smileyoff: Boolean read fsmileyoff write fsmileyoff;
    property bbcodeoff: Boolean read fbbcodeoff write fbbcodeoff;
    property usesig: Boolean read fusesig write fusesig;
    property ordertype: Boolean read fordertype write fordertype;
    property allownoticeauthor: Boolean read fallownoticeauthor write fallownoticeauthor;

    property forums;
    property threads;
  end;

  TDiscuzX = class(TCMSBoardPlugIn)
  private
    DiscuzXSettings: TDiscuzXSettings;

  const
    error_reg_ex: string = '<div id="messagetext" class="alert_error">\s+<p>(.*?)<\/';
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
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
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TDiscuzXSettings }

constructor TDiscuzXSettings.Create;
begin
  inherited Create;

  // default setup
  htmlon := False;
  allowimgcode := True;
  allowimgurl := True;
  parseurloff := False;
  smileyoff := False;
  bbcodeoff := False;
  usesig := False;
  ordertype := False;
  allownoticeauthor := False;
end;

{ TDiscuzX }

function TDiscuzX.SettingsClass;
begin
  Result := TDiscuzXSettings;
end;

function TDiscuzX.GetSettings;
begin
  Result := DiscuzXSettings;
end;

procedure TDiscuzX.SetSettings;
begin
  DiscuzXSettings := ACMSPlugInSettings as TDiscuzXSettings;
end;

function TDiscuzX.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with DiscuzXSettings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TDiscuzX.NeedPreLogin;
begin
  Result := True;
  ARequestURL := Website + 'member.php?mod=logging&action=login';
end;

function TDiscuzX.DoBuildLoginRequest;
var
  _formhash, _loginhash: string;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := APrevResponse;
      Expression := 'name="login" id="loginform_(.*?)"';

      if Exec(InputString) then
        _loginhash := Match[1];

      Expression := 'name="formhash" value="(.*?)"';

      if Exec(InputString) then
        _formhash := Match[1];
    finally
      Free;
    end;

  AHTTPRequest := THTTPRequest.Create(Website + 'member.php?mod=logging&action=login&loginsubmit=yes&handlekey=login&floatlogin=yes&loginhash=' + _loginhash);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := DiscuzXSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('formhash', _formhash);
    AddFormField('referer', Website);
    AddFormField('loginfield', 'username');
    AddFormField('username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('cookietime', '2592000');
    AddFormField('questionid', '0');
    AddFormField('answer', '');
    AddFormField('loginsubmit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDiscuzX.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('window.location.href', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := error_reg_ex;

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDiscuzX.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'forum.php?mod=post&action=reply&extra=&fid=' + VarToStr(DiscuzXSettings.forums) + '&tid=' + VarToStr(DiscuzXSettings.threads)
  else
    ARequestURL := Website + 'forum.php?mod=post&action=newthread&extra=&fid=' + VarToStr(DiscuzXSettings.forums);
end;

function TDiscuzX.DoAnalyzePrePost;
begin
  Result := not(Pos('name="message"', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := error_reg_ex;

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDiscuzX.DoBuildPostRequest;
const
  security_inputs: array [0 .. 1] of string = ('formhash', 'posttime');
var
  RequestURL: string;

  I: Integer;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'forum.php?mod=post&action=reply&extra=&replysubmit=yes&fid=' + VarToStr(DiscuzXSettings.forums) + '&tid=' + VarToStr(DiscuzXSettings.threads)
  else
    RequestURL := Website + 'forum.php?mod=post&action=newthread&extra=&topicsubmit=yes&fid=' + VarToStr(DiscuzXSettings.forums);

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := DiscuzXSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name="' + security_inputs[I] + '" id="' + security_inputs[I] + '" value="(.*?)"';

          if Exec(InputString) then
            AddFormField(security_inputs[I], Match[1]);
        end;
      finally
        Free;
      end;

    AddFormField('wysiwyg', '0');
    AddFormField('subject', Subject);
    AddFormField('message', Message);
    AddFormField('tags', Tags);

    AddFormField('save', '');
    AddFormField('readperm', '');

    with DiscuzXSettings do
    begin
      if htmlon then
        AddFormField('htmlon', '1')
      else
        AddFormField('htmlon', '0');
      if allowimgcode then
        AddFormField('allowimgcode', 'on');
      if allowimgurl then
        AddFormField('allowimgurl', 'on');
      if parseurloff then
        AddFormField('parseurloff', '1');
      if smileyoff then
        AddFormField('smileyoff', '1');
      if bbcodeoff then
        AddFormField('bbcodeoff', '1')
      else
        AddFormField('bbcodeoff', '0');
      if usesig then
        AddFormField('usesig', '1')
      else
        AddFormField('usesig', '0');
      if ordertype then
        AddFormField('ordertype', '1')
      else
        AddFormField('ordertype', '0');
      if allownoticeauthor then
        AddFormField('allownoticeauthor', '1')
      else
        AddFormField('allownoticeauthor', '0');
    end;

    AddFormField('hiddenreplies', '0');
    AddFormField('addfeed', '0');

    if PostReply then
      AddFormField('replysubmit', 'true')
    else
      AddFormField('topicsubmit', 'true');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDiscuzX.DoAnalyzePost;
begin
  Result := not(Pos('window.location.href', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := error_reg_ex;

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDiscuzX.GetIDsRequestURL;
begin
  Result := Website + 'search.php?mod=forum&adv=yes';
end;

function TDiscuzX.DoAnalyzeIDsRequest;
var
  BoardLevel: TStringList;
  BoardLevelIndex: Integer;

  CategoryName, CategoryHTMLList: string;

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
      InputString := ExtractTextBetween(AResponseStr, 'name="srchfid[]"', '</select>');
      Expression := 'optgroup label="--(.*?)"(.*?)<\/optgroup';

      if Exec(InputString) then
      begin
        repeat
          CategoryName := Match[1];
          CategoryHTMLList := Match[2];

          BoardLevel := TStringList.Create;
          try
            // adding TOP-category
            BoardLevel.Add(CategoryName);

            with TRegExpr.Create do
              try
                InputString := CategoryHTMLList;
                Expression := 'option.*? value="(\d+)"\s*>([&nbsp; ]*)(.*?)<\/';

                if Exec(InputString) then
                begin
                  repeat
                    // need +1 because the TOP-category has been added already
                    BoardLevelIndex := CharCount('&nbsp;', Match[2]) + 1;

                    if BoardLevelIndex > 0 then
                      BoardLevelIndex := (BoardLevelIndex div 3) + 1;

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

        until not ExecNext;
      end;
    finally
      Free;
    end;
  Result := FCheckedIDsList.Count;
end;

function TDiscuzX.GetName;
begin
  Result := 'Discuz!X';
end;

function TDiscuzX.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TDiscuzX.BelongsTo;
begin
  Result := (Pos('member.php?mod=logging&amp;action=login', string(AWebsiteSourceCode)) > 0);
end;

function TDiscuzX.GetArticleLink;
begin
  // TODO:
  Result := Format('%sforum.php?mod=viewthread&tid=%d', [AURL, AArticleID]);
end;

end.
