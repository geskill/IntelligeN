unit uDiscuz;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uConst, uWebsiteInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  TDiscuzSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhtmlon, fallowimgcode, fparseurloff, fsmileyoff, fbbcodeoff, ftagoff, fusesig, faddtoblog: Boolean;
    ficon: Variant;
  published
    [AttrDefaultValue(False)]
    property htmlon: Boolean read fhtmlon write fhtmlon;
    [AttrDefaultValue(True)]
    property allowimgcode: Boolean read fallowimgcode write fallowimgcode;
    [AttrDefaultValue(False)]
    property parseurloff: Boolean read fparseurloff write fparseurloff;
    [AttrDefaultValue(False)]
    property smileyoff: Boolean read fsmileyoff write fsmileyoff;
    [AttrDefaultValue(False)]
    property bbcodeoff: Boolean read fbbcodeoff write fbbcodeoff;
    [AttrDefaultValue(False)]
    property tagoff: Boolean read ftagoff write ftagoff;
    [AttrDefaultValue(False)]
    property usesig: Boolean read fusesig write fusesig;
    [AttrDefaultValue(False)]
    property addtoblog: Boolean read faddtoblog write faddtoblog;

    property forums;
    property threads;
    property icon: Variant read ficon write ficon;
  end;

  TDiscuz = class(TCMSBoardPlugIn)
  private
    DiscuzSettings: TDiscuzSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AWebsiteData: ICMSWebsiteData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AWebsiteData: ICMSWebsiteData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function DoAnalyzeIDsRequest(AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override; safecall;
  end;

implementation

{ TDiscuz }

function TDiscuz.SettingsClass;
begin
  Result := TDiscuzSettings;
end;

function TDiscuz.GetSettings;
begin
  Result := DiscuzSettings;
end;

procedure TDiscuz.SetSettings;
begin
  DiscuzSettings := ACMSPlugInSettings as TDiscuzSettings;
end;

function TDiscuz.LoadSettings;
begin
  Result := inherited LoadSettings(AWebsiteData);
  with DiscuzSettings do
  begin
    if Assigned(AWebsiteData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function TDiscuz.DoBuildLoginRequest;
begin
  Result := True;

  // POST /logging.php?action=login&loginsubmit=yes&inajax=1 HTTP/1.1

  AHTTPRequest := THTTPRequest.Create(Website + 'logging.php?action=login&loginsubmit=true');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := DiscuzSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('referer', ''); // ADDED
    AddFormField('cookietime', '2592000'); // ADDED

    AddFormField('loginfield', 'username');
    AddFormField('username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('questionid', '0');
    AddFormField('answer', '');
    AddFormField('userlogin', '');
    AddFormField('loginsubmit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDiscuz.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('action=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<h1><\/h1>\s+<p>(.*?)<';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDiscuz.NeedPrePost;
begin
  Result := True;
  if PostReply then
    ARequestURL := Website + 'post.php?action=reply&fid=' + VarToStr(DiscuzSettings.forums) + '&tid=' + VarToStr(DiscuzSettings.threads)
  else
    ARequestURL := Website + 'post.php?action=newthread&fid=' + VarToStr(DiscuzSettings.forums);
end;

function TDiscuz.DoAnalyzePrePost;
begin
  Result := True;
  // TODO: Überprüfung ob der User überhaupt Rechte hat, korrekt eingeloggt wurde
end;

function TDiscuz.DoBuildPostRequest;
var
  RequestURL: string;
begin
  Result := True;

  // &extra=page%3D1 NO

  if PostReply then
    RequestURL := Website + 'post.php?action=reply&replysubmit=yes&fid=' + VarToStr(DiscuzSettings.forums) + '&tid=' + VarToStr(DiscuzSettings.threads)
  else
    RequestURL := Website + 'post.php?action=newthread&topicsubmit=yes&fid=' + VarToStr(DiscuzSettings.forums);

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    // Referer := Website + 'post.php?action=newthread&fid=' + VarToStr(DiscuzSettings.forums);
    CharSet := DiscuzSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        InputString := APrevResponse;
        Expression := 'id="formhash" value="(.*?)"';

        if Exec(InputString) then
          AddFormField('formhash', Match[1]);
      finally
        Free;
      end;

    AddFormField('isblog', '');
    AddFormField('frombbs', '1');

    AddFormField('localid[]', '');
    AddFormField('attachperm[]', '0');
    AddFormField('attachdesc[]', '');
    AddFormField('readperm', '0');

    AddFormField('subject', Subject);
    AddFormField('message', Message);
    AddFormField('tags', Tags);

    if PostReply then
      AddFormField('fid', VarToStr(DiscuzSettings.forums));

    AddFormField('iconid', VarToStr(DiscuzSettings.icon));

    with DiscuzSettings do
    begin
      if htmlon then
        AddFormField('htmlon', '1')
      else
        AddFormField('htmlon', '0');
      if allowimgcode then
        AddFormField('allowimgcode', 'on');
      if parseurloff then
        AddFormField('parseurloff', '1');
      if smileyoff then
        AddFormField('smileyoff', '1');
      if bbcodeoff then
        AddFormField('bbcodeoff', '1');
      if tagoff then
        AddFormField('tagoff', '1');
      if usesig then
        AddFormField('usesig', '1');
      if addtoblog then
        AddFormField('addtoblog', '1');
    end;

    if PostReply then
      AddFormField('replysubmit', '')
    else
      AddFormField('topicsubmit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDiscuz.DoAnalyzePost;
begin
  Result := not((Pos('http-equiv="refresh" content="3', AResponseStr) = 0) and (Pos('window.location.href', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'message">.*?<p>(.*?)<';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDiscuz.DoAnalyzeIDsRequest;
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
      Expression := 'optgroup label="(.*?)"(.*?)<\/optgroup';

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
                Expression := 'option.*? value="(\d+)">([&nbsp; gt]*)(.*?)<\/';

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

function TDiscuz.GetName;
begin
  Result := 'Discuz!';
end;

function TDiscuz.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TDiscuz.BelongsTo;
begin
  Result := (Pos('logging.php?action=login', string(AWebsiteSourceCode)) > 0) { or (Pos('member.php?mod=logging&action=login',
    string(AWebsiteSourceCode)) > 0) } ;
end;

end.
