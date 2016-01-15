unit uuCMS;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, Math,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInHTTPClasses;

type
  TuCMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fextra_login, fneed_captcha, fhidden, funchecked, ferror_report, fprotected, foxygen_scene_com_special: Boolean;
    fcustom_login_url, fcustom_upload_url, fdescription_format: string;
  public
    constructor Create; override;
  published
    property extra_login: Boolean read fextra_login write fextra_login;
    property need_captcha: Boolean read fneed_captcha write fneed_captcha;
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url; // special rule with extra_login
    property description_format: string read fdescription_format write fdescription_format;

    property use_plainlinks;
    property use_textasdescription;
    property hidden: Boolean read fhidden write fhidden;
    property unchecked: Boolean read funchecked write funchecked;
    property error_report: Boolean read ferror_report write ferror_report;
    property protected: Boolean read fprotected write fprotected;
    property oxygen_scene_com_special: Boolean read foxygen_scene_com_special write foxygen_scene_com_special;

    property categorys;
  end;

  TuCMS = class(TCMSFormbasedPlugIn)
  private
    uCMSSettings: TuCMSSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;

    function NeedBeforePostAction: Boolean; override;
    function DoBeforePostAction(var ARequestID: Double): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TuCMSSettings }

constructor TuCMSSettings.Create;
begin
  inherited Create;

  // default setup
  extra_login := False;
  need_captcha := False;
  custom_login_url := '?p=userarea&location=login';
  custom_upload_url := '';
  description_format := '1';

  hidden := False;
  unchecked := False;
  error_report := False;
  fprotected := True;
  oxygen_scene_com_special := False;
end;

{ TuCMS }

function TuCMS.SettingsClass;
begin
  Result := TuCMSSettings;
end;

function TuCMS.GetSettings;
begin
  Result := uCMSSettings;
end;

procedure TuCMS.SetSettings;
begin
  uCMSSettings := ACMSPlugInSettings as TuCMSSettings;
end;

function TuCMS.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with uCMSSettings do
  begin
    if Assigned(AData) and (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;

    if SameStr('', custom_upload_url) then
    begin
      if extra_login then
        custom_upload_url := '?p=userarea&location=uploads'
      else
        custom_upload_url := '?p=upload'
    end;
  end;
end;

function TuCMS.NeedPreLogin;
begin
  Result := uCMSSettings.need_captcha;
  ARequestURL := Website + uCMSSettings.custom_login_url;
end;

function TuCMS.DoBuildLoginRequest;

var
  _captcha_id_c, _captcha_id_t: string;
  _captcha, _cookies: WideString;
begin
  Result := True;
{$REGION 'need_captcha'}
  if uCMSSettings.need_captcha then
  begin
    with TRegExpr.Create do
      try
        InputString := APrevResponse;
        Expression := 'TYPE="HIDDEN" NAME="c" VALUE="(.*?)"';

        if Exec(InputString) then
          _captcha_id_c := Match[1];

        Expression := 'TYPE="HIDDEN" NAME="t" VALUE="(.*?)"';

        if Exec(InputString) then
          _captcha_id_t := Match[1];
      finally
        Free;
      end;

    sleep(2000);

    if not CAPTCHAInput(Website, Subject, Website + 'gfx/secure/index.php?captcha=' + _captcha_id_c, GetName, _captcha, _cookies) then
    begin
      ErrorMsg := StrAbortedThrougthCAP;
      Result := False;
    end;
  end;
{$ENDREGION}
  AHTTPRequest := THTTPRequest.Create(Website + uCMSSettings.custom_login_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := uCMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('email', AccountName);
    AddFormField('password', AccountPassword);
    if uCMSSettings.need_captcha then
    begin
      AddFormField('code', _captcha);
      AddFormField('c', _captcha_id_c);
      AddFormField('t', _captcha_id_t);
    end;
    AddFormField('action', 'Login');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TuCMS.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('location=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<SPAN CLASS="ERROR">(.*?)<\/SPAN>';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := HTML2Text(Match[1]);
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

function TuCMS.DoBuildPostRequest;

  function oxygen_scene_com_hoster_list: string;
  var
    MirrorIndex: Integer;
  begin
    Result := '';

    for MirrorIndex := 0 to Max(4, AData.MirrorCount) - 1 do
    begin
      Result := Result + AData.Mirror[MirrorIndex].HosterShort;

      if not(MirrorIndex = AData.MirrorCount - 1) then
        Result := Result + ',';
    end;
    while Result[length(Result)] = ',' do
      Delete(Result, length(Result), 1);
  end;

const
  DownloadArray: array [0 .. 3] of string = ('download', 'mirror1', 'mirror2', 'mirror3');

var
  FormatSettings: TFormatSettings;

  I: Integer;

  _captcha, _cookies: WideString;
  _last_id, _captcha_id_c, _captcha_id_t: string;
begin
  Result := True;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := ',';

  AHTTPRequest := THTTPRequest.Create(Website + uCMSSettings.custom_upload_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := uCMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
{$REGION 'need_captcha'}
    if ((not uCMSSettings.extra_login) and uCMSSettings.need_captcha) or (Pos('TYPE="HIDDEN" NAME="c" VALUE=', APrevResponse) > 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := APrevResponse;

          Expression := 'TYPE="HIDDEN" NAME="c" VALUE="(.*?)"';
          if Exec(InputString) then
            _captcha_id_c := Match[1];

          Expression := 'TYPE="HIDDEN" NAME="t" VALUE="(.*?)"';
          if Exec(InputString) then
            _captcha_id_t := Match[1];
        finally
          Free;
        end;
      end;

      if not CAPTCHAInput(Website, Subject, Website + 'gfx/secure/index.php?captcha=' + _captcha_id_c, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;
    end;
{$ENDREGION}
    if uCMSSettings.extra_login then
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'NAME="last_id" VALUE="(.*?)"';

          if Exec(InputString) then
            _last_id := Match[1];
        finally
          Free;
        end;

    if uCMSSettings.extra_login then
      AddFormField('category_id', uCMSSettings.categorys)
    else
    begin
      if uCMSSettings.need_captcha then
      begin
        AddFormField('code', _captcha);
        AddFormField('c', _captcha_id_c);
        AddFormField('t', _captcha_id_t);
      end;
    end;

    AddFormField('category', VarToStr(uCMSSettings.categorys));

    AddFormField('uploader', AccountName);

    if Assigned(AData.FindControl(cPassword)) then
      AddFormField('password', AData.FindControl(cPassword).Value);

    AddFormField('title', Subject);

    if uCMSSettings.oxygen_scene_com_special then
      AddFormField('release_title', oxygen_scene_com_hoster_list)
    else if Assigned(AData.FindControl(cReleaseName)) then
      AddFormField('release_title', AData.FindControl(cReleaseName).Value);

    if Assigned(AData.FindControl(cReleaseDate)) then
      AddFormField('release_year', FormatDateTime('yyyy', StrToDate(AData.FindControl(cReleaseDate).Value, FormatSettings), FormatSettings));

    if Assigned(AData.FindControl(cCreator)) then
      AddFormField('interpreter', AData.FindControl(cCreator).Value);

    for I := 0 to AData.MirrorCount - 1 do
      if AData.Mirror[I].Size > 0 then
      begin
        AddFormField('size', FloatToStr(AData.Mirror[I].Size, FormatSettings));
        break;
      end;

    if Assigned(AData.FindControl(cRuntime)) then
      AddFormField('playtime', AData.FindControl(cRuntime).Value);

    if Assigned(AData.FindControl(cGenre)) then
      AddFormField('genre', AData.FindControl(cGenre).Value);

    if Assigned(AData.FindControl(cLanguage)) then
      AddFormField('language', AData.FindControl(cLanguage).Value);

    if Assigned(AData.FindControl(cVideoCodec)) then
      AddFormField('filetype', AData.FindControl(cVideoCodec).Value)
    else if AData.TypeID in [cAudio] then
      AddFormField('filetype', 'MP3')
    else if AData.TypeID in [cOther] then
      AddFormField('filetype', 'PDF')
    else
      AddFormField('filetype', 'ISO');

    if Assigned(AData.FindControl(cVideoStream)) then
      AddFormField('source', AData.FindControl(cVideoStream).Value)
    else if AData.TypeID in [cAudio] then
      // "LAME @ 128 kBit/s VBR, 48 kHz"
      // IAudioEncoder @ IAudioBitrate IAudioBitrateType, IAudioSamplingRate
      ;

    if Assigned(AData.FindControl(cPicture)) then
      AddFormField('thumb_url', AData.FindControl(cPicture).Value);

    if Assigned(AData.FindControl(cPicture)) and uCMSSettings.oxygen_scene_com_special then
      AddFormField('image_url', AData.FindControl(cPicture).Value)
    else
      AddFormField('image_url', '');

    AddFormField('preview_url', '');

    if Assigned(AData.FindControl(cSample)) then
      AddFormField('sample_url', AData.FindControl(cSample).Value)
    else
      AddFormField('sample_url', '');

    if not uCMSSettings.use_textasdescription then
    begin
      if Assigned(AData.FindControl(cDescription)) then
        AddFormField('description', AData.FindControl(cDescription).Value);
    end
    else
      AddFormField('description', Message);

    if Assigned(AData.FindControl(cNFO)) and not uCMSSettings.oxygen_scene_com_special then
      AddFormField('nfo', AData.FindControl(cNFO).Value);

    // max 4 mirrors
    for I := 0 to Min(4, AData.MirrorCount) - 1 do
    begin

      if uCMSSettings.use_plainlinks then
        AddFormField(DownloadArray[I], AData.Mirror[I].Directlink[0].Value)
      else if (AData.Mirror[I].CrypterCount > 0) then
        AddFormField(DownloadArray[I], AData.Mirror[I].Crypter[0].Value)
      else
      begin
        ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
        Result := False;
      end;

      if uCMSSettings.extra_login then
        AddFormField(DownloadArray[I] + '_hoster', AData.Mirror[I].Hoster);
    end;

    if not(AccountName = '') then
    begin
      AddFormField('user', AccountName);
      AddFormField('pass', AccountPassword);
    end
    else
    begin
      AddFormField('user', '');
      AddFormField('pass', '');
    end;

    if uCMSSettings.extra_login then
    begin
      AddFormField('format', uCMSSettings.description_format);

      if uCMSSettings.hidden then
        AddFormField('flags[hidden]', '1')
      else
        AddFormField('flags[hidden]', '0');
      if uCMSSettings.unchecked then
        AddFormField('flags[unchecked]', '1')
      else
        AddFormField('flags[unchecked]', '0');
      if uCMSSettings.error_report then
        AddFormField('flags[error_report]', '1')
      else
        AddFormField('flags[error_report]', '0');
      if uCMSSettings.protected then
        AddFormField('flags[protected]', '1')
      else
        AddFormField('flags[protected]', '0');
    end;

    if uCMSSettings.extra_login then
    begin
      AddFormField('id', _last_id);
      AddFormField('last_id', _last_id);
      AddFormField('action', 'Ändern');
    end
    else
      AddFormField('action', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TuCMS.DoAnalyzePost;
begin
  Result := not(Pos('<SPAN CLASS="SUCCESS">', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<SPAN CLASS="ERROR">(.*?)<\/SPAN>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TuCMS.GetIDsRequestURL;
begin
  Result := Website + '?q=&e=0';
end;

function TuCMS.DoAnalyzeIDsRequest;
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
        InputString := AResponseStr;
        Expression := 'OPTION VALUE="(\d+)">([&nbsp;]*)- (.*?)<\/';

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

function TuCMS.NeedBeforePostAction: Boolean;
begin
  Result := True
end;

function TuCMS.DoBeforePostAction(var ARequestID: Double): Boolean;
var
  HTTPParams: IHTTPParams;

  HTTPProcess: IHTTPProcess;

  HTTPError: Boolean;
begin
  Result := True;
  if uCMSSettings.extra_login then
  begin
    HTTPParams := THTTPParams.Create;
    with HTTPParams do
      AddFormField('action', 'Neu');

    ARequestID := HTTPManager.Post(Website + uCMSSettings.custom_upload_url, ARequestID, HTTPParams, TPlugInHTTPOptions.Create(Self));

    repeat
      sleep(50);
    until HTTPManager.HasResult(ARequestID);

    HTTPProcess := HTTPManager.GetResult(ARequestID);

    HTTPError := HTTPProcess.HTTPResult.HasError;

    if HTTPError then
    begin
      ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
      Result := False;
    end;
  end
  else if ((not uCMSSettings.extra_login) and uCMSSettings.need_captcha) then
  begin
    ARequestID := HTTPManager.Get(THTTPRequest.Create(Website + uCMSSettings.custom_upload_url), TPlugInHTTPOptions.Create(Self));

    repeat
      sleep(50);
    until HTTPManager.HasResult(ARequestID);

    HTTPProcess := HTTPManager.GetResult(ARequestID);

    HTTPError := HTTPProcess.HTTPResult.HasError;

    if HTTPError then
    begin
      ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
      Result := False;
    end;
  end;
end;

function TuCMS.GetName;
begin
  Result := 'uCMS';
end;

function TuCMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TuCMS.BelongsTo;
begin
  Result := (Pos('onSubmit="window.setTimeout(''DisableForm(\''''+this.name+''\'');'', 1); return(true);"', string(AWebsiteSourceCode)) > 0);
end;

function TuCMS.GetArticleLink;
begin
  // TODO:
  Result := Format('%s?id=%d', [AURL, AArticleID]);
end;

end.
