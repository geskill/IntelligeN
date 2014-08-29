unit uuCMS;

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
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TuCMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fextra_login, fneed_captcha, fhidden, funchecked, ferror_report, fprotected, foxygen_scene_com_special: Boolean;
    fcustom_login_url, fcustom_upload_url, fdescription_format: string;

  published
    [AttrDefaultValue(False)]
    property extra_login: Boolean read fextra_login write fextra_login;
    [AttrDefaultValue(False)]
    property need_captcha: Boolean read fneed_captcha write fneed_captcha;
    [AttrDefaultValue('?p=userarea&location=login')]
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    [AttrDefaultValue('')] // special rule with extra_login
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;
    [AttrDefaultValue('1')]
    property description_format: string read fdescription_format write fdescription_format;

    [AttrDefaultValue('')]
    property hoster_blacklist;
    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;
    [AttrDefaultValue(False)]
    property hidden: Boolean read fhidden write fhidden;
    [AttrDefaultValue(False)]
    property unchecked: Boolean read funchecked write funchecked;
    [AttrDefaultValue(False)]
    property error_report: Boolean read ferror_report write ferror_report;
    [AttrDefaultValue(True)]
    property protected: Boolean read fprotected write fprotected;
    [AttrDefaultValue(False)]
    property oxygen_scene_com_special: Boolean read foxygen_scene_com_special write foxygen_scene_com_special;

    property categorys;
  end;

  TuCMS = class(TCMSFormbasedPlugIn)
  private
    uCMSSettings: TuCMSSettings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    procedure PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function TuCMS.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, uCMSSettings, AComponentController);
  with uCMSSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AComponentController) and (categorys = null) then
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

function TuCMS.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  _captcha_id_c, _captcha_id_t: string;
  _captcha, _cookies: WideString;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
{$REGION 'need_captcha'}
    if uCMSSettings.need_captcha then
    begin
      try
        ResponseStr := Get(Website + uCMSSettings.custom_login_url);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
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

      sleep(2000);

      if not CAPTCHAInput(Website + 'gfx/secure/index.php?captcha=' + _captcha_id_c, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Exit;
      end;
    end;
{$ENDREGION}
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('email=' + AccountName);
        Add('password=' + AccountPassword);
        if uCMSSettings.need_captcha then
        begin
          Add('code=' + _captcha);
          Add('c=' + _captcha_id_c);
          Add('t=' + _captcha_id_t);
        end;
        Add('action=Login');
      end;

      Request.CharSet := uCMSSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + uCMSSettings.custom_login_url, Params, Enc);
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

    if (Pos('location=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStr;
          Expression := '<SPAN CLASS="ERROR">(.*?)<\/SPAN>';

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
      end;

      Exit;
    end;
  end;
  Result := True;
end;

procedure TuCMS.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
var
  Params: TStringList;
  Enc: TEncoding;
begin
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      Params.Add('action=Neu');

      Request.CharSet := uCMSSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          AResponse := Post(Website + uCMSSettings.custom_upload_url, Params, Enc);
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

function TuCMS.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;

  function oxygen_scene_com_hoster_list: string;
  var
    MirrorIndex, MirrorIndexedCount: Integer;
  begin
    Result := '';
    MirrorIndexedCount := 0;
    for MirrorIndex := 0 to AMirrorController.MirrorCount - 1 do
      if (Pos(string(AMirrorController.Mirror[MirrorIndex].Hoster), uCMSSettings.hoster_blacklist) = 0) then
      begin
        if MirrorIndexedCount = 4 then
          break;

        Result := Result + AMirrorController.Mirror[MirrorIndex].GetHoster(True);

        if not(MirrorIndex = AMirrorController.MirrorCount - 1) and not(MirrorIndexedCount = 3) then
          Result := Result + ',';
        Inc(MirrorIndexedCount);
      end;
    if Result[length(Result)] = ',' then
      Delete(Result, length(Result), 1);
  end;

const
  DownloadArray: array [0 .. 3] of string = ('download', 'mirror1', 'mirror2', 'mirror3');
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  FormatSettings: TFormatSettings;

  I: Integer;

  _captcha, _cookies: WideString;
  _need_captcha_later: Boolean;
  _last_id, _captcha_id_c, _captcha_id_t, _need_captcha_later_sourcecode: string;
  _count: Integer;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := ',';

  Result := False;
  with AIdHTTPHelper do
  begin
    _need_captcha_later := False;
    if uCMSSettings.extra_login then
    begin

      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'NAME="last_id" VALUE="(.*?)"';

          if Exec(InputString) then
            _last_id := Match[1];
        finally
          Free;
        end;

      _need_captcha_later := Pos('TYPE="HIDDEN" NAME="c" VALUE=', APrevResponse) > 0;
      _need_captcha_later_sourcecode := APrevResponse;
    end;
{$REGION 'need_captcha'}
    if ((not uCMSSettings.extra_login) and uCMSSettings.need_captcha) or _need_captcha_later then
    begin
      if not _need_captcha_later then
        try
          _need_captcha_later_sourcecode := Get(Website + uCMSSettings.custom_upload_url);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;

      with TRegExpr.Create do
      begin
        try
          InputString := _need_captcha_later_sourcecode;
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

      if not CAPTCHAInput(Website + 'gfx/secure/index.php?captcha=' + _captcha_id_c, GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Exit;
      end;
    end;
{$ENDREGION}
    Params := TStringList.Create;
    try
      with Params do
      begin
        if uCMSSettings.extra_login then
          Add('category_id=' + uCMSSettings.categorys)
        else
        begin
          if uCMSSettings.need_captcha then
          begin
            Add('code=' + _captcha);
            Add('c=' + _captcha_id_c);
            Add('t=' + _captcha_id_t);
          end;
        end;

        Add('category=' + uCMSSettings.categorys);

        Add('uploader=' + AccountName);

        if Assigned(AComponentController.FindControl(cPassword)) then
          Add('password=' + AComponentController.FindControl(cPassword).Value);

        Add('title=' + Subject);

        if uCMSSettings.oxygen_scene_com_special then
          Add('release_title=' + oxygen_scene_com_hoster_list)
        else if Assigned(AComponentController.FindControl(cReleaseName)) then
          Add('release_title=' + AComponentController.FindControl(cReleaseName).Value);

        if Assigned(AComponentController.FindControl(cReleaseDate)) then
          Add('release_year=' + FormatDateTime('yyyy', StrToDate(AComponentController.FindControl(cReleaseDate).Value, FormatSettings), FormatSettings));

        if Assigned(AComponentController.FindControl(cArtist)) then
          Add('interpreter=' + AComponentController.FindControl(cArtist).Value);

        for I := 0 to AMirrorController.MirrorCount - 1 do
          if AMirrorController.Mirror[I].Size > 0 then
          begin
            Add('size=' + FloatToStr(AMirrorController.Mirror[I].Size, FormatSettings));
            break;
          end;

        if Assigned(AComponentController.FindControl(cRuntime)) then
          Add('playtime=' + AComponentController.FindControl(cRuntime).Value);

        if Assigned(AComponentController.FindControl(cGenre)) then
          Add('genre=' + AComponentController.FindControl(cGenre).Value);

        if Assigned(AComponentController.FindControl(cLanguage)) then
          Add('language=' + AComponentController.FindControl(cLanguage).Value);

        if Assigned(AComponentController.FindControl(cVideoCodec)) then
          Add('filetype=' + AComponentController.FindControl(cVideoCodec).Value)
        else if AComponentController.TemplateTypeID in [cAudio] then
          Add('filetype=MP3')
        else if AComponentController.TemplateTypeID in [cOther] then
          Add('filetype=PDF')
        else
          Add('filetype=ISO');

        if Assigned(AComponentController.FindControl(cVideoStream)) then
          Add('source=' + AComponentController.FindControl(cVideoStream).Value);

        if Assigned(AComponentController.FindControl(cPicture)) then
          Add('thumb_url=' + AComponentController.FindControl(cPicture).Value);

        if Assigned(AComponentController.FindControl(cPicture)) and uCMSSettings.oxygen_scene_com_special then
          Add('image_url=' + AComponentController.FindControl(cPicture).Value)
        else
          Add('image_url=');

        Add('preview_url=');

        if Assigned(AComponentController.FindControl(cSample)) then
          Add('sample_url=' + AComponentController.FindControl(cSample).Value)
        else
          Add('sample_url=');

        if not uCMSSettings.use_textasdescription then
        begin
          if Assigned(AComponentController.FindControl(cDescription)) then
            Add('description=' + AComponentController.FindControl(cDescription).Value);
        end
        else
          Add('description=' + Message);

        if Assigned(AComponentController.FindControl(cNFO)) and not uCMSSettings.oxygen_scene_com_special then
          Add('nfo=' + AComponentController.FindControl(cNFO).Value);

        _count := 0;
        for I := 0 to AMirrorController.MirrorCount - 1 do
          if (Pos(string(AMirrorController.Mirror[I].Hoster), uCMSSettings.hoster_blacklist) = 0) then
          begin
            if _count = 4 then
              break;

            if uCMSSettings.use_plainlinks then
              Add(DownloadArray[_count] + '=' + AMirrorController.Mirror[I].DirectlinksMirror[0])
            else if (AMirrorController.Mirror[I].CrypterCount > 0) then
              Add(DownloadArray[_count] + '=' + AMirrorController.Mirror[I].Crypter[0].Link)
            else
            begin
              ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
              Exit;
            end;

            if uCMSSettings.extra_login then
              Add(DownloadArray[_count] + '_hoster=' + AMirrorController.Mirror[I].Hoster);

            Inc(_count);
          end;

        if not(AccountName = '') then
        begin
          Add('user=' + AccountName);
          Add('pass=' + AccountPassword);
        end
        else
        begin
          Add('user=');
          Add('pass=');
        end;

        if uCMSSettings.extra_login then
        begin
          Add('format=' + uCMSSettings.description_format);

          if uCMSSettings.hidden then
            Add('flags[hidden]=1')
          else
            Add('flags[hidden]=0');
          if uCMSSettings.unchecked then
            Add('flags[unchecked]=1')
          else
            Add('flags[unchecked]=0');
          if uCMSSettings.error_report then
            Add('flags[error_report]=1')
          else
            Add('flags[error_report]=0');
          if uCMSSettings.protected then
            Add('flags[protected]=1')
          else
            Add('flags[protected]=0');
        end;

        if uCMSSettings.extra_login then
        begin
          Add('id=' + _last_id);
          Add('last_id=' + _last_id);
          Add('action=Ändern');
        end
        else
          Add('action=');
      end;

      Request.CharSet := uCMSSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + uCMSSettings.custom_upload_url, Params, Enc);
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
      {
        Response.RawHeaders.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Response.txt');
        Params.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Params.htm');
        ReplyData.SaveToFile(ExtractFilePath(ParamStr(0)) + 'ReplyData.htm');
        }

      if (Pos('<SPAN CLASS="SUCCESS">', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := '<SPAN CLASS="ERROR">(.*?)<\/SPAN>';

            if Exec(InputString) then
            begin
              Self.ErrorMsg := HTML2Text(Match[1]);
              Exit;
            end;
          finally
            Free;
          end;

        Exit;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor TuCMS.Create;
begin
  inherited Create;
  uCMSSettings := TuCMSSettings.Create;
end;

destructor TuCMS.Destroy;
begin
  uCMSSettings.Free;
  inherited Destroy;
end;

function TuCMS.GetName;
begin
  Result := 'uCMS';
end;

function TuCMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TuCMS.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('onSubmit="window.setTimeout(''DisableForm(\''''+this.name+''\'');'', 1); return(true);"', string(AWebsiteSourceCode)) > 0);
end;

function TuCMS.GetIDs: Integer;
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
    try
      ResponseStr := IdHTTPHelper.Get(Website + '?q=&e=0');
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
        InputString := ResponseStr;
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

function TuCMS.Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean;
var
  IdHTTPHelper: TIdHTTPHelper;

  ResponseStr: string;
begin
  Result := False;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    if LoadSettings(ComponentController) then
    begin
{$REGION 'extra_login'}
      if uCMSSettings.extra_login then
      begin
        if not Login(IdHTTPHelper) then
          Exit;

        PrePostPage(IdHTTPHelper, ResponseStr);
      end;
{$ENDREGION}
      Result := PostPage(IdHTTPHelper, ComponentController, MirrorController, ResponseStr);
    end;
  finally
    IdHTTPHelper.Free;
  end;
end;

function TuCMS.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TuCMSSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
