unit uHTW;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants, Math,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uStringUtils,
  // Common
  uConst, uWebsiteInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInHTTPClasses;

type
  THTWSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fcategory_field, fsubcategory_field, fcustom_upload_url: string;

    fsubcategorys: Variant;
  published
    [AttrDefaultValue('')]
    property category_field: string read fcategory_field write fcategory_field;
    [AttrDefaultValue('')]
    property subcategory_field: string read fsubcategory_field write fsubcategory_field;

    [AttrDefaultValue('index.php?do=add')]
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;

    property categorys;
    property subcategorys: Variant read fsubcategorys write fsubcategorys;
  end;

  THTW = class(TCMSFormbasedPlugIn)
  private
    HTWSettings: THTWSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AWebsiteData: ICMSWebsiteData = nil): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AWebsiteData: ICMSWebsiteData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
  end;

implementation

function THTW.SettingsClass;
begin
  Result := THTWSettings;
end;

function THTW.GetSettings;
begin
  Result := HTWSettings;
end;

procedure THTW.SetSettings;
begin
  HTWSettings := ACMSPlugInSettings as THTWSettings;
end;

function THTW.LoadSettings;
begin
  Result := inherited LoadSettings(AWebsiteData);
  with HTWSettings do
  begin
    if Assigned(AWebsiteData) then
    begin
      if (categorys = null) then
      begin
        ErrorMsg := 'category is undefined!';
        Result := False;
        Exit;
      end;

      if (subcategorys = null) then
      begin
        ErrorMsg := 'subcategory is undefined!';
        Result := False;
      end;
    end;
  end;
end;

function THTW.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php?do=login');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := HTWSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('user', AccountName);
    AddFormField('pwd', AccountPassword);
    AddFormField('submit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function THTW.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('http-equiv="refresh" content="3', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<br><center>(.*?)<\/center>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function THTW.DoBuildPostRequest;
const
  DownloadArrayA: array [0 .. 4] of string = ('dll1', 'dll2', 'dll3', 'dll4', 'dll5');
  DownloadArrayB: array [0 .. 4] of string = ('link', 'mirror1', 'mirror2', 'mirror3', 'mirror4');
  HosterArray: array [0 .. 4] of string = ('link_hoster', 'hoster1', 'hoster2', 'hoster3', 'hoster4');
  StatusArray: array [0 .. 4] of string = ('link_status', 'status1', 'status2', 'status3', 'status4');
var
  FormatSettings: TFormatSettings;
  I: Integer;
begin
  Result := True;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  AHTTPRequest := THTTPRequest.Create(Website + HTWSettings.custom_upload_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := HTWSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('name', Subject);
    AddFormField('title', Subject);

    AddFormField('uploader', AccountName);

    if Assigned(AWebsiteData.FindControl(cReleaseName)) then
      AddFormField('rls_name', AWebsiteData.FindControl(cReleaseName).Value);

    if Assigned(AWebsiteData.FindControl(cPassword)) then
    begin
      AddFormField('pw', AWebsiteData.FindControl(cPassword).Value);
      AddFormField('passwort', AWebsiteData.FindControl(cPassword).Value);
    end;

    if Assigned(AWebsiteData.FindControl(cPicture)) then
      AddFormField('cover', AWebsiteData.FindControl(cPicture).Value);

    AddFormField(VarToStr(HTWSettings.category_field), HTWSettings.categorys);
    AddFormField(VarToStr(HTWSettings.subcategory_field), HTWSettings.subcategorys);

    if Assigned(AWebsiteData.FindControl(cVideoCodec)) and (not(HTWSettings.category_field = 'format') and not(HTWSettings.subcategory_field = 'format')) then
      AddFormField('format', AWebsiteData.FindControl(cVideoCodec).Value);

    if Assigned(AWebsiteData.FindControl(cGenre)) and (not(HTWSettings.category_field = 'genre') and not(HTWSettings.subcategory_field = 'genre')) then
      AddFormField('genre', AWebsiteData.FindControl(cGenre).Value);

    if Assigned(AWebsiteData.FindControl(cAudioStream)) then
      AddFormField('qual_ton', AWebsiteData.FindControl(cAudioStream).Value);

    if Assigned(AWebsiteData.FindControl(cVideoSystem)) then
      AddFormField('qual_bild', AWebsiteData.FindControl(cVideoSystem).Value);

    if Assigned(AWebsiteData.FindControl(cReleaseDate)) then
      AddFormField('jahr', FormatDateTime('yyyy', StrToDate(AWebsiteData.FindControl(cReleaseDate).Value, FormatSettings), FormatSettings))
    else
      AddFormField('jahr', '');

    for I := 0 to AWebsiteData.MirrorCount - 1 do
      if AWebsiteData.Mirror[I].Size > 0 then
      begin
        AddFormField('groesse', FloatToStr(AWebsiteData.Mirror[I].Size));
        AddFormField('size', FloatToStr(AWebsiteData.Mirror[I].Size));
        break;
      end;

    if Assigned(AWebsiteData.FindControl(cLanguage)) then
    begin
      if not(Pos(';', AWebsiteData.FindControl(cLanguage).Value) = 0) then
        AddFormField('sprache', 'Multi (De, En, Fr, Ru)')
      else
        case IndexText(AWebsiteData.FindControl(cLanguage).Value, ['GER', 'ENG', 'FRE', 'RUS']) of
          0:
            AddFormField('sprache', 'Deutsch');
          1:
            AddFormField('sprache', 'Englisch');
          2:
            AddFormField('sprache', 'Französisch');
          3:
            AddFormField('sprache', 'Russisch');
        else
          AddFormField('sprache', '');
        end
    end
    else
      AddFormField('sprache', '');

    if not HTWSettings.use_textasdescription then
    begin
      if Assigned(AWebsiteData.FindControl(cDescription)) then
        AddFormField('beschreibung', AWebsiteData.FindControl(cDescription).Value)
    end
    else
      AddFormField('beschreibung', Message);

    // max 5 mirrors
    for I := 0 to Min(5, AWebsiteData.MirrorCount) - 1 do
    begin
      if HTWSettings.use_plainlinks then
      begin
        AddFormField(DownloadArrayA[I], AWebsiteData.Mirror[I].Directlink[0].Value);
        AddFormField(DownloadArrayB[I], AWebsiteData.Mirror[I].Directlink[0].Value);
      end
      else if (AWebsiteData.Mirror[I].CrypterCount > 0) then
      begin
        AddFormField(DownloadArrayA[I], AWebsiteData.Mirror[I].Crypter[0].Value);
        AddFormField(DownloadArrayB[I], AWebsiteData.Mirror[I].Crypter[0].Value);
      end
      else
      begin
        ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
        Result := False;
      end;
      AddFormField(HosterArray[I], AWebsiteData.Mirror[I].Hoster);
      AddFormField(StatusArray[I], '');
    end;

    AddFormField('sent', '1');
    AddFormField('submit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function THTW.DoAnalyzePost;
begin
  Result := (Pos('javascript:history.back()', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<br><center>(.*?)<br><br>';

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

function THTW.GetName;
begin
  Result := 'HTW';
end;

function THTW.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function THTW.BelongsTo;
begin
  Result := False;
end;

function THTW.GetIDs;
begin
  // TODO:
  Result := FCheckedIDsList.Count;
end;

end.
