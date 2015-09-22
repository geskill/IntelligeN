unit uN3m0CMS;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants, Math,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uReleasenameUtils, uStringUtils,
  // Common
  uConst, uWebsiteInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInHTTPClasses;

type
  TN3m0CMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fround_size: Boolean;
    fcustom_login_url, fcustom_upload_url: string;
  published
    [AttrDefaultValue('index.php')]
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    [AttrDefaultValue('index.php?a=Hinzufuegen')]
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;

    [AttrDefaultValue(False)]
    property round_size: Boolean read fround_size write fround_size;

    property categorys;
  end;

  TN3m0CMS = class(TCMSFormbasedPlugIn)
  private
    N3m0CMSSettings: TN3m0CMSSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AWebsiteData: ICMSWebsiteData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
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

{ TN3m0CMS }

function TN3m0CMS.SettingsClass;
begin
  Result := TN3m0CMSSettings;
end;

function TN3m0CMS.GetSettings;
begin
  Result := N3m0CMSSettings;
end;

procedure TN3m0CMS.SetSettings;
begin
  N3m0CMSSettings := ACMSPlugInSettings as TN3m0CMSSettings;
end;

function TN3m0CMS.LoadSettings;
begin
  Result := inherited LoadSettings(AWebsiteData);
  with N3m0CMSSettings do
  begin
    if Assigned(AWebsiteData) and (AWebsiteData.TemplateTypeID in [cAudio, cMovie, cXXX]) and (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TN3m0CMS.NeedPreLogin;
begin
  Result := True;
  ARequestURL := Website + N3m0CMSSettings.custom_login_url;
end;

function TN3m0CMS.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + N3m0CMSSettings.custom_login_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := N3m0CMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('username', AccountName);
    AddFormField('benutzername', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('passwort', AccountPassword);
    AddFormField('login', 'Login');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TN3m0CMS.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('?logout', AResponseStr) = 0) and (Pos('logout/', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        ModifierG := False;
        InputString := AResponseStr;
        Expression := '<p>(.*?)<br';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]));
      finally
        Free;
      end;
end;

function TN3m0CMS.DoBuildPostRequest;

  function NumbersOnly(s: string): string;
  var
    X: Integer;
  begin
    for X := length(s) downto 1 do
      if not(s[X] in ['0' .. '9']) then
        Delete(s, X, 1);
    Result := s;
  end;

const
  HosterArray: array [0 .. 7] of string = ('Shragle.com', 'Depositfiles.com', 'Rapidshare.com', 'Uploaded.to', 'Share-Online.biz', 'Netload.in', 'Sharebase.to', 'x7.to');
var
  FormatSettings: TFormatSettings;

  I: Integer;

  HosterPos: Integer;
begin
  Result := True;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  AHTTPRequest := THTTPRequest.Create(Website + N3m0CMSSettings.custom_upload_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := N3m0CMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    case AWebsiteData.TemplateTypeID of
      cAudio:
        begin
          AddFormField('Kategorie', 'Musik');

          if MatchTextMask('VA%', AWebsiteData.FindControl(cReleaseName).Value) or MatchTextMask('%TOP%', AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Sampler')
          else if MatchTextMask('%CDM%', AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Maxi')
          else if MatchTextMask('%CDS%', AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Songs')
          else
            AddFormField('submusik', 'Alben');

          AddFormField('Genre', N3m0CMSSettings.categorys);
        end;
      cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360:
        begin
          AddFormField('Kategorie', 'Games');
          case AWebsiteData.TemplateTypeID of
            cNintendoDS:
              AddFormField('subgames', 'NDS');
            cPCGames:
              AddFormField('subgames', 'PC');
            cPlayStation2:
              AddFormField('subgames', 'PS2');
            cPlayStation3:
              AddFormField('subgames', 'PS3');
            cPlayStationPortable:
              AddFormField('subgames', 'PSP');
            cWii:
              AddFormField('subgames', 'Wii');
            cXbox:
              AddFormField('subgames', 'XBox');
            cXbox360:
              AddFormField('subgames', 'XBox360');
          end;
        end;
      cMovie:
        begin
          if TReleasenameUtils.IsSeries(AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('Kategorie', 'Serien')
          else
          begin
            AddFormField('Kategorie', 'Movies');
            AddFormField('mGenre', N3m0CMSSettings.categorys);
          end;
        end;
      cSoftware:
        AddFormField('Kategorie', 'Appz');
      cXXX:
        begin
          AddFormField('Kategorie', 'XXX');

          if MatchTextMask('%.iMAGESET%', AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('subxxx', 'Pictures')
          else if MatchTextMask('%.XXX.%', AWebsiteData.FindControl(cReleaseName).Value) then
            AddFormField('subxxx', 'Movies')
          else
            AddFormField('subxxx', 'Clips');

          AddFormField('xGenre', N3m0CMSSettings.categorys);
        end;
      cOther:
        AddFormField('Kategorie', 'eBooks');
    end;

    AddFormField('Titel', Subject);

    if Assigned(AWebsiteData.FindControl(cArtist)) then
      AddFormField('Interpret', AWebsiteData.FindControl(cArtist).Value);

    if Assigned(AWebsiteData.FindControl(cAudioBitrate)) then
      AddFormField('Bitrate', NumbersOnly(AWebsiteData.FindControl(cAudioBitrate).Value));

    if Assigned(AWebsiteData.FindControl(cLanguage)) then
    begin
      if not(Pos(';', AWebsiteData.FindControl(cLanguage).Value) = 0) then
        AddFormField('Sprache', 'Multilanguage')
      else
        case IndexText(AWebsiteData.FindControl(cLanguage).Value, ['GER', 'ENG', 'JPN', 'RUS']) of
          0:
            AddFormField('Sprache', 'Deutsch');
          1:
            AddFormField('Sprache', 'Englisch');
          2:
            AddFormField('Sprache', 'Japanisch');
          3:
            AddFormField('Sprache', 'Russisch');
        end;
    end;

    if Assigned(AWebsiteData.FindControl(cReleaseDate)) then
      AddFormField('Jahr', FormatDateTime('yyyy', StrToDateTimeDef((AWebsiteData.FindControl(cReleaseDate).Value), Now, FormatSettings), FormatSettings))
    else
      AddFormField('Jahr', FormatDateTime('yyyy', Now, FormatSettings));

    if not N3m0CMSSettings.use_textasdescription then
    begin
      if Assigned(AWebsiteData.FindControl(cDescription)) then
        AddFormField('Beschreibung', AWebsiteData.FindControl(cDescription).Value);
    end
    else
      AddFormField('Beschreibung', Message);

    for I := 0 to AWebsiteData.MirrorCount - 1 do
      if AWebsiteData.Mirror[I].Size > 0 then
      begin
        if N3m0CMSSettings.round_size then
          AddFormField('Groesse', IntToStr(round(AWebsiteData.Mirror[I].Size)))
        else
          AddFormField('Groesse', FloatToStr(AWebsiteData.Mirror[I].Size));
        break;
      end;

    if Assigned(AWebsiteData.FindControl(cPassword)) then
      AddFormField('Passwort', AWebsiteData.FindControl(cPassword).Value);

    if Assigned(AWebsiteData.FindControl(cPicture)) then
      AddFormField('Cover', AWebsiteData.FindControl(cPicture).Value);

    AddFormField('Mirror', IntToStr(AWebsiteData.MirrorCount));

    // max 10 mirrors
    for I := 0 to Min(10, AWebsiteData.MirrorCount) - 1 do
    begin
      HosterPos := IndexText(AWebsiteData.Mirror[I].Hoster, HosterArray);

      if HosterPos = -1 then
      begin
        AddFormField('Hoster' + IntToStr(I + 1), 'anderer Hoster');
        AddFormField('AHoster' + IntToStr(I + 1), AWebsiteData.Mirror[I].Hoster);
      end
      else
        AddFormField('Hoster' + IntToStr(I + 1), HosterArray[HosterPos]);

      if N3m0CMSSettings.use_plainlinks then
        AddFormField('Mirror' + IntToStr(I + 1), Trim(AWebsiteData.Mirror[I].Directlink[0].Value))
      else if (AWebsiteData.Mirror[I].CrypterCount > 0) then
        AddFormField('Mirror' + IntToStr(I + 1), AWebsiteData.Mirror[I].Crypter[0].Value)
      else
      begin
        ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
        Result := False;
      end;
    end;

    AddFormField('submit', 'File Hinzufuegen');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TN3m0CMS.DoAnalyzePost;
begin
  Result := (Pos(N3m0CMSSettings.custom_upload_url, AResponseStr) = 0);
  if Result then
    ErrorMsg := 'You have no access rights to make a new entry!'
  else
    Result := (Pos('select name="kategorie"', LowerCase(AResponseStr)) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        ModifierG := False;
        InputString := AResponseStr;
        Expression := '<td><b>(.*?)<\/b><\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := Trim(HTML2Text(Match[1]));
      finally
        Free;
      end;
end;

function TN3m0CMS.GetName;
begin
  Result := 'N3m0CMS';
end;

function TN3m0CMS.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TN3m0CMS.BelongsTo;
begin
  Result := (Pos('class="contenthead_inner"', string(AWebsiteSourceCode)) > 0);
end;

function TN3m0CMS.GetIDs;
begin
  /// not necessary - categories will be set by default
  Result := FCheckedIDsList.Count;
end;

end.
