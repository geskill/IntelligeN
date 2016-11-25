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
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInHTTPClasses;

type
  TN3m0CMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fround_size: Boolean;
    fcustom_login_url, fcustom_upload_url: string;
  public
    constructor Create; override;
  published
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    property use_plainlinks;
    property use_textasdescription;

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
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function NeedPreLogin(out ARequestURL: string): Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TN3m0CMSSettings }

constructor TN3m0CMSSettings.Create;
begin
  inherited Create;

  // default setup
  custom_login_url := 'index.php';
  custom_upload_url := 'index.php?a=Hinzufuegen';
  round_size := False;
end;

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
  Result := inherited LoadSettings(AData);
  with N3m0CMSSettings do
  begin
    if Assigned(AData) and (AData.TypeID in [cAudio, cMovie, cXXX]) and (categorys = null) then
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
    case AData.TypeID of
      cAudio:
        begin
          AddFormField('Kategorie', 'Musik');

          if MatchTextMask('VA%', AData.FindControl(cReleaseName).Value) or MatchTextMask('%TOP%', AData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Sampler')
          else if MatchTextMask('%CDM%', AData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Maxi')
          else if MatchTextMask('%CDS%', AData.FindControl(cReleaseName).Value) then
            AddFormField('submusik', 'Songs')
          else
            AddFormField('submusik', 'Alben');

          AddFormField('Genre', N3m0CMSSettings.categorys);
        end;
      cNintendoDS, cPCGames, cPlayStation3, cPlayStation4, cWii, cXbox360, cXboxOne:
        begin
          AddFormField('Kategorie', 'Games');
          case AData.TypeID of
            cNintendoDS:
              AddFormField('subgames', 'NDS');
            cPCGames:
              AddFormField('subgames', 'PC');
            cPlayStation3:
              AddFormField('subgames', 'PS3');
            cPlayStation4:
              AddFormField('subgames', 'PS4');
            cWii:
              AddFormField('subgames', 'Wii');
            cXbox360:
              AddFormField('subgames', 'XBox360');
            cXboxOne:
              AddFormField('subgames', 'XBoxOne');
          end;
        end;
      cMovie:
        begin
          if TReleasenameUtils.IsSeries(AData.FindControl(cReleaseName).Value) then
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

          if MatchTextMask('%.iMAGESET%', AData.FindControl(cReleaseName).Value) then
            AddFormField('subxxx', 'Pictures')
          else if MatchTextMask('%.XXX.%', AData.FindControl(cReleaseName).Value) then
            AddFormField('subxxx', 'Movies')
          else
            AddFormField('subxxx', 'Clips');

          AddFormField('xGenre', N3m0CMSSettings.categorys);
        end;
      cOther:
        AddFormField('Kategorie', 'eBooks');
    end;

    AddFormField('Titel', Subject);

    if Assigned(AData.FindControl(cCreator)) then
      AddFormField('Interpret', AData.FindControl(cCreator).Value);

    if Assigned(AData.FindControl(cAudioBitrate)) then
      AddFormField('Bitrate', ExtractNumbers(AData.FindControl(cAudioBitrate).Value));

    if Assigned(AData.FindControl(cLanguage)) then
    begin
      if not(Pos(';', AData.FindControl(cLanguage).Value) = 0) then
        AddFormField('Sprache', 'Multilanguage')
      else
        case IndexText(AData.FindControl(cLanguage).Value, ['GER', 'ENG', 'JPN', 'RUS']) of
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

    if Assigned(AData.FindControl(cReleaseDate)) then
      AddFormField('Jahr', FormatDateTime('yyyy', StrToDateTimeDef((AData.FindControl(cReleaseDate).Value), Now, FormatSettings), FormatSettings))
    else
      AddFormField('Jahr', FormatDateTime('yyyy', Now, FormatSettings));

    if not N3m0CMSSettings.use_textasdescription then
    begin
      if Assigned(AData.FindControl(cDescription)) then
        AddFormField('Beschreibung', AData.FindControl(cDescription).Value);
    end
    else
      AddFormField('Beschreibung', Message);

    for I := 0 to AData.MirrorCount - 1 do
      if AData.Mirror[I].Size > 0 then
      begin
        if N3m0CMSSettings.round_size then
          AddFormField('Groesse', IntToStr(round(AData.Mirror[I].Size)))
        else
          AddFormField('Groesse', FloatToStr(AData.Mirror[I].Size));
        break;
      end;

    if Assigned(AData.FindControl(cPassword)) then
      AddFormField('Passwort', AData.FindControl(cPassword).Value);

    if Assigned(AData.FindControl(cPicture)) then
      AddFormField('Cover', AData.FindControl(cPicture).Value);

    AddFormField('Mirror', IntToStr(AData.MirrorCount));

    // max 10 mirrors
    for I := 0 to Min(10, AData.MirrorCount) - 1 do
    begin
      HosterPos := IndexText(AData.Mirror[I].Hoster, HosterArray);

      if HosterPos = -1 then
      begin
        AddFormField('Hoster' + IntToStr(I + 1), 'anderer Hoster');
        AddFormField('AHoster' + IntToStr(I + 1), AData.Mirror[I].Hoster);
      end
      else
        AddFormField('Hoster' + IntToStr(I + 1), HosterArray[HosterPos]);

      if N3m0CMSSettings.use_plainlinks and (AData.Mirror[I].DirectlinkCount > 0) then
        AddFormField('Mirror' + IntToStr(I + 1), Trim(AData.Mirror[I].Directlink[0].Value))
      else if not N3m0CMSSettings.use_plainlinks and (AData.Mirror[I].CrypterCount > 0) then
        AddFormField('Mirror' + IntToStr(I + 1), AData.Mirror[I].Crypter[0].Value)
      else
      begin
        ErrorMsg := 'No directlink or crypter initialized! (enable use_plainlinks with a directlink for each mirror or disable use_plainlinks and add a crypter)';
        Result := False;
      end;
    end;

    AddFormField('submit', 'File Hinzufuegen');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TN3m0CMS.DoAnalyzePost;
begin
  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := 'action="(.*?)" method="post"';

      if Exec(InputString) then
      begin
        Result := False;

        if (Pos(N3m0CMSSettings.custom_upload_url, LowerCase(Match[1]))) = 0 then
        begin
          Self.ErrorMsg := 'You have no access rights to make a new entry!'
        end;

        with TRegExpr.Create do
          try
            InputString := AResponseStr;
            Expression := '<td><b>(.*?)<\/b><\/td>';

            if Exec(InputString) then
              Self.ErrorMsg := Trim(HTML2Text(Match[1]));
          finally
            Free;
          end;

      end
      else
      begin
        Result := True;
      end;
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

function TN3m0CMS.GetArticleLink;
begin
  // TODO:
  Result := Format('%s?id=%d', [AURL, AArticleID]);
end;

end.
