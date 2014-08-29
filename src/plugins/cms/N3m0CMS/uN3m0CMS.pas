unit uN3m0CMS;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uReleasenameUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TN3m0CMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fround_size: Boolean;
  published
    [AttrDefaultValue('')]
    property hoster_blacklist;
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
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string = ''): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

{ TN3m0CMS }

function TN3m0CMS.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, N3m0CMSSettings, AComponentController);
  with N3m0CMSSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AComponentController) and (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TN3m0CMS.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    // for getting a PHPSESSID
    Get(Website + 'index.php');

    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('benutzername=' + AccountName);
        Add('passwort=' + AccountPassword);
        Add('login=Login');
      end;

      Request.CharSet := N3m0CMSSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php', Params, Enc);
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

    if (Pos('?logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          ModifierG := False;
          InputString := ResponseStr;
          Expression := '<p>(.*?)<br';

          if Exec(InputString) then
            Self.ErrorMsg := Trim(HTML2Text(Match[1]));
        finally
          Free;
        end;
      Exit;
    end;
  end;
  Result := True;
end;

function TN3m0CMS.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string): Boolean;

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
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  FormatSettings: TFormatSettings;

  I: Integer;

  HosterPos: Integer;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        case AComponentController.TemplateTypeID of
          cAudio:
            begin
              Add('Kategorie=Musik');

              if MatchText('VA%', AComponentController.FindControl(cReleaseName).Value) or MatchText('%TOP%', AComponentController.FindControl(cReleaseName).Value) then
                Add('submusik=Sampler')
              else if MatchText('%CDM%', AComponentController.FindControl(cReleaseName).Value) then
                Add('submusik=Maxi')
              else if MatchText('%CDS%', AComponentController.FindControl(cReleaseName).Value) then
                Add('submusik=Songs')
              else
                Add('submusik=Alben');

              Add('Genre=' + N3m0CMSSettings.categorys);
            end;
          cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360:
            begin
              Add('Kategorie=Games');
              case AComponentController.TemplateTypeID of
                cNintendoDS:
                  Add('subgames=NDS');
                cPCGames:
                  Add('subgames=PC');
                cPlayStation2:
                  Add('subgames=PS2');
                cPlayStation3:
                  Add('subgames=PS3');
                cPlayStationPortable:
                  Add('subgames=PSP');
                cWii:
                  Add('subgames=Wii');
                cXbox:
                  Add('subgames=XBox');
                cXbox360:
                  Add('subgames=XBox360');
              end;
            end;
          cMovie:
            begin
              if TReleasenameUtils.IsSeries(AComponentController.FindControl(cReleaseName).Value) then
                Add('Kategorie=Serien')
              else
              begin
                Add('Kategorie=Movies');
                Add('mGenre=' + N3m0CMSSettings.categorys);
              end;
            end;
          cSoftware:
            Add('Kategorie=Appz');
          cXXX:
            begin
              Add('Kategorie=XXX');

              if MatchText('%.iMAGESET%', AComponentController.FindControl(cReleaseName).Value) then
                Add('subxxx=Pictures')
              else if MatchText('%.XXX.%', AComponentController.FindControl(cReleaseName).Value) then
                Add('subxxx=Movies')
              else
                Add('subxxx=Clips');

              Add('xGenre=' + N3m0CMSSettings.categorys);
            end;
          cOther:
            Add('Kategorie=' + 'eBooks');
        end;

        Add('Titel=' + Subject);

        if Assigned(AComponentController.FindControl(cArtist)) then
          Add('Interpret=' + AComponentController.FindControl(cArtist).Value);

        if Assigned(AComponentController.FindControl(cAudioBitrate)) then
          Add('Bitrate=' + NumbersOnly(AComponentController.FindControl(cAudioBitrate).Value));

        if Assigned(AComponentController.FindControl(cLanguage)) then
        begin
          if not(Pos(';', AComponentController.FindControl(cLanguage).Value) = 0) then
            Add('Sprache=Multilanguage')
          else
            case IndexText(AComponentController.FindControl(cLanguage).Value, ['GER', 'ENG', 'JPN', 'RUS']) of
              0:
                Add('Sprache=Deutsch');
              1:
                Add('Sprache=Englisch');
              2:
                Add('Sprache=Japanisch');
              3:
                Add('Sprache=Russisch');
            end;
        end;

        if Assigned(AComponentController.FindControl(cReleaseDate)) then
          Add('Jahr=' + FormatDateTime('yyyy', StrToDateTimeDef((AComponentController.FindControl(cReleaseDate).Value), Now, FormatSettings), FormatSettings))
        else
          Add('Jahr=' + FormatDateTime('yyyy', Now, FormatSettings));

        if not N3m0CMSSettings.use_textasdescription then
        begin
          if Assigned(AComponentController.FindControl(cDescription)) then
            Add('Beschreibung=' + AComponentController.FindControl(cDescription).Value);
        end
        else
          Add('Beschreibung=' + Message);

        for I := 0 to AMirrorController.MirrorCount - 1 do
          if AMirrorController.Mirror[I].Size > 0 then
          begin
            if N3m0CMSSettings.round_size then
              Add('Groesse=' + IntToStr(round(AMirrorController.Mirror[I].Size)))
            else
              Add('Groesse=' + FloatToStr(AMirrorController.Mirror[I].Size));
            break;
          end;

        if Assigned(AComponentController.FindControl(cPassword)) then
          Add('Passwort=' + AComponentController.FindControl(cPassword).Value);

        if Assigned(AComponentController.FindControl(cPicture)) then
          Add('Cover=' + AComponentController.FindControl(cPicture).Value);

        Add('Mirror=' + IntToStr(AMirrorController.MirrorCount));

        for I := 0 to AMirrorController.MirrorCount - 1 do
        begin
          if I = 10 then
            break;

          HosterPos := IndexText(AMirrorController.Mirror[I].Hoster, HosterArray);

          if HosterPos = -1 then
          begin
            Add('Hoster' + IntToStr(I + 1) + '=anderer Hoster');
            Add('AHoster' + IntToStr(I + 1) + '=' + AMirrorController.Mirror[I].Hoster);
          end
          else
            Add('Hoster' + IntToStr(I + 1) + '=' + HosterArray[HosterPos]);

          if N3m0CMSSettings.use_plainlinks then
            Add('Mirror' + IntToStr(I + 1) + '=' + Trim(AMirrorController.Mirror[I].DirectlinksMirror[0]))
          else if (AMirrorController.Mirror[I].CrypterCount > 0) then
            Add('Mirror' + IntToStr(I + 1) + '=' + AMirrorController.Mirror[I].Crypter[0].Link)
          else
          begin
            ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
            Exit;
          end;
        end;

        Add('submit=File Hinzufuegen');
      end;

      Request.CharSet := N3m0CMSSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'index.php?a=Hinzufuegen', Params, Enc);
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

      if not(Pos('select name="Kategorie" id="Kategorie"', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            ModifierG := False;
            InputString := ResponseStr;
            Expression := '<td><b>(.*?)<\/b><\/td>';

            if Exec(InputString) then
              Self.ErrorMsg := Trim(HTML2Text(Match[1]));
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

constructor TN3m0CMS.Create;
begin
  inherited Create;
  N3m0CMSSettings := TN3m0CMSSettings.Create;
end;

destructor TN3m0CMS.Destroy;
begin
  N3m0CMSSettings.Free;
  inherited Destroy;
end;

function TN3m0CMS.GetName: WideString;
begin
  Result := 'N3m0CMS';
end;

function TN3m0CMS.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TN3m0CMS.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('class="contenthead_inner"', string(AWebsiteSourceCode)) > 0);
end;

function TN3m0CMS.GetIDs: Integer;
begin
  Result := FCheckedIDsList.Count;
end;

function TN3m0CMS.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TN3m0CMSSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
