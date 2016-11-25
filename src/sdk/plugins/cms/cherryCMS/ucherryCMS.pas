unit ucherryCMS;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInHTTPClasses;

type
  TcherryCMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fcustom_login_url, fcustom_upload_url: string;
  public
    constructor Create; override;
  published
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    property use_plainlinks;
    property use_textasdescription;

    property categorys;
  end;

  TcherryCMS = class(TCMSFormbasedPlugIn)
  private
    cherryCMSSettings: TcherryCMSSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

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

{ TcherryCMSSettings }

constructor TcherryCMSSettings.Create;
begin
  inherited Create;

  // default setup
  custom_login_url := '?surf=login';
  custom_upload_url := '?surf=addupload';
end;

{ TcherryCMS }

function TcherryCMS.SettingsClass;
begin
  Result := TcherryCMSSettings;
end;

function TcherryCMS.GetSettings;
begin
  Result := cherryCMSSettings;
end;

procedure TcherryCMS.SetSettings;
begin
  cherryCMSSettings := ACMSPlugInSettings as TcherryCMSSettings;
end;

function TcherryCMS.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with cherryCMSSettings do
  begin
    if (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TcherryCMS.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php' + cherryCMSSettings.custom_login_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := cherryCMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('username', AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('stayloggedin', '1');
    AddFormField('send', 'a');
    AddFormField('send', 'Einloggen');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TcherryCMS.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<td colspan="2" style="text-align:center; color:#FF0000;">(.*?)<\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TcherryCMS.DoBuildPostRequest;
const
  languages: array [0 .. 5] of string = ('', 'GER', 'ENG', '', 'FRE', 'SPA');
var
  I, J: Integer;
  HosterList: string;
  _count: Integer;

  FormatSettings: TFormatSettings;
begin
  Result := True;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  AHTTPRequest := THTTPRequest.Create(Website + 'index.php' + cherryCMSSettings.custom_upload_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := cherryCMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    AddFormField('title', Subject);

    AddFormField('childgroup', cherryCMSSettings.categorys);

    if Assigned(AData.FindControl(cGenre)) then
      AddFormField('genre', AData.FindControl(cGenre).Value);

    if Assigned(AData.FindControl(cReleaseDate)) then
      AddFormField('year', FormatDateTime('yyyy', StrToDate(AData.FindControl(cReleaseDate).Value, FormatSettings), FormatSettings));

    if not cherryCMSSettings.use_textasdescription then
    begin
      if Assigned(AData.FindControl(cDescription)) then
        AddFormField('description', AData.FindControl(cDescription).Value)
    end
    else
      AddFormField('description', Message);

    if Assigned(AData.FindControl(cLanguage)) then
    begin
      if (IndexText(AData.FindControl(cLanguage).Value, languages) = -1) then
        AddFormField('language', '3')
      else
        AddFormField('language', IntToStr(IndexText(AData.FindControl(cLanguage).Value, languages)));
    end;

    for I := 0 to AData.MirrorCount - 1 do
      if AData.Mirror[I].Size > 0 then
      begin
        AddFormField('size', FloatToStr(AData.Mirror[I].Size));
        break;
      end;

    HosterList := '';
    for I := 0 to AData.MirrorCount - 1 do
    begin
      HosterList := HosterList + AData.Mirror[I].Hoster;
      if not(I = AData.MirrorCount - 1) then
        HosterList := HosterList + ', ';
    end;
    AddFormField('fdlhoster', HosterList);

    if Assigned(AData.FindControl(cPicture)) then
      AddFormField('coverlink', AData.FindControl(cPicture).Value);

    if Assigned(AData.FindControl(cNFO)) then
      AddFormField('nfo', AData.FindControl(cNFO).Value, 'nfo.nfo');

    if Assigned(AData.FindControl(cPassword)) then
      AddFormField('password', AData.FindControl(cPassword).Value);

    if Assigned(AData.FindControl(cSample)) then
      AddFormField('sample', AData.FindControl(cSample).Value);

    _count := 0;
    for I := 0 to AData.MirrorCount - 1 do

    begin
      if cherryCMSSettings.use_plainlinks and (AData.Mirror[I].DirectlinkCount > 0) then
      begin
        with TStringList.Create do
          try
            Text := AData.Mirror[I].Directlink[0].Value;

            for J := 0 to Count - 1 do
              AddFormField('url[' + IntToStr(_count) + '][]', Strings[J]);

            if _count = 0 then
              AddFormField('parts', IntToStr(Count));
          finally
            Free;
          end;
      end
      else if not cherryCMSSettings.use_plainlinks and (AData.Mirror[I].CrypterCount > 0) then
      begin
        for J := 0 to AData.Mirror[I].CrypterCount - 1 do
        begin
          AddFormField('url[' + IntToStr(_count) + '][]', AData.Mirror[I].Crypter[J].Value);

          if _count = 0 then
            AddFormField('parts', IntToStr(AData.Mirror[I].CrypterCount));
        end;
      end
      else
      begin
        ErrorMsg := 'No directlink or crypter initialized! (enable use_plainlinks with a directlink for each mirror or disable use_plainlinks and add a crypter)';
        Result := False;
      end;

      AddFormField('hoster[' + IntToStr(_count) + ']', AData.Mirror[I].Hoster);

      Inc(_count);
    end;

    AddFormField('mirrors', IntToStr(_count));

    AddFormField('comment', '');

    AddFormField('send', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TcherryCMS.DoAnalyzePost;
begin
  Result := not(Pos('<div style="margin: 5px 10px;">', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<td colspan="2" style="text-align:center; color:#FF0000;">(.*?)<\/td>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TcherryCMS.GetIDsRequestURL;
begin
  Result := Website + 'index.php' + cherryCMSSettings.custom_upload_url;
end;

function TcherryCMS.DoAnalyzeIDsRequest;
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
      InputString := ExtractTextBetween(AResponseStr, 'name="childgroup"', '</select>');
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
                Expression := 'option.*? value="(\d+)">(.*?)<\/';

                if Exec(InputString) then
                begin
                  repeat
                    // need +1 because the TOP-category has been added already
                    // there are no sub-sub-categories -> always 1
                    BoardLevelIndex := 1;

                    if (BoardLevelIndex = BoardLevel.Count) then
                      BoardLevel.Add(CleanPathName(Match[2]))
                    else
                    begin
                      repeat
                        BoardLevel.Delete(BoardLevel.Count - 1);
                      until (BoardLevelIndex = BoardLevel.Count);
                      BoardLevel.Add(CleanPathName(Match[2]));
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

function TcherryCMS.GetName;
begin
  Result := 'cherryCMS';
end;

function TcherryCMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TcherryCMS.BelongsTo;
begin
  Result := (Pos('name="str" value="Suche..." class="inputtext"', string(AWebsiteSourceCode)) > 0);
end;

function TcherryCMS.GetArticleLink;
begin
  // TODO:
  Result := Format('%s?id=%d', [AURL, AArticleID]);
end;

end.
