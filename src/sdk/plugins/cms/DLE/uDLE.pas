unit uDLE;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInHTTPClasses;

type
  TDLESettings = class(TCMSBlogPlugInSettings)
  strict private
    fcategory_field, fstop_mark, fcustom_upload_url: string;

    fnoshort, fapprove, fallow_main, fallow_comm, fallow_rating, fdbase_ws_special, fuse_plainlinks: Boolean;
  published
    [AttrDefaultValue('catlist[]')]
    property category_field: string read fcategory_field write fcategory_field;

    [AttrDefaultValue('#readmore#')]
    property stop_mark: string read fstop_mark write fstop_mark;
    [AttrDefaultValue('addnews.html')]
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    [AttrDefaultValue(False)]
    property noshort: Boolean read fnoshort write fnoshort;
    [AttrDefaultValue(False)]
    property approve: Boolean read fapprove write fapprove;
    [AttrDefaultValue(True)]
    property allow_main: Boolean read fallow_main write fallow_main;
    [AttrDefaultValue(True)]
    property allow_comm: Boolean read fallow_comm write fallow_comm;
    [AttrDefaultValue(True)]
    property allow_rating: Boolean read fallow_rating write fallow_rating;
    [AttrDefaultValue(False)]
    property dbase_ws_special: Boolean read fdbase_ws_special write fdbase_ws_special;
    [AttrDefaultValue(False)]
    property use_plainlinks: Boolean read fuse_plainlinks write fuse_plainlinks;

    property categorys;
  end;

  TDLE = class(TCMSBlogPlugIn)
  private
    DLESettings: TDLESettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

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

function TDLE.SettingsClass;
begin
  Result := TDLESettings;
end;

function TDLE.GetSettings;
begin
  Result := DLESettings;
end;

procedure TDLE.SetSettings;
begin
  DLESettings := ACMSPlugInSettings as TDLESettings;
end;

function TDLE.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with DLESettings do
  begin
    if (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TDLE.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := DLESettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('login_name', AccountName);
    AddFormField('login_password', AccountPassword);
    AddFormField('login', 'submit');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDLE.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not(Pos('action=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '(''dle-info''>|class="infobg">)(.*?)(<div id=''dle-content''>|<\/table>)';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[2]);
      finally
        Free;
      end;
end;

function TDLE.NeedPrePost;
begin
  Result := True;
  ARequestURL := Website + DLESettings.custom_upload_url;
end;

function TDLE.DoAnalyzePrePost;
begin
  Result := not(Pos('addnews', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '''dle-info''>(.*?)<div id=''dle-content''>';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[1]);
      finally
        Free;
      end;
end;

function TDLE.DoBuildPostRequest;

  function GetShortText(ALongText: string): string;
  var
    _Offset: Integer;
  begin
    _Offset := Pos(DLESettings.stop_mark, ALongText);
    if not(_Offset = 0) then
      Result := copy(ALongText, 1, _Offset - 1)
    else
      Result := ALongText;
  end;

  function GetFullText(ALongText: string): string;
  var
    _Offset: Integer;
  begin
    _Offset := Pos(DLESettings.stop_mark, ALongText);
    if not(_Offset = 0) then
      Result := copy(ALongText, _Offset + length(DLESettings.stop_mark))
    else
      Result := ALongText;
  end;

var
  I, HosterCount: Integer;
  ShortHosterList: string;

  _captcha, _cookies: WideString;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + DLESettings.custom_upload_url);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := DLESettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
{$REGION 'CAPTCHA'}
    if (Pos('name="sec_code"', APrevResponse) > 0) then
    begin
      _cookies := HTTPManager.GetResult(APrevRequest).HTTPResult.HTTPResponse.Cookies.Text;
      if not CAPTCHAInput(Website + 'engine/modules/antibot.php', GetName, _captcha, _cookies) then
      begin
        ErrorMsg := StrAbortedThrougthCAP;
        Result := False;
      end;
      // CookieList := _cookies;
      AHTTPRequest.Cookies.Text := _cookies;

      AddFormField('sec_code', _captcha);
    end;
{$ENDREGION}
    with TRegExpr.Create do
      try
        InputString := APrevResponse;
        Expression := '<input type="hidden" name="([^"]*?)" value="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            if (IndexText(Match[1], ['mod', 'do', 'subaction']) = -1) then
              AddFormField(Match[1], Match[2]);
          until not ExecNext;
        end;
      finally
        Free;
      end;

    AddFormField('title', Subject);

    AddFormField(DLESettings.category_field, VarToStr(DLESettings.categorys));

    AddFormField('short_story', GetShortText(message));

    if DLESettings.noshort then
      AddFormField('full_story', GetFullText(message))
    else
      AddFormField('full_story', StringReplace(message, DLESettings.stop_mark, '', []));

    AddFormField('tags', Tags);

    with DLESettings do
    begin
      if approve then
        AddFormField('approve', '1');
      if allow_main then
        AddFormField('allow_main', '1');
      if allow_comm then
        AddFormField('allow_comm', '1');
      if allow_rating then
        AddFormField('allow_rating', '1');
    end;

    if DLESettings.dbase_ws_special then
    begin
      AddFormField('xfield[releasename]', AData.FindControl(cReleaseName).Value);

      if Assigned(AData.FindControl(cPicture)) then
        AddFormField('xfield[cover1]', AData.FindControl(cPicture).Value)
      else
        AddFormField('xfield[cover1]', '');

      for I := 0 to AData.MirrorCount - 1 do
        if AData.Mirror[I].Size > 0 then
        begin
          AddFormField('xfield[groesse]', FloatToStr(AData.Mirror[I].Size) + ' MB');
          break;
        end;

      if Assigned(AData.FindControl(cPassword)) then
        AddFormField('xfield[passwort]', AData.FindControl(cPassword).Value)
      else
        AddFormField('xfield[passwort]', '');

      HosterCount := AData.MirrorCount;

      if (HosterCount = 1) then
      begin
        AddFormField('xfield[hoster]', AData.Mirror[0].Hoster);
      end
      else if (HosterCount > 1) and (HosterCount < 4) then
      begin
        ShortHosterList := '';
        for I := 0 to AData.MirrorCount - 1 do
        begin
          ShortHosterList := ShortHosterList + AData.Mirror[I].HosterShort;
          if not(I = AData.MirrorCount - 1) then
            ShortHosterList := ShortHosterList + ' / ';
        end;
        AddFormField('xfield[hoster]', ShortHosterList);
      end
      else
        AddFormField('xfield[hoster]', 'Multi');

      if Assigned(AData.FindControl(cLanguage)) then
      begin
        if not(Pos(';', AData.FindControl(cLanguage).Value) = 0) then
          AddFormField('xfield[sprache]', '3')
        else
          case IndexText(AData.FindControl(cLanguage).Value, ['GER', 'ENG']) of
            0:
              AddFormField('xfield[sprache]', '1');
            1:
              AddFormField('xfield[sprache]', '2');
          else
            AddFormField('xfield[sprache]', '4');
          end;
      end
      else
        AddFormField('xfield[sprache]', '4');

      for I := 0 to AData.MirrorCount - 1 do
        if AData.Mirror[I].Size > 0 then
        begin
          AddFormField('xfield[parts]', IntToStr(AData.Mirror[I].Parts));
          break;
        end;

      if Assigned(AData.FindControl(cRuntime)) then
        AddFormField('xfield[laufzeit]', AData.FindControl(cRuntime).Value)
      else
        AddFormField('xfield[laufzeit]', '');

      if Assigned(AData.FindControl(cAudioBitrate)) then
        AddFormField('xfield[bitrate]', AData.FindControl(cAudioBitrate).Value)
      else
        AddFormField('xfield[bitrate]', '');

      if Assigned(AData.FindControl(cAudioStream)) then
        AddFormField('xfield[audiostream]', AData.FindControl(cAudioStream).Value)
      else
        AddFormField('xfield[audiostream]', '');

      if Assigned(AData.FindControl(cVideoStream)) then
        AddFormField('xfield[videostream]', AData.FindControl(cVideoStream).Value)
      else
        AddFormField('xfield[videostream]', '');

      if Assigned(AData.FindControl(cVideoCodec)) then
        AddFormField('xfield[codec]', AData.FindControl(cVideoCodec).Value)
      else
        AddFormField('xfield[codec]', '');

      AddFormField('xfield[linkmulti]', '');

      for I := 0 to AData.MirrorCount - 1 do
      begin
        if DLESettings.use_plainlinks then
        begin
          if (AData.Mirror[I].DirectlinkCount > 0) then
            AddFormField('xfield[' + LowerCase(StringReplace(ChangeFileExt(AData.Mirror[I].Hoster, ''), '-', '', [rfReplaceAll])) + ']', AData.Mirror[I].Directlink[0].Value)
        end
        else
        begin
          if (AData.Mirror[I].CrypterCount > 0) then
            AddFormField('xfield[' + LowerCase(StringReplace(ChangeFileExt(AData.Mirror[I].Hoster, ''), '-', '', [rfReplaceAll])) + ']', AData.Mirror[I].Crypter[0].Value)
          else
          begin
            ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
            Result := False;
          end;
        end;
      end;

      if Assigned(AData.FindControl(cTrailer)) then
        AddFormField('xfield[trailer]', AData.FindControl(cTrailer).Value)
      else
        AddFormField('xfield[trailer]', '');

      if Assigned(AData.FindControl(cNFO)) then
        AddFormField('xfield[nfo]', AData.FindControl(cNFO).Value)
      else
        AddFormField('xfield[nfo]', '');
    end;

    AddFormField('bbfont', '0');
    AddFormField('bbsize', '0');
    AddFormField('mod', 'addnews');
    AddFormField('add', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TDLE.DoAnalyzePost;
begin
  Result := (Pos('javascript:history', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '(''dle-info''>|class="infobg">|class="errors"|class="short_story")(.*?)(<div id=''dle-content''>|<\/table>)';

        if Exec(InputString) then
          Self.ErrorMsg := HTML2Text(Match[2]);
      finally
        Free;
      end;
end;

function TDLE.GetIDsRequestURL;
begin
  NeedPrePost(Result);
end;

function TDLE.DoAnalyzeIDsRequest;
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
        InputString := ExtractTextBetween(AResponseStr, 'name="catlist[]"', '</select>');
        Expression := 'option.*? value="(\d+)" >([&nbsp;]*)(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            BoardLevelIndex := CharCount('&nbsp;', Match[2]);

            if BoardLevelIndex > 0 then
              BoardLevelIndex := BoardLevelIndex div 3;

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
        end
        else if (Pos('dtree', AResponseStr) > 0) then
        begin
          // MOD: Category Tree: i.e.: http://www.byhero.com/addnews.html

          /// Need ; in StringReplace because TOP category ends also with </span><span style="margin:2px"> but has no ; before
          InputString := StringReplace(AResponseStr, ';</span><span style="margin:2px">', ';', [rfReplaceAll, rfIgnoreCase]);
          Expression := '(<span style="margin:2px">[&nbsp;]*|<label).*?name="category\[\]"  value="(\d+)" \/>(.*?)<\/label>';

          if Exec(InputString) then
          begin
            repeat
              BoardLevelIndex := CharCount('&nbsp;', Match[1]);

              if BoardLevelIndex > 0 then
                BoardLevelIndex := BoardLevelIndex div 5;

              if (BoardLevelIndex = BoardLevel.Count) then
                BoardLevel.Add(CleanPathName(Match[3]))
              else
              begin
                repeat
                  BoardLevel.Delete(BoardLevel.Count - 1);
                until (BoardLevelIndex = BoardLevel.Count);
                BoardLevel.Add(CleanPathName(Match[3]));
              end;

              AddID(Match[2], IDPath(BoardLevel));
            until not ExecNext;
          end;
        end;
      finally
        Free;
      end;
  finally
    BoardLevel.Free;
  end;
  Result := FCheckedIDsList.Count;
end;

function TDLE.GetName;
begin
  Result := 'DLE';
end;

function TDLE.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TDLE.BelongsTo;
begin
  Result := (Pos('js/dle_ajax.js', string(AWebsiteSourceCode)) > 0) or (Pos('dle_login_hash', string(AWebsiteSourceCode)) > 0);
end;

end.
