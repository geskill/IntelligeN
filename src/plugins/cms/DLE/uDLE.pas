unit uDLE;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols, IdHTTP,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

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
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    procedure PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function TDLE.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, DLESettings, AComponentController);
  with DLESettings do
  begin
    if Assigned(AComponentController) and SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TDLE.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('login_name=' + AccountName);
        Add('login_password=' + AccountPassword);
        Add('login=submit');
      end;

      Request.CharSet := DLESettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website, Params, Enc);
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

    if (Pos('action=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '(''dle-info''>|class="infobg">)(.*?)(<div id=''dle-content''>|<\/table>)';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Match[2]);
        finally
          Free;
        end;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TDLE.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string);
begin
  with AIdHTTPHelper do
  begin
    try
      AResponse := Get(Website + DLESettings.custom_upload_url);
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('addnews', AResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AResponse;
          Expression := '''dle-info''>(.*?)<div id=''dle-content''>';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Match[1]);
        finally
          Free;
        end;
      Exit;
    end;
  end;
end;

function TDLE.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;

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
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I, HosterCount: Integer;
  ShortHosterList: string;

  _captcha, _cookies: WideString;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
{$REGION 'CAPTCHA'}
      if (Pos('name="sec_code"', APrevResponse) > 0) then
      begin
        _cookies := CookieList;
        if not CAPTCHAInput(Website + 'engine/modules/antibot.php', GetName, _captcha, _cookies) then
        begin
          ErrorMsg := StrAbortedThrougthCAP;
          Exit;
        end;
        CookieList := _cookies;

        Params.Add('sec_code=' + _captcha);
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
                Params.Add(Match[1] + '=' + Match[2]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
      with Params do
      begin
        Add('title=' + Subject);

        Add('catlist[]=' + VarToStr(DLESettings.categorys));

        if Assigned(AComponentController.FindControl(cDescription)) then
          Add('short_story=' + GetShortText(message));

        if DLESettings.noshort then
          Add('full_story=' + GetFullText(message))
        else
          Add('full_story=' + StringReplace(message, DLESettings.stop_mark, '', []));

        Add('tags=' + Tags);

        with DLESettings do
        begin
          if approve then
            Add('approve=1');
          if allow_main then
            Add('allow_main=1');
          if allow_comm then
            Add('allow_comm=1');
          if allow_rating then
            Add('allow_rating=1');
        end;

        if DLESettings.dbase_ws_special then
        begin
          Add('xfield[releasename]=' + AComponentController.FindControl(cReleaseName).Value);

          if Assigned(AComponentController.FindControl(cPicture)) then
            Add('xfield[cover1]=' + AComponentController.FindControl(cPicture).Value)
          else
            Add('xfield[cover1]=');

          for I := 0 to AMirrorController.MirrorCount - 1 do
            if AMirrorController.Mirror[I].Size > 0 then
            begin
              Add('xfield[groesse]=' + FloatToStr(AMirrorController.Mirror[I].Size) + ' MB');
              break;
            end;

          if Assigned(AComponentController.FindControl(cPassword)) then
            Add('xfield[passwort]=' + AComponentController.FindControl(cPassword).Value)
          else
            Add('xfield[passwort]=');

          HosterCount := AMirrorController.MirrorCount;

          if (HosterCount = 1) then
          begin
            Add('xfield[hoster]=' + AMirrorController.Mirror[0].Hoster);
          end
          else if (HosterCount > 1) and (HosterCount < 4) then
          begin
            ShortHosterList := '';
            for I := 0 to AMirrorController.MirrorCount - 1 do
            begin
              ShortHosterList := ShortHosterList + AMirrorController.Mirror[I].GetHoster(True);
              if not(I = AMirrorController.MirrorCount - 1) then
                ShortHosterList := ShortHosterList + ' / ';
            end;
            Add('xfield[hoster]=' + ShortHosterList);
          end
          else
            Add('xfield[hoster]=Multi');

          if Assigned(AComponentController.FindControl(cLanguage)) then
          begin
            if not(Pos(';', AComponentController.FindControl(cLanguage).Value) = 0) then
              Add('xfield[sprache]=3')
            else
              case IndexText(AComponentController.FindControl(cLanguage).Value, ['GER', 'ENG']) of
                0:
                  Add('xfield[sprache]=1');
                1:
                  Add('xfield[sprache]=2');
              else
                Add('xfield[sprache]=4');
              end;
          end
          else
            Add('xfield[sprache]=4');

          for I := 0 to AMirrorController.MirrorCount - 1 do
            if AMirrorController.Mirror[I].Size > 0 then
            begin
              Add('xfield[parts]=' + IntToStr(AMirrorController.Mirror[I].Parts));
              break;
            end;

          if Assigned(AComponentController.FindControl(cRuntime)) then
            Add('xfield[laufzeit]=' + AComponentController.FindControl(cRuntime).Value)
          else
            Add('xfield[laufzeit]=');

          if Assigned(AComponentController.FindControl(cAudioBitrate)) then
            Add('xfield[bitrate]=' + AComponentController.FindControl(cAudioBitrate).Value)
          else
            Add('xfield[bitrate]=');

          if Assigned(AComponentController.FindControl(cAudioStream)) then
            Add('xfield[audiostream]=' + AComponentController.FindControl(cAudioStream).Value)
          else
            Add('xfield[audiostream]=');

          if Assigned(AComponentController.FindControl(cVideoStream)) then
            Add('xfield[videostream]=' + AComponentController.FindControl(cVideoStream).Value)
          else
            Add('xfield[videostream]=');

          if Assigned(AComponentController.FindControl(cVideoCodec)) then
            Add('xfield[codec]=' + AComponentController.FindControl(cVideoCodec).Value)
          else
            Add('xfield[codec]=');

          Add('xfield[linkmulti]=');

          for I := 0 to AMirrorController.MirrorCount - 1 do
          begin
            if DLESettings.use_plainlinks then
            begin
              if (AMirrorController.Mirror[I].DirectlinksMirrorCount > 0) then
                Add('xfield[' + LowerCase(StringReplace(ChangeFileExt(AMirrorController.Mirror[I].Hoster, ''), '-', '', [rfReplaceAll]))
                    + ']=' + AMirrorController.Mirror[I].DirectlinksMirror[0])
            end
            else
            begin
              if (AMirrorController.Mirror[I].CrypterCount > 0) then
                Add('xfield[' + LowerCase(StringReplace(ChangeFileExt(AMirrorController.Mirror[I].Hoster, ''), '-', '', [rfReplaceAll]))
                    + ']=' + AMirrorController.Mirror[I].Crypter[0].Link)
              else
              begin
                ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
                Result := False;
              end;
            end;
          end;

          if Assigned(AComponentController.FindControl(cTrailer)) then
            Add('xfield[trailer]=' + AComponentController.FindControl(cTrailer).Value)
          else
            Add('xfield[trailer]=');

          if Assigned(AComponentController.FindControl(cNFO)) then
            Add('xfield[nfo]=' + AComponentController.FindControl(cNFO).Value)
          else
            Add('xfield[nfo]=');
        end;

        Add('bbfont=0');
        Add('bbsize=0');
        Add('tags=');
        Add('mod=addnews');
        Add('add=');
      end;

      Request.Referer := Website + DLESettings.custom_upload_url;
      Request.CharSet := DLESettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + DLESettings.custom_upload_url, Params, Enc);
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

      if not(Pos('javascript:history', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := '(''dle-info''>|class="infobg">|class="errors")(.*?)(<div id=''dle-content''>|<\/table>)';

            if Exec(InputString) then
              Self.ErrorMsg := HTML2Text(Match[2]);
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

function TDLE.GetName;
begin
  Result := 'DLE';
end;

function TDLE.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function TDLE.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('js/dle_ajax.js', string(AWebsiteSourceCode)) > 0) or (Pos('dle_login_hash', string(AWebsiteSourceCode)) > 0);
end;

function TDLE.GetIDs: Integer;
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
    IdHTTPHelper.Request.Referer := Website;

    DLESettings := TDLESettings.Create;
    try
      LoadSettings;

      if not(AccountName = '') then
        if not Login(IdHTTPHelper) then
          Exit;

      PrePostPage(IdHTTPHelper, ResponseStr);
    finally
      DLESettings.Free;
    end;
  finally
    IdHTTPHelper.Free;
  end;

  BoardLevel := TStringList.Create;
  try
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(ResponseStr, 'name="catlist[]"', '</select>');
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
        end;
      finally
        Free;
      end;
  finally
    BoardLevel.Free;
  end;
  Result := FCheckedIDsList.Count;
end;

function TDLE.Exec;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;
begin
  Result := False;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    // IdHTTPHelper.HTTPOptions := IdHTTPHelper.HTTPOptions - [hoForceEncodeParams];

    DLESettings := TDLESettings.Create;
    try
      LoadSettings(ComponentController);

      if not(AccountName = '') then
        if not Login(IdHTTPHelper) then
          Exit;

      PrePostPage(IdHTTPHelper, ResponseStr);

      Result := PostPage(IdHTTPHelper, ComponentController, MirrorController, ResponseStr);
    finally
      DLESettings.Free;
    end;
  finally
    IdHTTPHelper.Free;
  end;
end;

function TDLE.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TDLESettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
