unit ucherryCMS;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TcherryCMSSettings = class(TCMSFormbasedPlugInSettings)
  strict private
    fcustom_login_url, fcustom_upload_url: string;
  published
    [AttrDefaultValue('?surf=login')]
    property custom_login_url: string read fcustom_login_url write fcustom_login_url;
    [AttrDefaultValue('?surf=addupload')]
    property custom_upload_url: string read fcustom_upload_url write fcustom_upload_url;

    [AttrDefaultValue('')]
    property hoster_blacklist;
    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;

    property categorys;
  end;

  TcherryCMS = class(TCMSFormbasedPlugIn)
  private
    cherryCMSSettings: TcherryCMSSettings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; override;
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

{ TcherryCMS }

function TcherryCMS.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, cherryCMSSettings, AComponentController);
  with cherryCMSSettings do
  begin
    if SameStr('', Charset) then
      Charset := DefaultCharset;

    if Assigned(AComponentController) and (categorys = null) then
    begin
      ErrorMsg := 'category is undefined!';
      Result := False;
    end;
  end;
end;

function TcherryCMS.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        AddFormField('username', AccountName, cherryCMSSettings.Charset).ContentTransfer := 'binary';
        AddFormField('password', AccountPassword, cherryCMSSettings.Charset).ContentTransfer := 'binary';
        AddFormField('stayloggedin', '1', cherryCMSSettings.Charset);
        AddFormField('send', 'a', cherryCMSSettings.Charset);
        AddFormField('send', 'Einloggen', cherryCMSSettings.Charset);
      end;

      Request.ContentType := Params.RequestContentType;
      try
        ResponseStr := Post(Website + 'index.php' + cherryCMSSettings.custom_login_url, Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

    finally
      Params.Free;
    end;

    if (Pos('=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<td colspan="2" style="text-align:center; color:#FF0000;">(.*?)<\/td>';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Match[1]);
        finally
          Free;
        end;

      Exit;
    end;
  end;
  Result := True;
end;

function TcherryCMS.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  languages: array [0 .. 5] of string = ('', 'GER', 'ENG', '', 'FRE', 'SPA');
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I, J: Integer;

  HosterList: string;

  NFOStringStream: TStringStream;

  _count: Integer;

  FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      NFOStringStream := TStringStream.Create('', TEncoding.ASCII);
      try
        with Params do
        begin
          AddFormField('title', Subject, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          AddFormField('childgroup', cherryCMSSettings.categorys, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cGenre)) then
            AddFormField('genre', AComponentController.FindControl(cGenre).Value, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cReleaseDate)) then
            AddFormField('year', FormatDateTime('yyyy', StrToDate(AComponentController.FindControl(cReleaseDate).Value, FormatSettings),
                FormatSettings), cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if not cherryCMSSettings.use_textasdescription then
          begin
            if Assigned(AComponentController.FindControl(cDescription)) then
              AddFormField('description', AComponentController.FindControl(cDescription).Value, cherryCMSSettings.Charset).ContentTransfer := 'binary'
          end
          else
            AddFormField('description', Message, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cLanguage)) then
          begin
            if (IndexText(AComponentController.FindControl(cLanguage).Value, languages) = -1) then
              AddFormField('language', '3', cherryCMSSettings.Charset)
            else
              AddFormField('language', IntToStr(IndexText(AComponentController.FindControl(cLanguage).Value, languages)), cherryCMSSettings.Charset);
          end;

          for I := 0 to AMirrorController.MirrorCount - 1 do
            if AMirrorController.Mirror[I].Size > 0 then
            begin
              AddFormField('size', FloatToStr(AMirrorController.Mirror[I].Size), cherryCMSSettings.Charset).ContentTransfer := 'binary';
              break;
            end;

          HosterList := '';
          for I := 0 to AMirrorController.MirrorCount - 1 do
            if (Pos(string(AMirrorController.Mirror[I].Hoster), cherryCMSSettings.hoster_blacklist) = 0) then
            begin
              HosterList := HosterList + AMirrorController.Mirror[I].Hoster;
              if not(I = AMirrorController.MirrorCount - 1) then
                HosterList := HosterList + ', ';
            end;
          AddFormField('fdlhoster', HosterList, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cPicture)) then
            AddFormField('coverlink', AComponentController.FindControl(cPicture).Value, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cNFO)) then
          begin
            NFOStringStream.WriteString(AComponentController.FindControl(cNFO).Value);
            AddObject('nfo', 'text/plain', cherryCMSSettings.Charset, NFOStringStream, 'nfo.nfo').ContentTransfer := 'binary';
          end;

          if Assigned(AComponentController.FindControl(cPassword)) then
            AddFormField('password', AComponentController.FindControl(cPassword).Value, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          if Assigned(AComponentController.FindControl(cSample)) then
            AddFormField('sample', AComponentController.FindControl(cSample).Value, cherryCMSSettings.Charset).ContentTransfer := 'binary';

          _count := 0;
          for I := 0 to AMirrorController.MirrorCount - 1 do
            if (Pos(string(AMirrorController.Mirror[I].Hoster), cherryCMSSettings.hoster_blacklist) = 0) then
            begin
              if cherryCMSSettings.use_plainlinks then
              begin
                with TStringList.Create do
                  try
                    Text := AMirrorController.Mirror[I].DirectlinksMirror[0];

                    for J := 0 to Count - 1 do
                      Params.AddFormField('url[' + IntToStr(_count) + '][]', Strings[J], cherryCMSSettings.Charset).ContentTransfer := 'binary';

                    if _count = 0 then
                      Params.AddFormField('parts', IntToStr(Count), cherryCMSSettings.Charset);
                  finally
                    Free;
                  end;
              end
              else if (AMirrorController.Mirror[I].CrypterCount > 0) then
              begin
                for J := 0 to AMirrorController.Mirror[I].CrypterCount - 1 do
                begin
                  AddFormField('url[' + IntToStr(_count) + '][]', AMirrorController.Mirror[I].Crypter[J].Link, cherryCMSSettings.Charset)
                    .ContentTransfer := 'binary';

                  if _count = 0 then
                    AddFormField('parts', IntToStr(AMirrorController.Mirror[I].CrypterCount), cherryCMSSettings.Charset);
                end;
              end
              else
              begin
                ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
                Exit;
              end;

              AddFormField('hoster[' + IntToStr(_count) + ']', AMirrorController.Mirror[I].Hoster, cherryCMSSettings.Charset)
                .ContentTransfer := 'binary';

              Inc(_count);
            end;

          AddFormField('mirrors', IntToStr(_count), cherryCMSSettings.Charset);

          AddFormField('comment', '', cherryCMSSettings.Charset);

          AddFormField('send', '', cherryCMSSettings.Charset);
        end;

        Request.ContentType := Params.RequestContentType;
        try
          ResponseStr := Post(Website + 'index.php' + cherryCMSSettings.custom_upload_url, Params);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;

        if (Pos('<div style="margin: 5px 10px;">', ResponseStr) = 0) then
        begin
          with TRegExpr.Create do
            try
              InputString := ResponseStr;
              Expression := '<td colspan="2" style="text-align:center; color:#FF0000;">(.*?)<\/td>';

              if Exec(InputString) then
                Self.ErrorMsg := HTML2Text(Match[1]);
            finally
              Free;
            end;

          Exit;
        end;

        Result := True;
      finally
        NFOStringStream.Free;
      end;
    finally
      Params.Free;
    end;
  end;
end;

constructor TcherryCMS.Create;
begin
  inherited Create;
  cherryCMSSettings := TcherryCMSSettings.Create;
end;

destructor TcherryCMS.Destroy;
begin
  cherryCMSSettings.Free;
  inherited Destroy;
end;

function TcherryCMS.GetName: WideString;
begin
  Result := 'cherryCMS';
end;

function TcherryCMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TcherryCMS.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('name="str" value="Suche..." class="inputtext"', string(AWebsiteSourceCode)) > 0);
end;

function TcherryCMS.GetIDs: Integer;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;

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
  Result := 0;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    LoadSettings;

    if not(AccountName = '') then
      if not Login(IdHTTPHelper) then
        Exit;

    try
      ResponseStr := IdHTTPHelper.Get(Website + 'index.php' + cherryCMSSettings.custom_upload_url);
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

  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(ResponseStr, 'name="childgroup"', '</select>');
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

function TcherryCMS.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TcherryCMSSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
