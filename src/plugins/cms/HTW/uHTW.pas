unit uHTW;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

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

    [AttrDefaultValue('')]
    property hoster_blacklist;
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

function THTW.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, HTWSettings, AComponentController);
  with HTWSettings do
  begin
    if SameStr('', Charset) then
      Charset := DefaultCharset;

    if Assigned(AComponentController) then
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

function THTW.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        AddFormField('user', AccountName, HTWSettings.Charset).ContentTransfer := 'binary';
        AddFormField('pwd', AccountPassword, HTWSettings.Charset).ContentTransfer := 'binary';
        AddFormField('submit', '', HTWSettings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'index.php?do=login', Params);
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

    if (Pos('http-equiv="refresh" content="3', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<br><center>(.*?)<\/center>';

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

function THTW.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  DownloadArrayA: array [0 .. 4] of string = ('dll1', 'dll2', 'dll3', 'dll4', 'dll5');
  DownloadArrayB: array [0 .. 4] of string = ('link', 'mirror1', 'mirror2', 'mirror3', 'mirror4');
  HosterArray: array [0 .. 4] of string = ('link_hoster', 'hoster1', 'hoster2', 'hoster3', 'hoster4');
  StatusArray: array [0 .. 4] of string = ('link_status', 'status1', 'status2', 'status3', 'status4');
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  FormatSettings: TFormatSettings;

  I: Integer;

  _count: Integer;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        AddFormField('name', Subject, HTWSettings.Charset).ContentTransfer := 'binary';
        AddFormField('title', Subject, HTWSettings.Charset).ContentTransfer := 'binary';

        AddFormField('uploader', AccountName, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cReleaseName)) then
          AddFormField('rls_name', AComponentController.FindControl(cReleaseName).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cPassword)) then
        begin
          AddFormField('pw', AComponentController.FindControl(cPassword).Value, HTWSettings.Charset).ContentTransfer := 'binary';
          AddFormField('passwort', AComponentController.FindControl(cPassword).Value, HTWSettings.Charset).ContentTransfer := 'binary';
        end;

        if Assigned(AComponentController.FindControl(cPicture)) then
          AddFormField('cover', AComponentController.FindControl(cPicture).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        AddFormField(VarToStr(HTWSettings.category_field), HTWSettings.categorys, HTWSettings.Charset).ContentTransfer := 'binary';
        AddFormField(VarToStr(HTWSettings.subcategory_field), HTWSettings.subcategorys, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cVideoCodec)) and
          (not(HTWSettings.category_field = 'format') and not(HTWSettings.subcategory_field = 'format')) then
          AddFormField('format', AComponentController.FindControl(cVideoCodec).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cGenre)) and
          (not(HTWSettings.category_field = 'genre') and not(HTWSettings.subcategory_field = 'genre')) then
          AddFormField('genre', AComponentController.FindControl(cGenre).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cAudioStream)) then
          AddFormField('qual_ton', AComponentController.FindControl(cAudioStream).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cVideoSystem)) then
          AddFormField('qual_bild', AComponentController.FindControl(cVideoSystem).Value, HTWSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cReleaseDate)) then
          AddFormField('jahr', FormatDateTime('yyyy', StrToDate(AComponentController.FindControl(cReleaseDate).Value, FormatSettings),
              FormatSettings), HTWSettings.Charset).ContentTransfer := 'binary'
        else
          AddFormField('jahr', '', HTWSettings.Charset);

        for I := 0 to AMirrorController.MirrorCount - 1 do
          if AMirrorController.Mirror[I].Size > 0 then
          begin
            AddFormField('groesse', FloatToStr(AMirrorController.Mirror[I].Size), HTWSettings.Charset).ContentTransfer := 'binary';
            AddFormField('size', FloatToStr(AMirrorController.Mirror[I].Size), HTWSettings.Charset).ContentTransfer := 'binary';
            break;
          end;

        if Assigned(AComponentController.FindControl(cLanguage)) then
        begin
          if not(Pos(';', AComponentController.FindControl(cLanguage).Value) = 0) then
            AddFormField('sprache', 'Multi (De, En, Fr, Ru)', HTWSettings.Charset).ContentTransfer := 'binary'
          else
            case IndexText(AComponentController.FindControl(cLanguage).Value, ['GER', 'ENG', 'FRE', 'RUS']) of
              0:
                AddFormField('sprache', 'Deutsch', HTWSettings.Charset);
              1:
                AddFormField('sprache', 'Englisch', HTWSettings.Charset);
              2:
                AddFormField('sprache', 'Französisch', HTWSettings.Charset);
              3:
                AddFormField('sprache', 'Russisch', HTWSettings.Charset);
            else
              AddFormField('sprache', '', HTWSettings.Charset);
            end
        end
        else
          AddFormField('sprache', '', HTWSettings.Charset);

        if not HTWSettings.use_textasdescription then
        begin
          if Assigned(AComponentController.FindControl(cDescription)) then
            AddFormField('beschreibung', AComponentController.FindControl(cDescription).Value, HTWSettings.Charset).ContentTransfer := 'binary'
        end
        else
          AddFormField('beschreibung', Message, HTWSettings.Charset).ContentTransfer := 'binary';

        _count := 0;
        for I := 0 to AMirrorController.MirrorCount - 1 do
          if (Pos(string(AMirrorController.Mirror[I].Hoster), HTWSettings.hoster_blacklist) = 0) then
          begin
            if _count = 5 then
              break;

            if HTWSettings.use_plainlinks then
            begin
              AddFormField(DownloadArrayA[_count], AMirrorController.Mirror[I].DirectlinksMirror[0], HTWSettings.Charset).ContentTransfer := 'binary';
              AddFormField(DownloadArrayB[_count], AMirrorController.Mirror[I].DirectlinksMirror[0], HTWSettings.Charset).ContentTransfer := 'binary';
            end
            else if (AMirrorController.Mirror[I].CrypterCount > 0) then
            begin
              AddFormField(DownloadArrayA[_count], AMirrorController.Mirror[I].Crypter[0].Link, HTWSettings.Charset).ContentTransfer := 'binary';
              AddFormField(DownloadArrayB[_count], AMirrorController.Mirror[I].Crypter[0].Link, HTWSettings.Charset).ContentTransfer := 'binary';
            end
            else
            begin
              ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
              Exit;
            end;
            AddFormField(HosterArray[_count], AMirrorController.Mirror[I].Hoster, HTWSettings.Charset).ContentTransfer := 'binary';
            AddFormField(StatusArray[_count], '', HTWSettings.Charset);

            Inc(_count);
          end;

        AddFormField('sent', '1', HTWSettings.Charset);
        AddFormField('submit', '', HTWSettings.Charset);
      end;

      try
        ResponseStr := Post(Website + VarToStr(HTWSettings.custom_upload_url), Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      if not(Pos('javascript:history.back()', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
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

        Exit;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor THTW.Create;
begin
  inherited Create;
  HTWSettings := THTWSettings.Create;
end;

destructor THTW.Destroy;
begin
  HTWSettings.Free;
  inherited Destroy;
end;

function THTW.GetName;
begin
  Result := 'HTW';
end;

function THTW.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function THTW.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := False;
end;

function THTW.GetIDs: Integer;
begin
  //
end;

function THTW.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, THTWSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
