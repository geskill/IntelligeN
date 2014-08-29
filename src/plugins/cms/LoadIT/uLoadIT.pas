unit uLoadIT;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols, IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TLoadITSettings = class(TCMSFormbasedPlugInSettings)
  published
    [AttrDefaultValue('')]
    property hoster_blacklist;

    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;

    property categorys;
  end;

  TLoadIT = class(TCMSFormbasedPlugIn)
  private
    LoadITSettings: TLoadITSettings;
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

function TLoadIT.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, LoadITSettings, AComponentController);
  with LoadITSettings do
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

function TLoadIT.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        AddFormField('username', AccountName, LoadITSettings.Charset).ContentTransfer := 'binary';
        AddFormField('password', AccountPassword, LoadITSettings.Charset).ContentTransfer := 'binary';
        AddFormField('submit', 'Login', LoadITSettings.Charset);
      end;

      try
        ResponseStr := Post(Website + 'index.php?p=login', Params);
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

    ResponseStr := Get(Website + 'index.php?p=home');

    if (Pos('p=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := 'alert\("(.*?)"';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
          end
          else
          begin
            Expression := 'class="maincontent">(.*?)<';

            if Exec(InputString) then
            begin
              Self.ErrorMsg := Trim(HTML2Text(Match[1]));
            end
          end;
        finally
          Free;
        end;
      Exit;
    end;
  end;
  Result := True;
end;

function TLoadIT.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string): Boolean;
const
  DownloadArray: array [0 .. 2, 0 .. 1] of string = (('mirror1_hoster', 'mirror1_links'), ('mirror2_hoster', 'mirror2_links'), ('mirror3_hoster', 'mirror3_links'));
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;

  I: Integer;

  _count: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with Params do
      begin
        AddFormField('up_name', Subject, LoadITSettings.Charset).ContentTransfer := 'binary';

        AddFormField('up_kat', LoadITSettings.categorys, LoadITSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cPassword)) then
          AddFormField('up_pw', AComponentController.FindControl(cPassword).Value, LoadITSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cPicture)) then
          AddFormField('up_cover', AComponentController.FindControl(cPicture).Value, LoadITSettings.Charset).ContentTransfer := 'binary';

        for I := 0 to AMirrorController.MirrorCount - 1 do
          if AMirrorController.Mirror[I].Size > 0 then
          begin
            AddFormField('up_size', FloatToStr(AMirrorController.Mirror[I].Size), LoadITSettings.Charset).ContentTransfer := 'binary';
            break;
          end;

        if Assigned(AComponentController.FindControl(cAudioBitrate)) then
          AddFormField('up_bitrate', AComponentController.FindControl(cAudioBitrate).Value, LoadITSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cGenre)) then
          AddFormField('genre', AComponentController.FindControl(cGenre).Value, LoadITSettings.Charset).ContentTransfer := 'binary';

        if Assigned(AComponentController.FindControl(cTrailer)) then
          AddFormField('up_trailer', AComponentController.FindControl(cTrailer).Value, LoadITSettings.Charset).ContentTransfer := 'binary';

        if not LoadITSettings.use_textasdescription then
        begin
          if Assigned(AComponentController.FindControl(cDescription)) then
            AddFormField('beschreibung', AComponentController.FindControl(cDescription).Value, LoadITSettings.Charset).ContentTransfer := 'binary'
        end
        else
          AddFormField('beschreibung', Message, LoadITSettings.Charset).ContentTransfer := 'binary';

        _count := 0;
        for I := 0 to AMirrorController.MirrorCount - 1 do
          if (Pos(string(AMirrorController.Mirror[I].Hoster), LoadITSettings.hoster_blacklist) = 0) then
          begin
            if _count = 3 then
              break;

            AddFormField(DownloadArray[_count][0], AMirrorController.Mirror[I].Hoster, LoadITSettings.Charset).ContentTransfer := 'binary';

            if LoadITSettings.use_plainlinks then
              AddFormField(DownloadArray[_count][1], AMirrorController.Mirror[I].DirectlinksMirror[0], LoadITSettings.Charset).ContentTransfer := 'binary'
            else if (AMirrorController.Mirror[I].CrypterCount > 0) then
              AddFormField(DownloadArray[_count][1], AMirrorController.Mirror[I].Crypter[0].Link, LoadITSettings.Charset).ContentTransfer := 'binary'
            else
            begin
              ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
              Exit;
            end;

            Inc(_count);
          end;
      end;

      try
        ResponseStr := Post(Website + 'index.php?p=insert', Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor TLoadIT.Create;
begin
  inherited Create;
  LoadITSettings := TLoadITSettings.Create;
end;

destructor TLoadIT.Destroy;
begin
  LoadITSettings.Free;
  inherited Destroy;
end;

function TLoadIT.GetName;
begin
  Result := 'LoadIT';
end;

function TLoadIT.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TLoadIT.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := False;
end;

function TLoadIT.GetIDs: Integer;
begin
  //
end;

function TLoadIT.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TLoadITSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
