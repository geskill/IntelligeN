unit uwFusion;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSFormbasedClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TwFusionSettings = class(TCMSFormbasedPlugInSettings)
  published
    [AttrDefaultValue('')]
    property hoster_blacklist;

    [AttrDefaultValue(False)]
    property use_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription;

    property categorys;
  end;

  TwFusion = class(TCMSFormbasedPlugIn)
  private
    wFusionSettings: TwFusionSettings;
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

function TwFusion.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, wFusionSettings, AComponentController);
  with wFusionSettings do
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

function TwFusion.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        Add('name=' + AccountName);
        Add('password=' + AccountPassword);
        Add('login=Login');
      end;

      Request.CharSet := wFusionSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'login/', Params, Enc);
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

    if (Pos('logout/', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<h3>(.*?)<\/h3>';

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

function TwFusion.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I: Integer;

  _lang: string;
  _count: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        if Assigned(AComponentController.FindControl(cReleaseName)) then
          Add('title=' + AComponentController.FindControl(cReleaseName).Value);

        Add('category=' + VarToStr(wFusionSettings.categorys));

        if Assigned(AComponentController.FindControl(cPicture)) then
          Add('cover=' + AComponentController.FindControl(cPicture).Value);

        if Assigned(AComponentController.FindControl(cGenre)) then
          Add('genre=' + AComponentController.FindControl(cGenre).Value);

        if Assigned(AComponentController.FindControl(cPassword)) then
          Add('password=' + AComponentController.FindControl(cPassword).Value);

        if Assigned(AComponentController.FindControl(cLanguage)) then
        begin
          _lang := AComponentController.FindControl(cLanguage).Value;

          if (Pos('GER', _lang) > 0) and not(Pos('ENG', _lang) > 0) and not(Pos('FRE', _lang) > 0) and not(Pos('SPA', _lang) > 0) and not
            (Pos('JPN', _lang) > 0) then
            Add('lang=1')
          else if (Pos('ENG', _lang) > 0) and not(Pos('GER', _lang) > 0) and not(Pos('FRE', _lang) > 0) and not(Pos('SPA', _lang) > 0) and not
            (Pos('JPN', _lang) > 0) then
            Add('lang=2')
          else if (Pos('SPA', _lang) > 0) and not(Pos('GER', _lang) > 0) and not(Pos('FRE', _lang) > 0) and not(Pos('ENG', _lang) > 0) and not
            (Pos('JPN', _lang) > 0) then
            Add('lang=5')
          else if (Pos('FRE', _lang) > 0) and not(Pos('GER', _lang) > 0) and not(Pos('ENG', _lang) > 0) and not(Pos('SPA', _lang) > 0) and not
            (Pos('JPN', _lang) > 0) then
            Add('lang=6')
          else if (Pos('JPN', _lang) > 0) and not(Pos('GER', _lang) > 0) and not(Pos('FRE', _lang) > 0) and not(Pos('SPA', _lang) > 0) and not
            (Pos('ENG', _lang) > 0) then
            Add('lang=8')
          else
            Add('lang=3');
        end;

        if not wFusionSettings.use_textasdescription then
        begin
          if Assigned(AComponentController.FindControl(cDescription)) then
            Add('description=' + AComponentController.FindControl(cDescription).Value);
        end
        else
          Add('description=' + Message);

        if Assigned(AComponentController.FindControl(cNFO)) then
          Add('nfo=' + AComponentController.FindControl(cNFO).Value);

        _count := 0;
        for I := 0 to AMirrorController.MirrorCount - 1 do
          if (Pos(string(AMirrorController.Mirror[I].Hoster), wFusionSettings.hoster_blacklist) = 0) then
          begin
            if wFusionSettings.use_plainlinks then
              Add('urls[]=' + AMirrorController.Mirror[I].DirectlinksMirror[0])
            else if (AMirrorController.Mirror[I].CrypterCount > 0) then
              Add('urls[]=' + AMirrorController.Mirror[I].Crypter[0].Link)
            else
            begin
              ErrorMsg := 'No crypter initialized! (disable use_plainlinks or add a crypter)';
              Exit;
            end;

            Inc(_count);
          end;

        Add('mirror_count=' + IntToStr(_count));

        Add('upload=Download eintragen');
      end;

      Request.CharSet := wFusionSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'upload/', Params, Enc);
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

      if (Pos('<br /><br /><a href=', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := 'r<\/div>\s+<div class="new">\s+<br \/>(.*?)<br /><br />';

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

constructor TwFusion.Create;
begin
  inherited Create;
  wFusionSettings := TwFusionSettings.Create;
end;

destructor TwFusion.Destroy;
begin
  wFusionSettings.Free;
  inherited Destroy;
end;

function TwFusion.GetName;
begin
  Result := 'wFusion';
end;

function TwFusion.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TwFusion.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  // TODO:
  Result := False;
end;

function TwFusion.GetIDs;
begin
  // TODO:
  Result := FCheckedIDsList.Count;
end;

function TwFusion.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TwFusionSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
