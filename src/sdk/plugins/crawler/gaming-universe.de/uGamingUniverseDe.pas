unit uGamingUniverseDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGamingUniverseDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TGamingUniverseDe }

function TGamingUniverseDe.GetName;
begin
  Result := 'gaming-universe.de';
end;

function TGamingUniverseDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cNintendoDS, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360];
  Result := Word(_TemplateTypeIDs);
end;

function TGamingUniverseDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TGamingUniverseDe.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TGamingUniverseDe.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TGamingUniverseDe.Exec;
const
  website = 'http://gaming-universe.de/spiele/';
var
  _ComponentIDs: TComponentIDs;
  _Count: Integer;
  _Title, _Website: string;

  function GetPlatform(ATemplateTypeID: TTemplateTypeID): string;
  begin
    case ATemplateTypeID of
      cGameCube:
        Result := 'gamecube';
      cNintendoDS:
        Result := 'nintendods';
      cPlayStation2:
        Result := 'playstation2';
      cPlayStation3:
        Result := 'playstation3';
      cPlayStationPortable:
        Result := 'playstationportable';
      cWii:
        Result := 'wii';
      cXbox:
        Result := 'xbox';
      cXbox360:
        Result := 'xbox360';
    end;
  end;

  procedure deep_search(AWebsitesourcecode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsitesourcecode;
          Expression := ''', ''(.*?)'',';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    end;

    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
    begin

      with TRegExpr.Create do
        try
          InputString := AWebsitesourcecode;
          Expression := 'Genre:<\/b><br>(.*?)<br>';

          if Exec(InputString) then
          begin
            AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
          end;
        finally
          Free;
        end;
    end;

    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsitesourcecode;
          Expression := '<\/div>\s+<\/div>\s+<div.*?<div style=''text\-align.*?>(.*?)<\/div';

          if Exec(InputString) then
            if not(SameStr(Trim(Match[1]), '') or (copy(Match[1], 1, 1) = #$D)) then
              AComponentController.FindControl(cDescription).AddValue(HTML2Text(Match[1]), GetName);
        finally
          Free;
        end;
    end;
  end;

var
  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Website := 'http://' + GetPlatform(TTemplateTypeID(ATemplateTypeID)) + '.gaming-universe.de/';
  _Count := 0;

  HTTPRequest := THTTPRequest.Create(_Website + 'spiele/');
  HTTPRequest.Referer := website;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('game_keyw', _Title);
    AddFormField('game_suche', '');
    AddFormField('game_suche', 'Abschicken');
  end;

  RequestID1 := HTTPManager.Post(HTTPRequest, HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      // Expression := '"searchResultTitle"><a href="(.*?)"';
      Expression := 'class=''fl_390''><a href=''(.*?)''>';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
