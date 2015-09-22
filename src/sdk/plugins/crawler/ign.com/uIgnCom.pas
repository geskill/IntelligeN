unit uIgnCom;

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
  TIgnCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TIgnCom.GetName;
begin
  result := 'ign.com';
end;

function TIgnCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360];
  result := Word(_TemplateTypeIDs);
end;

function TIgnCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TIgnCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TIgnCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TIgnCom.Exec;
const
  website = 'ign.com/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  function GetPlatform(ATemplateTypeID: TTemplateTypeID): string;
  begin
    case ATemplateTypeID of
      cAudio:
        ;
      cGameCube:
        result := 'GameCube';
      cMovie:
        ;
      cNintendoDS:
        result := 'DS';
      cPCGames:
        result := 'PC';
      cPlayStation2:
        result := 'PS2';
      cPlayStation3:
        result := 'PS3';
      cPlayStationPortable:
        result := 'PSP';
      cSoftware:
        ;
      cWii:
        result := 'Wii';
      cXbox:
        result := 'Xbox';
      cXbox360:
        result := 'Xbox360';
      cXXX:
        ;
      cOther:
        result := 'Other';
    end;

  end;

  procedure deep_search(AWebsitesourcecode: string);

    procedure deep_genre_search(aGenrecode: string);
    begin
      with TRegExpr.Create do
        try
          InputString := aGenrecode;
          Expression := '(\S+)';

          if Exec(InputString) then
          begin
            repeat
              aGenrecode := Match[1];
              AComponentController.FindControl(cGenre).AddValue(aGenrecode, GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          ModifierS := False;
          InputString := AWebsitesourcecode;
          Expression := 'alt="(.*?)" title="(.*?)" src="(.*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(Match[3], GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitesourcecode;
          Expression := 'Genre: <\/strong>(.*?)<br\/>';

          if Exec(InputString) then
            deep_genre_search(Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;

    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          ModifierS := True;
          InputString := AWebsitesourcecode;
          if Pos('<div class="column-about-boxart">', AWebsitesourcecode) = 0 then
            Expression := '<div id="about-tabs-data" class="txt-para">(.*?)<div'
          else
            Expression := '<div class="column-about-boxart">(.*?)<div';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(HTMLDecode(Match[1]))), GetName);
        finally
          Free;
        end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create('http://search.' + website + 'products?sort=relevance&so=exact&platform=' + HTTPEncode
        (GetPlatform(TTemplateTypeID(ATemplateTypeID))) + '&query=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      // Expression := '"searchResultTitle"><a href="(.*?)"';
      Expression := '<div class="result-image">\s+<a href="(.*?)"';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

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
