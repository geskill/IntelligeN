unit uGametrailersCom;

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
  TGametrailersCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TGametrailersCom }

function TGametrailersCom.GetName;
begin
  Result := 'gametrailers.com';
end;

function TGametrailersCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := cGames;
  Result := Word(_TemplateTypeIDs);
end;

function TGametrailersCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cTrailer];
  Result := LongWord(_ComponentIDs);
end;

function TGametrailersCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TGametrailersCom.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TGametrailersCom.Exec;
const
  website = 'http://www.gametrailers.com/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  function GetPlatform(ATemplateTypeID: TTemplateTypeID): string;
  begin
    case ATemplateTypeID of
      cGameCube:
        Result := '&s_platforms[gc]=on';
      cNintendoDS:
        Result := '&s_platforms[dsi]=on&s_platforms[dsiware]=on&s_platforms[3ds]=on&s_platforms[ds]=on';
      cPCGames:
        Result := '&s_platforms[pc]=on';
      cPlayStation2:
        Result := '&s_platforms[ps2]=on';
      cPlayStation3:
        Result := '&s_platforms[ps3]=on';
      cPlayStationPortable:
        Result := '&s_platforms[psn]=on';
      cWii:
        Result := '&s_platforms[wii]=on&s_platforms[wiiu]=on&s_platforms[vcon]=on&s_platforms[wiiware]=on';
      cXbox:
        Result := '&s_platforms[xbox]=on';
      cXbox360:
        Result := '&s_platforms[xb360]=on&s_platforms[xbla]=on&s_platforms[xblig]=on';
    end;
  end;

  procedure deep_search(AWebsitesourcecode: string);
  begin
    if (AComponentController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitesourcecode;
          Expression := '<h3 class="MovieTitle">\s+<a href="(.*?)" class="gamepage_content_row_title">(.*?)<';

          if Exec(InputString) then
          begin
            repeat (AComponentController.FindControl(cTrailer) as ITrailer)
              .AddValue(website + Match[1], Trim(Match[2]), GetName);
            until not ExecNext;
          end;
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

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'search.php?platlogic=OR' + GetPlatform(TTemplateTypeID(ATemplateTypeID))
        + '&orderby=rdate&s_order=DESC&search_type=advanced&s=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      // Expression := '"searchResultTitle"><a href="(.*?)"';
      Expression := 'class="search_game_row_thumb">\s+<a href="(.*?)">';

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
