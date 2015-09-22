unit uLudibriaCom;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TLudibriaCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TLudibriaCom.GetName;
begin
  result := 'Ludibria.com';
end;

function TLudibriaCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cNintendoDS, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox360];
  result := Word(_TemplateTypeIDs);
end;

function TLudibriaCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TLudibriaCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TLudibriaCom.GetLimitDefaultValue;
begin
  result := 0;
end;

procedure TLudibriaCom.Exec;
const
  website = 'http://www.ludibria.com/';
var
  _ComponentIDs: TComponentIDs;
  _ReleaseName: WideString;

  function TemplateTypeIDToSysId(TemplateTypeID: TTemplateTypeID): string;
  begin
    case TemplateTypeID of
      cGameCube:
        result := 'ngc';
      cNintendoDS:
        result := 'nds';
      cPlayStation2:
        result := 'ps2';
      cPlayStation3:
        result := 'ps3';
      cPlayStationPortable:
        result := 'psp';
      cWii:
        result := 'wii';
      cXbox360:
        result := 'x36';
    end;
  end;

var
  RequestID1, RequestID2, RequestID3: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _ReleaseName := AComponentController.FindControl(cReleaseName).Value;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'index.php?sys=' + TemplateTypeIDToSysId(TTemplateTypeID(ATemplateTypeID)) + '&srg=' + HTTPEncode
        (_ReleaseName)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<a href=''([^\"]+?)''(.*?) class="alink">' + _ReleaseName + '<\/a>';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          RequestID3 := HTTPManager.Get(website + StringReplace(Match[1], 'nfo', 'dwn', []), RequestID2, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID3);

          AComponentController.FindControl(cNFO).AddValue(HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode, GetName);

        until not ExecNext;
      end;
    finally
      Free;
    end;
end;

end.
