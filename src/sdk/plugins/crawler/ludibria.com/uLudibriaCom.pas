unit uLudibriaCom;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TLudibriaCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TLudibriaCom.GetName;
begin
  result := 'Ludibria.com';
end;

function TLudibriaCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cNintendoDS, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox360];
  result := Word(_TemplateTypeIDs);
end;

function TLudibriaCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TLudibriaCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TLudibriaCom.GetResultsLimitDefaultValue;
begin
  result := 0;
end;

function TLudibriaCom.Exec;
const
  website = 'http://www.ludibria.com/';
var
  _ComponentIDs: TControlIDs;
  _ReleaseName: WideString;

  function TemplateTypeIDToSysId(ATypeID: TTypeID): string;
  begin
    case ATypeID of
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
  LongWord(_ComponentIDs) := AControlIDs;
  _ReleaseName := AControlController.FindControl(cReleaseName).Value;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'index.php?sys=' + TemplateTypeIDToSysId(TTypeID(ATypeID)) + '&srg=' + HTTPEncode
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

          AControlController.FindControl(cNFO).AddProposedValue(GetName, HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode);

        until not ExecNext;
      end;
    finally
      Free;
    end;
end;

end.
