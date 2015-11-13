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
  protected { . }
  const
    WEBSITE = 'http://www.ludibria.com/';
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

function TLudibriaCom.GetName;
begin
  result := 'Ludibria.com';
end;

function TLudibriaCom.InternalGetAvailableTypeIDs;
begin
  result := [cGameCube, cNintendoDS, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox360];
end;

function TLudibriaCom.InternalGetAvailableControlIDs;
begin
  result := [cNFO];
end;

function TLudibriaCom.InternalGetControlIDDefaultValue;
begin
  result := True;
end;

function TLudibriaCom.InternalGetDependentControlIDs;
begin
  result := [cReleaseName];
end;

function TLudibriaCom.InternalExecute;
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

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(WEBSITE + 'index.php?sys=' + TemplateTypeIDToSysId(TTypeID(ATypeID)) + '&srg=' + HTTPEncode(_ReleaseName)), TPlugInHTTPOptions.Create(Self));

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

          RequestID2 := HTTPManager.Get(WEBSITE + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          RequestID3 := HTTPManager.Get(WEBSITE + StringReplace(Match[1], 'nfo', 'dwn', []), RequestID2, TPlugInHTTPOptions.Create(Self));

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

function TLudibriaCom.GetResultsLimitDefaultValue;
begin
  result := 1;
end;

end.
