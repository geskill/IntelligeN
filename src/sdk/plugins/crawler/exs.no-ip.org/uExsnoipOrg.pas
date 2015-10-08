unit uExsnoipOrg;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TExsnoipOrg = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TExsnoipOrg.GetName;
begin
  result := 'exs.no-ip.org';
end;

function TExsnoipOrg.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  result := Word(_TemplateTypeIDs);
end;

function TExsnoipOrg.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TExsnoipOrg.GetControlIDDefaultValue;
begin
  result := True;
end;

function TExsnoipOrg.GetResultsLimitDefaultValue;
begin
  result := 0;
end;

function TExsnoipOrg.Exec;
const
  website = 'http://exs.no-ip.org/';
var
  _ReleaseName: string;
  _Count: Integer;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _ReleaseName := AControlController.FindControl(cReleaseName).Value;
  _Count := 0;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'index.php?search_for=' + HTTPEncode(_ReleaseName)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := ';<a href="nforeader\.php\?id=(.*?)"';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(HTMLDecode(website + 'nfofile/' + Match[1] + '.nfo'), RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          AControlController.FindControl(cNFO).AddProposedValue(GetName, HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
