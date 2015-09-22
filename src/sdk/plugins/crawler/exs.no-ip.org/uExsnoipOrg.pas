unit uExsnoipOrg;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TExsnoipOrg = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TExsnoipOrg.GetName;
begin
  result := 'exs.no-ip.org';
end;

function TExsnoipOrg.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  result := Word(_TemplateTypeIDs);
end;

function TExsnoipOrg.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TExsnoipOrg.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TExsnoipOrg.GetLimitDefaultValue;
begin
  result := 0;
end;

procedure TExsnoipOrg.Exec;
const
  website = 'http://exs.no-ip.org/';
var
  _ReleaseName: string;
  _Count: Integer;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _ReleaseName := AComponentController.FindControl(cReleaseName).Value;
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

          AComponentController.FindControl(cNFO).AddValue(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode, GetName);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
