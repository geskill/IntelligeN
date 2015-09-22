unit uNfodbRu;

interface

uses
  // Delphi
  SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TNfodbRu = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TNfodbRu.GetName;
begin
  result := 'Nfodb.ru';
end;

function TNfodbRu.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cAudio];
  result := Word(_TemplateTypeIDs);
end;

function TNfodbRu.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TNfodbRu.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TNfodbRu.GetLimitDefaultValue;
begin
  result := 0;
end;

procedure TNfodbRu.Exec;
const
  website = 'http://nfodb.ru/';
var
  _Releasename: string;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _Releasename := AComponentController.FindControl(cReleaseName).Value;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + '?do_search=Search&frelease=' + HTTPEncode(_Releasename)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
  begin
    try
      ModifierS := False;
      InputString := ResponseStrSearchResult;
      Expression := 'href=''nfo-(\d+)-';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(website + 'get_nfo-' + Match[1] + '.php', RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          AComponentController.FindControl(cNFO).AddValue(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode, GetName);
        until not ExecNext;
      end;
    finally
      Free;
    end;
  end;
end;

end.
