unit uNfodbRu;

interface

uses
  // Delphi
  SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TNfodbRu = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TNfodbRu.GetName;
begin
  result := 'Nfodb.ru';
end;

function TNfodbRu.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cAudio];
  result := LongWord(_TemplateTypeIDs);
end;

function TNfodbRu.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cNFO];
  result := LongWord(_ComponentIDs);
end;

function TNfodbRu.GetControlIDDefaultValue;
begin
  result := True;
end;

function TNfodbRu.GetResultsLimitDefaultValue;
begin
  result := 0;
end;

function TNfodbRu.Exec;
const
  website = 'http://nfodb.ru/';
var
  _Releasename: string;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _Releasename := AControlController.FindControl(cReleaseName).Value;

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

          AControlController.FindControl(cNFO).AddProposedValue(GetName, HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);
        until not ExecNext;
      end;
    finally
      Free;
    end;
  end;
end;

end.
