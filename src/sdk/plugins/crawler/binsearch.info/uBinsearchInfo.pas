unit uBinsearchInfo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TBinsearchInfo = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TBinsearchInfo.GetName;
begin
  Result := 'Binsearch.info';
end;

function TBinsearchInfo.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)];
  Result := Word(_TemplateTypeIDs);
end;

function TBinsearchInfo.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cNFO];
  Result := LongWord(_ComponentIDs);
end;

function TBinsearchInfo.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TBinsearchInfo.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

function TBinsearchInfo.Exec;
const
  website = 'http://binsearch.info';

  procedure deep_search(AWebsite: string);
  begin
    with TRegExpr.Create do
      try
        InputString := AWebsite;
        Expression := '<pre>(.*?)<\/pre>';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cNFO).AddProposedValue(GetName, HTML2Text(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

var
  _ComponentIDs: TControlIDs;
  _ReleaseName: string;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;

  _ReleaseName := AControlController.FindControl(cReleaseName).Value;

  // '&m=n&max=25&adv_age=365&adv_sort=date&adv_nfo=on'
  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + '/index.php?q=' + HTTPEncode(_ReleaseName) + '&max=250&adv_age=&server='),
    TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      // ModifierG := False;
      InputString := ResponseStrSearchResult;
      Expression := 'viewNFO\.php\?oid=(.*?)&amp;';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(website + '/viewNFO.php?oid=' + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

        until not ExecNext;
      end;
    finally
      Free;
    end;

end;

end.
