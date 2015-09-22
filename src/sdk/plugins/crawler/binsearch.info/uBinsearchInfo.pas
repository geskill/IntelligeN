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
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TBinsearchInfo = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TBinsearchInfo.GetName;
begin
  Result := 'Binsearch.info';
end;

function TBinsearchInfo.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  Result := Word(_TemplateTypeIDs);
end;

function TBinsearchInfo.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cNFO];
  Result := LongWord(_ComponentIDs);
end;

function TBinsearchInfo.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TBinsearchInfo.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TBinsearchInfo.Exec;
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
            AComponentController.FindControl(cNFO).AddValue(HTML2Text(Match[1]), GetName);
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

var
  _ComponentIDs: TComponentIDs;
  _ReleaseName: string;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;

  _ReleaseName := AComponentController.FindControl(cReleaseName).Value;

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
