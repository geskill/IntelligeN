unit uPornofilmVertriebDe;

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
  TPornofilmVertriebDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TPornofilmVertriebDe }

function TPornofilmVertriebDe.GetName;
begin
  Result := 'pornofilm-vertrieb.de';
end;

function TPornofilmVertriebDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TPornofilmVertriebDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cRuntime, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TPornofilmVertriebDe.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TPornofilmVertriebDe.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TPornofilmVertriebDe.Exec;
const
  website = 'http://www.pornofilm-vertrieb.de/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img id="showCover" src="(.*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(website + Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<span>(\d+) min<\/span>';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '10px;overflow:auto;padding:3px">(.*?)<\/div>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Match[1], GetName);
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

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'index.php?shopid=2&action=catalog&search=' + HTTPEncode(_Title)),
    TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<a class="Btn" href="(.*?)">';

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
