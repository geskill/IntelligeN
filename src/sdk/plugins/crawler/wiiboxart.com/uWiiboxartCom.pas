unit uWiiboxartCom;

interface

uses
  // Delphi
  SysUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TWiiboxartCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TWiiboxartCom.GetName;
begin
  result := 'wiiboxart.com';
end;

function TWiiboxartCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cWii];
  result := Word(_TemplateTypeIDs);
end;

function TWiiboxartCom.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture];

  result := LongWord(_ComponentIDs);
end;

function TWiiboxartCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TWiiboxartCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TWiiboxartCom.Exec;
const
  website = 'http://www.wiiboxart.com/';
var
  _Title: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  _Title := AComponentController.FindControl(cTitle).Value;

  HTTPParams := THTTPParams.Create(ptMultipartFormData);
  HTTPParams.AddFormField('search', _Title);
  HTTPParams.AddFormField('ie', 'ISO-8859-1');

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'search.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      ModifierS := False;
      InputString := ResponseStrSearchResult;
      Expression := '"artwork\/cover\/(.*?)" target=';

      if Exec(InputString) then
      begin
        repeat
          AComponentController.FindControl(cPicture).AddValue(website + 'artwork/cover/' + Match[1], GetName);
        until not ExecNext;
      end;
    finally
      Free;
    end;
end;

end.
