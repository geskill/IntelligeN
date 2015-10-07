unit uWiiboxartCom;

interface

uses
  // Delphi
  SysUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TWiiboxartCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

function TWiiboxartCom.GetName;
begin
  result := 'wiiboxart.com';
end;

function TWiiboxartCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cWii];
  result := Word(_TemplateTypeIDs);
end;

function TWiiboxartCom.GetAvailableControlIDs;
var
  // _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  // _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cPicture];

  result := LongWord(_ComponentIDs);
end;

function TWiiboxartCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TWiiboxartCom.GetResultsLimitDefaultValue;
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
  _Title := AControlController.FindControl(cTitle).Value;

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
          AControlController.FindControl(cPicture).AddProposedValue(GetName, website + 'artwork/cover/' + Match[1]);
        until not ExecNext;
      end;
    finally
      Free;
    end;
end;

end.
