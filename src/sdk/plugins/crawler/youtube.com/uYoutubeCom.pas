unit uYoutubeCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TYoutubeCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TYoutubeCom }

function TYoutubeCom.GetName;
begin
  result := 'Youtube.com';
end;

function TYoutubeCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)] - [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TYoutubeCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cTrailer];

  result := LongWord(_ComponentIDs);
end;

function TYoutubeCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TYoutubeCom.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

function TYoutubeCom.Exec;
const
  website = 'https://www.youtube.com/';
var
  _Title: string;
  _ComponentIDs: TControlIDs;
  _Count: Integer;

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  if (AControlController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
  begin
    HTTPRequest := THTTPRequest.Create(website + 'results?search_type=videos&search_query=' + HTTPEncode(_Title + ' hd trailer'));
    HTTPRequest.Referer := website;

    RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

    ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

    with TRegExpr.Create do
    begin
      try
        ModifierS := False;
        InputString := ExtractTextBetween(ResponeStr, '"item-section">', '</ol>');
        Expression := '<h3(.*?)href="\/(.*?)"(.*?)dir="ltr">(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cTrailer).AddProposedValue(GetName, website + Match[2], HTML2Text(Match[4]));
            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
    end;
  end;
end;

end.
