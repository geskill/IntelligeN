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
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TYoutubeCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TYoutubeCom }

function TYoutubeCom.GetName;
begin
  result := 'Youtube.com';
end;

function TYoutubeCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TYoutubeCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cTrailer];

  result := LongWord(_ComponentIDs);
end;

function TYoutubeCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TYoutubeCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TYoutubeCom.Exec;
const
  website = 'https://www.youtube.com/';
var
  _Title: string;
  _ComponentIDs: TComponentIDs;
  _Count: Integer;

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  if (AComponentController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
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
            (AComponentController.FindControl(cTrailer) as ITrailer).AddValue(website + Match[2], HTML2Text(Match[4]), GetName);
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
