unit uImagesGoogleDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TImagesGoogleDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TImagesGoogleDe.GetName;
begin
  result := 'images.google.de';
end;

function TImagesGoogleDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  result := Word(_TemplateTypeIDs);
end;

function TImagesGoogleDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture];
  result := LongWord(_ComponentIDs);
end;

function TImagesGoogleDe.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TImagesGoogleDe.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TImagesGoogleDe.Exec;
const
  website = 'https://www.google.de/';
var
  _ComponentIDs: TComponentIDs;
  _Title, _RequestURL: string;
  _Count: Integer;

  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  if TTemplateTypeID(ATemplateTypeID) = cMovie then
    _RequestURL := website + 'search?tbm=isch&q=' + HTTPEncode(_Title + ' cover')
  else
    _RequestURL := website + 'search?tbm=isch&q=' + HTTPEncode(TTemplateTypeIDToString(TTemplateTypeID(ATemplateTypeID)) + ' ' + _Title + ' cover');

  RequestID := HTTPManager.Get(THTTPRequest.Create(_RequestURL), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := 'imgres\?imgurl=(.*?)&';

      if Exec(InputString) then
      begin
        repeat
          AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
