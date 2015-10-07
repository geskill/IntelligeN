unit uImagesGoogleDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TImagesGoogleDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

function TImagesGoogleDe.GetName;
begin
  result := 'images.google.de';
end;

function TImagesGoogleDe.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)];
  result := Word(_TemplateTypeIDs);
end;

function TImagesGoogleDe.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture];
  result := LongWord(_ComponentIDs);
end;

function TImagesGoogleDe.GetControlIDDefaultValue;
begin
  result := True;
end;

function TImagesGoogleDe.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

procedure TImagesGoogleDe.Exec;
const
  website = 'https://www.google.de/';
var
  _ComponentIDs: TControlIDs;
  _Title, _RequestURL: string;
  _Count: Integer;

  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  if TTypeID(ATypeID) = cMovie then
    _RequestURL := website + 'search?tbm=isch&q=' + HTTPEncode(_Title + ' cover')
  else
    _RequestURL := website + 'search?tbm=isch&q=' + HTTPEncode(TypeIDToString(TTypeID(ATypeID)) + ' ' + _Title + ' cover');

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
          AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
