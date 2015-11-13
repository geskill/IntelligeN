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
  protected { . }
  const
    WEBSITE = 'https://www.google.de/';
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

function TImagesGoogleDe.GetName;
begin
  result := 'images.google.de';
end;

function TImagesGoogleDe.InternalGetAvailableTypeIDs;
begin
  result := [low(TTypeID) .. high(TTypeID)];
end;

function TImagesGoogleDe.InternalGetAvailableControlIDs;
begin
  result := [cPicture];
end;

function TImagesGoogleDe.InternalGetControlIDDefaultValue;
begin
  result := True;
end;

function TImagesGoogleDe.InternalGetDependentControlIDs;
begin
  result := [cTitle];
end;

function TImagesGoogleDe.InternalExecute;
var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LSearchString, LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  if ATypeID = cMovie then
    LSearchString := LTitle + ' cover'
  else
    LSearchString := TypeIDToString(ATypeID) + ' ' + LTitle + ' cover';

  LResponeStr := GETRequest(WEBSITE + 'search?tbm=isch&q=' + HTTPEncode(LSearchString), LRequestID1);

  with TRegExpr.Create do
    try
      InputString := LResponeStr;
      Expression := 'imgres\?imgurl=(.*?)&';

      if Exec(InputString) then
      begin
        repeat
          AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);

          Inc(LCount);
        until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

function TImagesGoogleDe.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

end.
