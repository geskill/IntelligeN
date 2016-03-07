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
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TImagesGoogleDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'https://www.google.de/';
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AAccountData: IAccountData; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

function TImagesGoogleDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TImagesGoogleDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TImagesGoogleDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

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

function TImagesGoogleDe.InternalGetRetrieveData;
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

  Result := True;
end;

function TImagesGoogleDe.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

end.
