unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uURLUtils;

type
  T%FullName% = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = '%Website%';
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

function T%FullName%.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function T%FullName%.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function T%FullName%.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function T%FullName%.GetName;
begin
  { TODO : change name? }
  Result := '%FullName%';
end;

function T%FullName%.InternalGetAvailableTypeIDs;
begin
  { TODO : change categories }
  // Result :=  [ low(TTypeID) .. high(TTypeID)] - [cAudio, cEBook];
  Result := [cAudio, cEBook, cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation3, cPlayStation4, cPlayStationPortable, cSoftware, cWii, cWiiU, cXbox360, cXboxOne, cXXX, cOther];
end;

function T%FullName%.InternalGetAvailableControlIDs;
begin
  { TODO : change controls for which this plugin retrieve information }
  Result := [cReleaseDate, cTitle, cNFO];

  if not(ATypeID = cXXX) then
    Result := Result + [cDescription];

  if (ATypeID = cMovie) or (ATypeID = cXbox360) then
    Result := Result + [cPicture];

  if (ATypeID = cMovie) or (ATypeID = cPCGames) then
    Result := Result + [cGenre];

  if (ATypeID = cMovie) then
    Result := Result + [cAudioStream, cRuntime, cVideoStream];
end;

function T%FullName%.InternalGetControlIDDefaultValue;
begin
  Result := True;

  { TODO : change values for default retrieval controls }
  if (cPicture = AControlID) or (cNFO = AControlID) then
    Result := False;
end;

function T%FullName%.InternalGetDependentControlIDs;
begin
  { TODO : change the dependent controls. These controls are necessarily, otherwise crawler is not used }
  Result := [cTitle];
end;

function T%FullName%.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'TODO_EXPRESSION-TO-GET-ALL-PRODUCT-PICTURES';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;
  
var
  LTitle: string;
  LCount: Integer;
  
  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  { TODO : change if you need for example the releasename. This value is related to InternalGetDependentControlIDs }
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;
  
  { TODO : update the example code here }
  // This is a very simple crawler example, see other crawlers for more information.
  LResponeStr := GETRequest(WEBSITE + 'TODO_URL' + HTTPEncode(LTitle), LRequestID1);

  if not(Pos('TODO_HAS-AT-LEAST-ONE-ITEM', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'TODO_EXPRESSION-TO-GET-DETAILED-PAGE-URL';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr); // Using a deep_search procedure to extract information

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else if not(Pos('TODO_MAYBE-DIRECT-REDIRECT-TO-DETAIL-PAGE-FOR-SPECIFIC-SEARCH-RESULT', LResponeStr) = 0) then
  begin
    deep_search(LResponeStr);
  end;
  
  Result := True;
end;

function T%FullName%.GetResultsLimitDefaultValue;
begin
  { TODO : set default value for the maximal number of pages for information retrieval }
  Result := 5;
end;

end.
