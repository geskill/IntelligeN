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
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils;

type
  T%FullName% = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = '%Website%';
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
  { TODO : change elements }
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

  { TODO : change default values }
  if (cPicture = AControlID) or (cNFO = AControlID) then
    Result := False;
end;

function T%FullName%.InternalGetDependentControlIDs;
begin
  { TODO : change the dependent controls }
  Result := [cReleaseName];
end;

function T%FullName%.InternalExecute;
begin
  { TODO : your code here }
end;

function T%FullName%.GetResultsLimitDefaultValue;
begin
  { TODO : set default crawling site max }
  Result := 5;
end;

end.
