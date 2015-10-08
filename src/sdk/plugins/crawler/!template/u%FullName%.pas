unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  T%FullName% = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function T%FullName%.GetName;
begin
  { TODO : change name? }
  Result := '%FullName%';
end;

function T%FullName%.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  { TODO : change categories }
  _TemplateTypeIDs := [cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360, cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function T%FullName%.GetAvailableControlIDs;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  _TemplateTypeID := TTypeID(ATypeID);

  { TODO : change elements }
  _ComponentIDs := [cReleaseDate, cTitle, cNFO];

  if not(_TemplateTypeID = cXXX) then
    _ComponentIDs := _ComponentIDs + [cDescription];

  if (_TemplateTypeID = cMovie) or (_TemplateTypeID = cXbox360) then
    _ComponentIDs := _ComponentIDs + [cPicture];

  if (_TemplateTypeID = cMovie) or (_TemplateTypeID = cPCGames) then
    _ComponentIDs := _ComponentIDs + [cGenre];

  if (_TemplateTypeID = cMovie) then
    _ComponentIDs := _ComponentIDs + [cAudioStream, cRuntime, cVideoStream];

  Result := LongWord(_ComponentIDs);
end;

function T%FullName%.GetControlIDDefaultValue;
var
  _ComponentID: TControlID;
begin
  _ComponentID := TControlID(AControlID);

  Result := True;

  { TODO : change default values }
  if (cPicture = _ComponentID) or (cNFO = _ComponentID) then
    Result := False;
end;

function T%FullName%.GetResultsLimitDefaultValue;
begin
  { TODO : set default crawling site max }
  Result := 5;
end;

function T%FullName%.Exec;
begin
  { TODO : your code here }
end;

end.
