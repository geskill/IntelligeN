unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  T%FullName% = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function T%FullName%.GetName;
begin
  { TODO : change name? }
  Result := '%FullName%';
end;

function T%FullName%.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  { TODO : change categories }
  _TemplateTypeIDs := [cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360, cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function T%FullName%.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

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

function T%FullName%.GetComponentIDDefaultValue;
var
  _ComponentID: TComponentID;
begin
  _ComponentID := TComponentID(ComponentID);

  Result := True;

  { TODO : change default values }
  if (cPicture = _ComponentID) or (cNFO = _ComponentID) then
    Result := False;
end;

function T%FullName%.GetLimitDefaultValue;
begin
  { TODO : set default crawling site max }
  Result := 5;
end;

procedure T%FullName%.Exec;
begin
  { TODO : your code here }
end;

end.
