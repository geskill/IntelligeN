unit uGamespotCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGamespotCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TGamespotCom.GetName;
begin
  result := 'gamespot.com';
end;

function TGamespotCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox360];
  result := Word(_TemplateTypeIDs);
end;

function TGamespotCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre];
  result := LongWord(_ComponentIDs);
end;

function TGamespotCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TGamespotCom.GetLimitDefaultValue;
begin
  result := 0;
end;

procedure TGamespotCom.Exec;
const
  website = 'http://www.gamespot.com/';
var
  _searchname, s: string;
  _Count: Integer;
begin
  _searchname := AComponentController.FindControl(cReleaseName).Value;
  if (_searchname = '') then
    _searchname := AComponentController.FindControl(cTitle).Value;

  _Count := 0;

  //s := Get(website + 'search.html?qs=' + HTTPEncode(_searchname));
end;

end.
