unit uGamespotCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGamespotCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

function TGamespotCom.GetName;
begin
  result := 'gamespot.com';
end;

function TGamespotCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox360];
  result := Word(_TemplateTypeIDs);
end;

function TGamespotCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre];
  result := LongWord(_ComponentIDs);
end;

function TGamespotCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TGamespotCom.GetResultsLimitDefaultValue;
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
  _searchname := AControlController.FindControl(cReleaseName).Value;
  if (_searchname = '') then
    _searchname := AControlController.FindControl(cTitle).Value;

  _Count := 0;

  //s := Get(website + 'search.html?qs=' + HTTPEncode(_searchname));
end;

end.
