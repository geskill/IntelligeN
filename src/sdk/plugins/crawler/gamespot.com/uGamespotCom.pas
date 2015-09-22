unit uGamespotCom;

interface

uses
  // Delphi
  SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass;

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
  result := Word(_ComponentIDs);
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

  with TIdHTTP.Create(nil) do
    try
      HandleRedirects := True;

      s := Get(website + 'search.html?qs=' + HTTPEncode(_searchname));

      //
    finally
      Free;
    end;
end;

end.
