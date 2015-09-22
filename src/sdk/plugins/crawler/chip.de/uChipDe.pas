unit uChipDe;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass;

type
  TChipDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TChipDe }

function TChipDe.GetName: WideString;
begin
  Result := 'chip.de';
end;

function TChipDe.GetAvailableTemplateTypeIDs: Integer;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cSoftware];
  Result := Word(_TemplateTypeIDs);
end;

function TChipDe.GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TChipDe.GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean;
begin
  Result := True;
end;

function TChipDe.GetLimitDefaultValue: Integer;
begin
  Result := 5;
end;

procedure TChipDe.Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController);
const
  website = 'http://www.chip.de/';
begin
  with TIdHTTPHelper.Create(Self) do
    try
      //
    finally
      Free;
    end
end;

end.
