unit uCustomScript;

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
  // Plugin system
  uPlugInCrawlerClass;

type
  TCustomScript = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TCustomScript.GetName;
begin
  Result := 'CustomScript';
end;

function TCustomScript.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  Result := Word(_TemplateTypeIDs);
end;

function TCustomScript.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [ low(TComponentID) .. high(TComponentID)];
  Result := LongWord(_ComponentIDs);
end;

function TCustomScript.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TCustomScript.GetLimitDefaultValue;
begin
  Result := 0;
end;

procedure TCustomScript.Exec;
begin
  { TODO : your code here }
end;

end.
