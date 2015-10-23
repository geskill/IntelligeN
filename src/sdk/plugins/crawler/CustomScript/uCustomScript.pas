unit uCustomScript;

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
  // Plugin system
  uPlugInCrawlerClass;

type
  TCustomScript = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TCustomScript.GetName;
begin
  Result := 'CustomScript';
end;

function TCustomScript.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)];
  Result := LongWord(_TemplateTypeIDs);
end;

function TCustomScript.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [ low(TControlID) .. high(TControlID)];
  Result := LongWord(_ComponentIDs);
end;

function TCustomScript.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TCustomScript.GetResultsLimitDefaultValue;
begin
  Result := 0;
end;

function TCustomScript.Exec;
begin
  { TODO : your code here }
end;

end.
