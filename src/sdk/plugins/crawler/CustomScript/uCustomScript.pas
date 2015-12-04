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

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

function TCustomScript.GetName;
begin
  Result := 'CustomScript';
end;

function TCustomScript.InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool;
begin
  {TODO: Add IScript reader}
end;

function TCustomScript.GetResultsLimitDefaultValue;
begin
  Result := 0;
end;

end.
