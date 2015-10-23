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
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInCrawlerClass;

type
  TChipDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TChipDe }

function TChipDe.GetName: WideString;
begin
  Result := 'chip.de';
end;

function TChipDe.GetAvailableTypeIDs: Integer;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cSoftware];
  Result := LongWord(_TemplateTypeIDs);
end;

function TChipDe.GetAvailableControlIDs(const ATypeID: Integer): Integer;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TChipDe.GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool;
begin
  Result := True;
end;

function TChipDe.GetResultsLimitDefaultValue: Integer;
begin
  Result := 5;
end;

function TChipDe.Exec;
const
  website = 'http://www.chip.de/';
begin

end;

end.
