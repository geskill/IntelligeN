unit uYoutubeCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TYoutubeCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'https://www.youtube.com/';
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

{ TYoutubeCom }

function TYoutubeCom.GetName;
begin
  result := 'Youtube.com';
end;

function TYoutubeCom.InternalGetAvailableTypeIDs;
begin
  result := [ low(TTypeID) .. high(TTypeID)] - [cEBook, cXXX];
end;

function TYoutubeCom.InternalGetAvailableControlIDs;
begin
  result := [cTrailer];
end;

function TYoutubeCom.InternalGetControlIDDefaultValue;
begin
  result := True;
end;

function TYoutubeCom.InternalGetDependentControlIDs;
begin
  result := [cTitle];
end;

function TYoutubeCom.InternalExecute;
var
  LTitle: string;
  LCount: Integer;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  if ACanUse(cTrailer) then
  begin
    with TRegExpr.Create do
    begin
      try
        ModifierS := False;
        InputString := ExtractTextBetween(SimpleGETRequest(WEBSITE + 'results?search_type=videos&search_query=' + HTTPEncode(LTitle + ' hd trailer')), '"item-section">', '</ol>');
        Expression := '<h3(.*?)href="\/(.*?)"(.*?)dir="ltr">(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cTrailer).AddProposedValue(GetName, WEBSITE + Match[2], HTML2Text(Match[4]));
            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
    end;
  end;

  Result := True;
end;

function TYoutubeCom.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

end.
