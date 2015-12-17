unit uGametrailersCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGametrailersCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.gametrailers.com/';
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

{ TGametrailersCom }

function TGametrailersCom.GetName;
begin
  Result := 'gametrailers.com';
end;

function TGametrailersCom.InternalGetAvailableTypeIDs;
begin
  Result := cGames;
end;

function TGametrailersCom.InternalGetAvailableControlIDs;
begin
  Result := [cTrailer];
end;

function TGametrailersCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TGametrailersCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TGametrailersCom.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if ACanUse(cTrailer) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<h3 class="MovieTitle">\s+<a href="(.*?)" class="gamepage_content_row_title">(.*?)<';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cTrailer).AddProposedValue(GetName, WEBSITE + Match[1], Trim(Match[2]));
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // not very useful wrapper for google search...
  LResponeStr := GETRequest(WEBSITE + 'global/search/?q=' + HTTPEncode(LTitle) + HTTPEncode(' ' + TypeIDToString(ATypeID)), LRequestID1);

  if (Pos('No results', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'class="search_game_row_thumb">\s+<a href="(.*?)">';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TGametrailersCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
