unit uAdultdvdempireCom;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
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
  TAdultdvdempireCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.adultdvdempire.com/';
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

{ TAdultdvdempireCom }

function TAdultdvdempireCom.GetName;
begin
  Result := 'adultdvdempire.com';
end;

function TAdultdvdempireCom.InternalGetAvailableTypeIDs;
begin
  Result := [cXXX];
end;

function TAdultdvdempireCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cDescription];
end;

function TAdultdvdempireCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TAdultdvdempireCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TAdultdvdempireCom.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);

    function MakeLinebreaks(const AHTMLText: string): string;
    begin
      Result := StringReplace(AHTMLText, '</p><p>', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;

  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'property=''og:image'' content="(.*?)"';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], 'h.', 'bh.', [rfIgnoreCase]));
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '"Category" \/>(.*?)<\/';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(Match[1]));
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<h4.*?>(.*?)<\/h4>';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(ReduceWhitespace(HTML2Text(MakeLinebreaks(Match[1])))));
          end;
        finally
          Free;
        end;
    end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LSearchString: string;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // http://www.adultdvdempire.com/allsearch/search?q=Mollys%20Life%2018

  LResponeStr := GETRequest(WEBSITE + 'allsearch/search?sort=released&q=' + HTTPEncode(LTitle), LRequestID1);

  if not(Pos('searchRefineForm', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'item-cover"><a href=" \/(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

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

function TAdultdvdempireCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
