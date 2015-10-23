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
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TAdultdvdempireCom }

function TAdultdvdempireCom.GetName;
begin
  Result := 'adultdvdempire.com';
end;

function TAdultdvdempireCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  Result := LongWord(_TemplateTypeIDs);
end;

function TAdultdvdempireCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];

  Result := LongWord(_ComponentIDs);
end;

function TAdultdvdempireCom.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TAdultdvdempireCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

function TAdultdvdempireCom.Exec;
const
  website = 'http://www.adultdvdempire.com/';
var
  _ComponentIDs: TControlIDs;
  _Title: string;
  _Count: Integer;

  procedure CrawlDetailedGenre(AGenreList: string);
  begin
    if (Pos('/', AGenreList) > 0) or (Pos(',', AGenreList) > 0) or (Pos('|', AGenreList) > 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AGenreList;
          Expression := '([^\/,|]+)';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(Match[1]));
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end
    else
      AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(AGenreList));
  end;

  procedure deep_search(aWebsitecode: string);

    function MakeLinebreaks(AHTMLText: string): string;
    begin
      Result := StringReplace(AHTMLText, '</p><p>', sLineBreak, [rfReplaceAll, rfIgnoreCase])
    end;

  begin
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<p class="Tagline">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(ReduceWhitespace(HTML2Text(MakeLinebreaks(Match[1])))));
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Categories<\/h2>(.*?)<h2>';

          if Exec(InputString) then
            CrawlDetailedGenre(Trim(ReduceWhitespace(HTML2Text(Match[1], False))));
        finally
          Free;
        end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<img src="(.*?)"';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], 'm.', 'h.', []));
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], 'm.', 'bh.', []));
          end;
        finally
          Free;
        end;
  end;

var
  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  // http://www.adultdvdempire.com/allsearch/search?q=Mollys%20Life%2018

  HTTPRequest := THTTPRequest.Create(website + 'allsearch/search?sort=released&exactMatch=' + HTTPEncode(_Title) + '&q=' + HTTPEncode(_Title));
  HTTPRequest.Referer := website;

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<p class="title"><a href="\/(.*?)"';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
