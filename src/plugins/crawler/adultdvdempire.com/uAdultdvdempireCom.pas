unit uAdultdvdempireCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TAdultdvdempireCom = class(TCrawlerPlugIn)
  protected
    function SimpleGETRequest(AURL: string; AIdHTTPHelper: TIdHTTPHelper): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TAdultdvdempireCom }

function TAdultdvdempireCom.SimpleGETRequest;
var
  ReplyData: TStringStream;
begin
  ReplyData := TStringStream.Create('', CP_UTF8);
  try
    AIdHTTPHelper.Get(AURL, ReplyData);

    Result := ReplyData.DataString;
  finally
    ReplyData.Free;
  end;
end;

function TAdultdvdempireCom.GetName;
begin
  Result := 'adultdvdempire.com';
end;

function TAdultdvdempireCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TAdultdvdempireCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];

  Result := LongWord(_ComponentIDs);
end;

function TAdultdvdempireCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TAdultdvdempireCom.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TAdultdvdempireCom.Exec;
const
  website = 'http://www.adultdvdempire.com/';
var
  _ComponentIDs: TComponentIDs;
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
              AComponentController.FindControl(cGenre).AddValue(Trim(Match[1]), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end
    else
      AComponentController.FindControl(cGenre).AddValue(Trim(AGenreList), GetName);
  end;

  procedure deep_search(aWebsitecode: string);

    function MakeLinebreaks(AHTMLText: string): string;
    begin
      Result := StringReplace(AHTMLText, '</p><p>', sLineBreak, [rfReplaceAll, rfIgnoreCase])
    end;

  begin
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<p class="Tagline">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cDescription).AddValue(Trim(ReduceWhitespace(HTML2Text(MakeLinebreaks(Match[1])))), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Categories<\/h2>(.*?)<h2>';

          if Exec(InputString) then
            CrawlDetailedGenre(Trim(ReduceWhitespace(HTML2Text(Match[1], False))));
        finally
          Free;
        end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<img src="(.*?)"';

          if Exec(InputString) then
          begin
            AComponentController.FindControl(cPicture).AddValue(StringReplace(Match[1], 'm.', 'h.', []), GetName);
            AComponentController.FindControl(cPicture).AddValue(StringReplace(Match[1], 'm.', 'bh.', []), GetName);
          end;
        finally
          Free;
        end;
  end;

var
  IdHTTPHelper: TIdHTTPHelper;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  // http://www.adultdvdempire.com/allsearch/search?q=Mollys%20Life%2018

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    IdHTTPHelper.Request.Referer := website;

    ResponseStrSearchResult := SimpleGETRequest(website + 'allsearch/search?sort=released&exactMatch=' + HTTPEncode(_Title) + '&q=' + HTTPEncode(_Title), IdHTTPHelper);

    with TRegExpr.Create do
      try
        InputString := ResponseStrSearchResult;
        Expression := '<p class="title"><a href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat

            deep_search(SimpleGETRequest(website + Match[1], IdHTTPHelper));

            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  finally
    IdHTTPHelper.Free;
  end;
end;

end.
