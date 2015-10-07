unit uImdbCom;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TImdbCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

{ TImdbCom }

function TImdbCom.GetName;
begin
  Result := 'Imdb.com';
end;

function TImdbCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TImdbCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TImdbCom.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TImdbCom.GetResultsLimitDefaultValue: Integer;
begin
  Result := 0;
end;

procedure TImdbCom.Exec;
const
  website = 'http://www.imdb.com/';
var
  _ComponentIDs: TControlIDs;
  _Title: string;

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

  function GetIMDBLargePicture(ASmallPicture: string): string;
  begin
    Result := copy(ASmallPicture, 1, Pos('V1._', ASmallPicture)) + '1_.jpg';
  end;

  procedure deep_search(AWebsiteSourceCode: string);
  begin

    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<span class="outline">(.*?)<\/span>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;

    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<span class="genre">(.*?)<\/span>';

          if Exec(InputString) then
            CrawlDetailedGenre(Trim(HTML2Text(Match[1], False)));
        finally
          Free;
        end;

    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img src="([^"]*)"';

          if Exec(InputString) then
            if Pos('@', string(Match[1])) > 0 then // some films have two @@ some single @
              AControlController.FindControl(cPicture).AddProposedValue(GetName, GetIMDBLargePicture(Match[1]));
        finally
          Free;
        end;
  end;

var
  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  // _Count := 0;

  RequestID := HTTPManager.Get(THTTPRequest.Create(website + 'search/title?title=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<td class="number">(.*?)<\/tr>';

      // <img src="([^"]*)".*?<span class="outline">(.*?)<\/span>.*?<span class="genre">(.*?)<\/span>

      if Exec(InputString) then
      begin
        repeat

          deep_search(Match[1]);

          // Inc(_Count);
        until not ExecNext; // not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
