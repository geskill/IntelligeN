unit uImdbCom;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TImdbCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TImdbCom }

function TImdbCom.GetName;
begin
  Result := 'Imdb.com';
end;

function TImdbCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TImdbCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TImdbCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TImdbCom.GetLimitDefaultValue: Integer;
begin
  Result := 0;
end;

procedure TImdbCom.Exec;
const
  website = 'http://www.imdb.com/';
var
  _ComponentIDs: TComponentIDs;
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

  function GetIMDBLargePicture(ASmallPicture: string): string;
  begin
    Result := copy(ASmallPicture, 1, Pos('V1._', ASmallPicture)) + '1_.jpg';
  end;

  procedure deep_search(AWebsiteSourceCode: string);
  begin

    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<span class="outline">(.*?)<\/span>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<span class="genre">(.*?)<\/span>';

          if Exec(InputString) then
            CrawlDetailedGenre(Trim(HTML2Text(Match[1], False)));
        finally
          Free;
        end;

    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img src="([^"]*)"';

          if Exec(InputString) then
            if Pos('@', string(Match[1])) > 0 then // some films have two @@ some single @
              AComponentController.FindControl(cPicture).AddValue(GetIMDBLargePicture(Match[1]), GetName);
        finally
          Free;
        end;
  end;

var
  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
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
