unit uJadedvideoCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TJadedvideoCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TJadedvideoCom.GetName;
begin
  result := 'Jadedvideo.com';
end;

function TJadedvideoCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TJadedvideoCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre];
  result := LongWord(_ComponentIDs);
end;

function TJadedvideoCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TJadedvideoCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TJadedvideoCom.Exec;
const
  jvurl = 'http://www.jadedvideo.com/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(AWebsiteSourceCode: string);

    procedure ExtractGenres(AGenreCode: string);
    begin
      with TRegExpr.Create do
        try
          InputString := AGenreCode;
          Expression := '">(.*?)<\/A>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<SPAN class="eight_pt">\s+<IMG src="(.*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Categories:(.*?)<\/TD>';

          if Exec(InputString) then
            ExtractGenres(Match[1]);
        finally
          Free;
        end;
  end;

var
  RequestID1, RequestID2, RequestID3: Double;

  HTTPParams: IHTTPParams;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(jvurl + 'vap/Adult_Content_Accept.html'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('productNo', '');
    AddFormField('legacyProductId', '');
    AddFormField('title', _Title);
    AddFormField('titleMatchTypeCd', '1');
    AddFormField('castNames', '');
    AddFormField('castMatchTypeCd', '1');
    AddFormField('directorName', '');
  end;

  RequestID2 := HTTPManager.Post(jvurl + 'info/Advanced_Search.html', RequestID1, HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID2);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode;

  if not(Pos('SEARCH RESULTS', ResponseStrSearchResult) = 0) then
  begin
    with TRegExpr.Create do
      try
        ModifierS := True;
        ModifierG := False;
        InputString := ResponseStrSearchResult;
        Expression := '<TD width="124" align="left" valign="top">\s+<A href="(.*?)"';

        if Exec(InputString) then
        begin
          repeat

            RequestID3 := HTTPManager.Get(jvurl + Match[1], RequestID2, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID3);

            deep_search(HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode);

            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else
    deep_search(ResponseStrSearchResult);
end;

end.
