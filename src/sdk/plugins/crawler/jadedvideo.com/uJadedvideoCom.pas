unit uJadedvideoCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TJadedvideoCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TJadedvideoCom.GetName;
begin
  result := 'Jadedvideo.com';
end;

function TJadedvideoCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TJadedvideoCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre];
  result := LongWord(_ComponentIDs);
end;

function TJadedvideoCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TJadedvideoCom.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

function TJadedvideoCom.Exec;
const
  jvurl = 'http://www.jadedvideo.com/';
var
  _ComponentIDs: TControlIDs;
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
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<SPAN class="eight_pt">\s+<IMG src="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
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
