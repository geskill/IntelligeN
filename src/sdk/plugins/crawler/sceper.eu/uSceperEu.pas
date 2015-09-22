unit uSceperEu;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TSceperEu = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TSceperEu }

function TSceperEu.GetName: WideString;
begin
  Result := 'sceper.eu';
end;

function TSceperEu.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TSceperEu.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cReleaseDate, cPicture, cGenre, cDescription];

  if _TemplateTypeID in [cMovie] then
    _ComponentIDs := _ComponentIDs + [cAudioStream, cTrailer, cVideoCodec, cVideoStream];

  if _TemplateTypeID in cGames then
    _ComponentIDs := _ComponentIDs + [cVideoSystem];

  Result := LongWord(_ComponentIDs);
end;

function TSceperEu.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TSceperEu.GetLimitDefaultValue;
begin
  Result := 0;
end;

procedure TSceperEu.Exec;
const
  website = 'http://sceper.eu/';
var
  _ComponentIDs: TComponentIDs;
  _ReleaseName: string;
  _Count: Integer;

  FormatSettings: TFormatSettings;

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

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="meta">Release Info<\/div>\s+<p><a href="([^"]*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
        finally
          Free;
        end;

    if (cReleaseDate in _ComponentIDs) and Assigned(AComponentController.FindControl(cReleaseDate)) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Release Date:<\/span> (\d{2})-(\d{2})-(\d{4})<br \/>';

          if Exec(InputString) then
            AComponentController.FindControl(cReleaseDate).AddValue(DateToStr(EncodeDate(StrToIntDef(Match[3], 0), StrToIntDef(Match[2], 0),
                  StrToIntDef(Match[1], 0)), FormatSettings), GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Genre.*?<\/(span|strong)>.? (.*?)<br \/>';

          if Exec(InputString) then
            CrawlDetailedGenre(Match[2]);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cVideoCodec) <> nil) and (cVideoCodec in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Video:<\/span> (\w+) |';

          if Exec(InputString) then
            AComponentController.FindControl(cVideoCodec).AddValue(Match[1], GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Source:<\/span> (.*?)<br \/>';

          if Exec(InputString) then
            AComponentController.FindControl(cVideoStream).AddValue(Match[1], GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Audio:<\/span> \w+, (\w+), .*?<br \/>';
          if Exec(InputString) then
            AComponentController.FindControl(cAudioStream).AddValue(Match[1], GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Region:\s?<\/strong>\s?(.*?)<br \/>';

          if Exec(InputString) then
            AComponentController.FindControl(cVideoSystem).AddValue(Match[1], GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="meta">Release Description<\/div>\s+<p>(.*?)<\/p>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
        finally
          Free;
        end;

    if (AComponentController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<a href="([^"]*?)" target="_blank">Trailer<\/a>';

          if Exec(InputString) then
            AComponentController.FindControl(cTrailer).AddValue(Match[1], GetName);
        finally
          Free;
        end;
  end;

var
  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _ReleaseName := AComponentController.FindControl(cReleaseName).Value;
  // _Count := 0;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  RequestID := HTTPManager.Get(THTTPRequest.Create(website + 'search/' + HTTPEncode(_ReleaseName) + '/'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<h2 class="title">(.*?)<\/div> <!-- end .entry -->	';

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
