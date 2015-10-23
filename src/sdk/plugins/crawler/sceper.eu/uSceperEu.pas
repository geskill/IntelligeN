unit uSceperEu;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TSceperEu = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TSceperEu }

function TSceperEu.GetName: WideString;
begin
  Result := 'sceper.eu';
end;

function TSceperEu.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)] - [cXXX];
  Result := LongWord(_TemplateTypeIDs);
end;

function TSceperEu.GetAvailableControlIDs;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cReleaseDate, cPicture, cGenre, cDescription];

  if _TemplateTypeID in [cMovie] then
    _ComponentIDs := _ComponentIDs + [cAudioStream, cTrailer, cVideoCodec, cVideoStream];

  if _TemplateTypeID in cGames then
    _ComponentIDs := _ComponentIDs + [cVideoSystem];

  Result := LongWord(_ComponentIDs);
end;

function TSceperEu.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TSceperEu.GetResultsLimitDefaultValue;
begin
  Result := 0;
end;

function TSceperEu.Exec;
const
  website = 'http://sceper.eu/';
var
  _ComponentIDs: TControlIDs;
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

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="meta">Release Info<\/div>\s+<p><a href="([^"]*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if (cReleaseDate in _ComponentIDs) and Assigned(AControlController.FindControl(cReleaseDate)) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Release Date:<\/span> (\d{2})-(\d{2})-(\d{4})<br \/>';

          if Exec(InputString) then
            AControlController.FindControl(cReleaseDate).AddProposedValue(GetName, DateToStr(EncodeDate(StrToIntDef(Match[3], 0), StrToIntDef(Match[2], 0),
                  StrToIntDef(Match[1], 0)), FormatSettings));
        finally
          Free;
        end;

    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Genre.*?<\/(span|strong)>.? (.*?)<br \/>';

          if Exec(InputString) then
            CrawlDetailedGenre(Match[2]);
        finally
          Free;
        end;

    if (AControlController.FindControl(cVideoCodec) <> nil) and (cVideoCodec in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Video:<\/span> (\w+) |';

          if Exec(InputString) then
            AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if (AControlController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Source:<\/span> (.*?)<br \/>';

          if Exec(InputString) then
            AControlController.FindControl(cVideoStream).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if (AControlController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'bold">Audio:<\/span> \w+, (\w+), .*?<br \/>';
          if Exec(InputString) then
            AControlController.FindControl(cAudioStream).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if (AControlController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Region:\s?<\/strong>\s?(.*?)<br \/>';

          if Exec(InputString) then
            AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="meta">Release Description<\/div>\s+<p>(.*?)<\/p>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;

    if (AControlController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<a href="([^"]*?)" target="_blank">Trailer<\/a>';

          if Exec(InputString) then
            AControlController.FindControl(cTrailer).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
  end;

var
  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _ReleaseName := AControlController.FindControl(cReleaseName).Value;
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
