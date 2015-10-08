unit uThaliaDe;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TThaliaDe = class(TCrawlerPlugIn)
  protected
    function ThaliaDetailedPageRequest(AFollowUpRequest: Double; AWebsite: string): string;
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TThaliaDe }

function TThaliaDe.ThaliaDetailedPageRequest;
var
  RequestID: Double;
begin
  RequestID := HTTPManager.Get(AWebsite, AFollowUpRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  Result := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
end;

function TThaliaDe.GetName;
begin
  Result := 'Thalia.de';
end;

function TThaliaDe.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)] - [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TThaliaDe.GetAvailableControlIDs;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cPicture, cLanguage, cDescription];

  if not(_TemplateTypeID = cOther) then
    _ComponentIDs := _ComponentIDs + [cGenre];

  if (_TemplateTypeID = cMovie) then
    _ComponentIDs := _ComponentIDs + [cRuntime];

  Result := LongWord(_ComponentIDs);
end;

function TThaliaDe.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TThaliaDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

function TThaliaDe.Exec;
const
  website = 'http://www.thalia.de/';
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
  _Title, _SearchType, _Language, _Genre: string;
  _Count: Integer;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AControlController.FindControl(cLanguage) <> nil) and (cLanguage in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Sprache\(n\):<\/strong> (.*?)<\/li>';

          if Exec(InputString) then
          begin
            repeat
              _Language := Match[1];
              if (Pos('/', _Language) > 0) or (Pos(',', _Language) > 0) or (Pos('|', _Language) > 0) then
              begin
                with TRegExpr.Create do
                begin
                  try
                    InputString := _Language;
                    Expression := '([^\/,|]+)';

                    if Exec(InputString) then
                    begin
                      repeat
                        AControlController.FindControl(cLanguage).AddProposedValue(GetName, Trim(Match[1]));
                      until not ExecNext;
                    end;
                  finally
                    Free;
                  end;
                end;
              end
              else
                AControlController.FindControl(cLanguage).AddProposedValue(GetName, _Language);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Stilrichtung:<\/strong> (.*?)<\/li>';

          if Exec(InputString) then
          begin
            repeat
              _Genre := Match[1];
              if (Pos('/', _Genre) > 0) or (Pos(',', _Genre) > 0) or (Pos('|', _Genre) > 0) then
              begin
                with TRegExpr.Create do
                begin
                  try
                    InputString := _Genre;
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
                end;
              end
              else
                AControlController.FindControl(cGenre).AddProposedValue(GetName, _Genre);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AControlController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Spieldauer:<\/strong> (\d+) Minuten<\/li>';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1], GetName)
          else
          begin
            Expression := 'Spieldauer:<\/strong> (\d+):(\d+):(\d+)<\/li>';

            if Exec(InputString) then
              AControlController.FindControl(cRuntime).AddProposedValue(GetName, IntToStr(StrToInt(Match[1])) + Match[2]);
          end;
        finally
          Free;
        end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div id="tab-content_content" class="detailLayer">.*?<p>(.*?)<\/p>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div class="detailImageContainer" style="width:200px; height:300px;">\s+<a href="(.*?)"';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
          end;
        finally
          Free;
        end;
  end;

var
  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  _TemplateTypeID := TTypeID(ATypeID);
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  (*
    <option value="ANY" >in allen Kategorien</option>
    <option value="BUCH" >in B&#252;chern</option>
    <option value="HOERBUCH" >in H&#246;rb&#252;chern</option>
    <option value="EBOOK" >in Ebooks</option>
    <option value="MUSIK" >in Musik</option>
    <option value="FILM"  selected>in Film</option>
    <option value="SOFTWARE" >in Software</option>
    <option value="SPIEL" >in Games</option>
    <option value="GESELLSCHAFTSSPIEL" >in Spielwaren</option>
    *)

  case _TemplateTypeID of
    cAudio:
      _SearchType := 'MUSIK';
    cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360:
      _SearchType := 'SPIEL';
    cMovie:
      _SearchType := 'FILM';
    cSoftware:
      _SearchType := 'SOFTWARE';
    cOther:
      _SearchType := 'ANY';
  end;

  HTTPRequest := THTTPRequest.Create(website + 'shop/tha_homestartseite/suche/?sswg=' + HTTPEncode(_SearchType) + '&sq=' + HTTPEncode(_Title));
  HTTPRequest.Referer := website;

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      if _TemplateTypeID in cGames then
        Expression := '<h2><a href="(.*?)".*?Medium: ([\w ]+)'
      else
        Expression := '<h2><a href="(.*?)"';
      if Exec(InputString) then
      begin
        repeat
          if _TemplateTypeID in cGames then
          begin
            if ((_TemplateTypeID = cNintendoDS) and (Match[2] = 'Nintendo DS')) or { }
            ((_TemplateTypeID in [cGameCube, cWii]) and ((Match[2] = 'GameCube') or (Match[2] = 'Nintendo Wii'))) or { }
            ((_TemplateTypeID = cPCGames) and ((Match[2] = 'DVD') or (Match[2] = 'CD-ROM') or (Match[2] = 'PC Games'))) or { }
            ((_TemplateTypeID in [cPlayStation2, cPlayStation3]) and ((Match[2] = 'PS2') or (Match[2] = 'PlayStation 2') or (Match[2] = 'PlayStation 3'))) or
            { }
            ((_TemplateTypeID = cPlayStationPortable) and ((Match[2] = 'PSP (Playstation Portable)') or (Match[2] = 'UMD'))) or { }
            ((_TemplateTypeID in [cXbox, cXbox360]) and ((Match[2] = 'Xbox') or (Match[2] = 'XBOX 360'))) then
              deep_search(ThaliaDetailedPageRequest(RequestID, Match[1]))
          end
          else
            deep_search(ThaliaDetailedPageRequest(RequestID, Match[1]));
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
