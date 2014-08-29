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
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TThaliaDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

{ TThaliaDe }

function TThaliaDe.GetName: WideString;
begin
  Result := 'Thalia.de';
end;

function TThaliaDe.GetAvailableTemplateTypeIDs: Integer;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TThaliaDe.GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cLanguage, cDescription];

  if not(_TemplateTypeID = cOther) then
    _ComponentIDs := _ComponentIDs + [cGenre];

  if (_TemplateTypeID = cMovie) then
    _ComponentIDs := _ComponentIDs + [cRuntime];

  Result := LongWord(_ComponentIDs);
end;

function TThaliaDe.GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean;
begin
  Result := True;
end;

function TThaliaDe.GetLimitDefaultValue: Integer;
begin
  Result := 5;
end;

procedure TThaliaDe.Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController);
const
  website = 'http://www.thalia.de/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
  _Title, _SearchType, _Language, _Genre: string;
  _Count: Integer;
  _ReplyData: TStringStream;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AComponentController.FindControl(cLanguage) <> nil) and (cLanguage in _ComponentIDs) then
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
                        AComponentController.FindControl(cLanguage).AddValue(Trim(Match[1]), GetName);
                      until not ExecNext;
                    end;
                  finally
                    Free;
                  end;
                end;
              end
              else
                AComponentController.FindControl(cLanguage).AddValue(_Language, GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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
                        AComponentController.FindControl(cGenre).AddValue(Trim(Match[1]), GetName);
                      until not ExecNext;
                    end;
                  finally
                    Free;
                  end;
                end;
              end
              else
                AComponentController.FindControl(cGenre).AddValue(_Genre, GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Spieldauer:<\/strong> (\d+) Minuten<\/li>';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName)
          else
          begin
            Expression := 'Spieldauer:<\/strong> (\d+):(\d+):(\d+)<\/li>';

            if Exec(InputString) then
              AComponentController.FindControl(cRuntime).AddValue(IntToStr(StrToInt(Match[1])) + Match[2], GetName);
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div id="tab-content_content" class="detailLayer">.*?<p>(.*?)<\/p>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div class="detailImageContainer" style="width:200px; height:300px;">\s+<a href="(.*?)"';

          if Exec(InputString) then
          begin
            AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
          end;
        finally
          Free;
        end;
  end;

begin
  _TemplateTypeID := TTemplateTypeID(ATemplateTypeID);
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  with TIdHTTPHelper.Create(Self) do
    try
      Request.Referer := website;

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

      _ReplyData := TStringStream.Create('');
      try
        Get(website + 'shop/tha_homestartseite/suche/?sswg=' + HTTPEncode(_SearchType) + '&sq=' + HTTPEncode(_Title), _ReplyData);

        with TRegExpr.Create do
          try
            InputString := _ReplyData.DataString;
            if _TemplateTypeID in [cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360] then
              Expression := '<h2><a href="(.*?)".*?Medium: ([\w ]+)'
            else
              Expression := '<h2><a href="(.*?)"';
            if Exec(InputString) then
            begin
              repeat
                if _TemplateTypeID in [cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360] then
                begin
                  if ((_TemplateTypeID = cNintendoDS) and (Match[2] = 'Nintendo DS')) or
                    ((_TemplateTypeID in [cGameCube, cWii]) and ((Match[2] = 'GameCube') or (Match[2] = 'Nintendo Wii'))) or
                    ((_TemplateTypeID = cPCGames) and ((Match[2] = 'DVD') or (Match[2] = 'CD-ROM') or (Match[2] = 'PC Games'))) or
                    ((_TemplateTypeID in [cPlayStation2, cPlayStation3]) and ((Match[2] = 'PS2') or (Match[2] = 'PlayStation 2') or
                        (Match[2] = 'PlayStation 3'))) or //
                    ((_TemplateTypeID = cPlayStationPortable) and ((Match[2] = 'PSP (Playstation Portable)') or (Match[2] = 'UMD'))) or
                    ((_TemplateTypeID in [cXbox, cXbox360]) and ((Match[2] = 'Xbox') or (Match[2] = 'XBOX 360'))) then
                    deep_search(Get(Match[1]))
                end
                else
                  deep_search(Get(Match[1]));
                Inc(_Count);
              until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
            end;
          finally
            Free;
          end;

      finally
        _ReplyData.Free;
      end;
    finally
      Free;
    end;
end;

end.
