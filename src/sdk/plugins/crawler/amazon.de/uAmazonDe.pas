unit uAmazonDe;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uAmazonCom;

type
  TAmazonDe = class(TAmazonCom)
  public
    function GetName: WideString; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TAmazonDe }

function TAmazonDe.GetName: WideString;
begin
  Result := 'Amazon.de';
end;

procedure TAmazonDe.Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController);
const
  website = 'http://www.amazon.de/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;

  _Title, _search_alias: string;
  _Count: Integer;

  ResponseStrSearchResult: string;
  RequestID: Double;

  procedure deep_search(aWebsitecode: string);
  var
    _tracklist: string;
  begin
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<li> <b>Spieldauer:<\/b> (\d+) Minuten<\/li>';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<li> <b>Format:<\/b> (.*?)<\/li>';

          if Exec(InputString) then
          begin
            if (Pos('NTSC', string(Match[1])) > 0) then
              AComponentController.FindControl(cVideoSystem).AddValue('NTSC', GetName)
            else if (Pos('PAL', string(Match[1])) > 0) then
              AComponentController.FindControl(cVideoSystem).AddValue('PAL', GetName);
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<tr class="\w+">\s+<td>\s+(.*?)\s<\/td>';

          if Exec(InputString) then
          begin
            _tracklist := '';
            repeat
              _tracklist := _tracklist + Trim(Match[1]) + sLineBreak;
            until not ExecNext;
            AComponentController.FindControl(cDescription).AddValue(copy(_tracklist, 1, length(_tracklist) - 2), GetName);
          end;
        finally
          Free;
        end;
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div class="productDescriptionWrapper"[ ]?>(.*?)<div class="emptyClear"';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cDescription).AddValue(Trim(AmazonHTMLDescription2Text(Match[1])), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'i\.src = "(.*?)"';

          if Exec(InputString) then
          begin
            if not(Pos('no-img', string(Match[1])) > 0) then
              AComponentController.FindControl(cPicture).AddValue(AmazonOriginalSize(Match[1]), GetName);
          end;
        finally
          Free;
        end;
  end;

  procedure other_search(aWebsitecode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := aWebsitecode;

        Expression := '<a class="title" href="(.*?)">.*?<\/a>';
        if Exec(InputString) then
          deep_search(AmazonDetailedPageRequest(RequestID, Match[1]));

        Expression := '<a href="(.*?)">.*?<\/a>';
        if Exec(InputString) then
          deep_search(AmazonDetailedPageRequest(RequestID, Match[1]));
      finally
        Free;
      end;
  end;

  procedure game_table_search(aWebsitecode: string; AFilter: Boolean = False);
  begin
    with TRegExpr.Create do
      try
        InputString := aWebsitecode;
        Expression := '<td class="tpType">\s+<a .*?href="(.*?)">(.*?)<\/a>';

        if Exec(InputString) then
        begin
          repeat
            if (not AFilter) or (AFilter and (_TemplateTypeID = AmazonExtractGameCategory(Match[2]))) then
              deep_search(AmazonDetailedPageRequest(RequestID, Match[1]));
          until not ExecNext;
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

  case _TemplateTypeID of
    cAudio:
      _search_alias := 'search-alias=popular';
    cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360:
      _search_alias := 'search-alias=videogames';
    cMovie:
      _search_alias := 'search-alias=dvd';
    cSoftware:
      _search_alias := 'search-alias=software';
    cOther:
      _search_alias := 'search-alias=aps';
  end;

  ResponseStrSearchResult := AmazonSearchRequest(website, _search_alias, _Title, RequestID);

  with TRegExpr.Create do
  begin
    try
      InputString := ResponseStrSearchResult;
      Expression := 'div id="result_\d+"(.*?)(clear="all"|class="unfloat")';

      if Exec(InputString) then
      begin
        repeat

          if _TemplateTypeID in cGames then
          begin
            if Pos('class="formats" colspan=2>Plattformen', string(Match[1])) > 0 then
              game_table_search(Match[1], True)

            else if _TemplateTypeID = AmazonExtractGameCategory(Match[1], True) then
              game_table_search(Match[1]);
          end
          else
            other_search(Match[1]);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;

    finally
      Free;
    end;
  end;
end;

end.
