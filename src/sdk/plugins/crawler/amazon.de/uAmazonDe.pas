unit uAmazonDe;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInCrawlerClass, uAmazonCom;

type
  TAmazonDe = class(TAmazonCom)
  public
    function GetName: WideString; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TAmazonDe }

function TAmazonDe.GetName: WideString;
begin
  Result := 'Amazon.de';
end;

function TAmazonDe.Exec;
const
  website = 'http://www.amazon.de/';
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;

  _Title, _search_alias: string;
  _Count: Integer;

  ResponseStrSearchResult: string;
  RequestID: Double;

  procedure deep_search(aWebsitecode: string);
  var
    _tracklist: string;
  begin
    if (AControlController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<li> <b>Spieldauer:<\/b> (\d+) Minuten<\/li>';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
    if (AControlController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<li> <b>Format:<\/b> (.*?)<\/li>';

          if Exec(InputString) then
          begin
            if (Pos('NTSC', string(Match[1])) > 0) then
              AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
            else if (Pos('PAL', string(Match[1])) > 0) then
              AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL');
          end;
        finally
          Free;
        end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
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
            AControlController.FindControl(cDescription).AddProposedValue(GetName, copy(_tracklist, 1, length(_tracklist) - 2));
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
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(AmazonHTMLDescription2Text(Match[1])));
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'i\.src = "(.*?)"';

          if Exec(InputString) then
          begin
            if not(Pos('no-img', string(Match[1])) > 0) then
              AControlController.FindControl(cPicture).AddProposedValue(GetName, AmazonOriginalSize(Match[1]));
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
  _TemplateTypeID := TTypeID(ATypeID);
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
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
