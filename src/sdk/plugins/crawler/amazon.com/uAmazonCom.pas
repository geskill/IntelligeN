unit uAmazonCom;

interface

uses
  // Delphi
  Windows, SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TAmazonCom = class(TCrawlerPlugIn)
  protected
    function AmazonHTMLDescription2Text(AHtmlContent: string): string;
    function AmazonOriginalSize(ASizedImage: string): string;
    function AmazonSearchRequest(AWebsite, ASearchAlias, ATitle: string; out AFollowUpRequest: Double): string;
    function AmazonExtractGameCategory(AHTML: string; AInsideHTML: Boolean = False): TTemplateTypeID;
    function AmazonDetailedPageRequest(AFollowUpRequest: Double; AWebsite: string): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TAmazonCom }

function TAmazonCom.AmazonHTMLDescription2Text;
var
  Text: string;
begin
  Text := HTML2Text(AHtmlContent, True, True);
  try
    Result := HTMLDecode(Text);
  except
    Result := Text;
  end;
end;

function TAmazonCom.AmazonOriginalSize;
begin
  Result := ASizedImage;
  with TRegExpr.Create do
    try
      Result := ReplaceRegExpr('\._(.*?)_\.', ASizedImage, '.', False);
    finally
      Free;
    end;
end;

function TAmazonCom.AmazonSearchRequest;
var
  HTTPRequest: IHTTPRequest;
begin
  HTTPRequest := THTTPRequest.Create(AWebsite + 's/ref=nb_sb_noss?url=' + HTTPEncode(ASearchAlias) + '&field-keywords=' + HTTPEncode(ATitle) + '&x=0&y=0');
  HTTPRequest.Referer := AWebsite;

  AFollowUpRequest := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(AFollowUpRequest);

  Result := HTTPManager.GetResult(AFollowUpRequest).HTTPResult.SourceCode;
end;

function TAmazonCom.AmazonExtractGameCategory(AHTML: string; AInsideHTML: Boolean = False): TTemplateTypeID;
var
  Category: string;
begin
  Result := cMovie;

  if not AInsideHTML then
    Category := AHTML
  else
    with TRegExpr.Create do
      try
        InputString := AHTML;
        Expression := '<span class="platform">(.*?)<\/span>';

        if Exec(InputString) then
          Category := Match[1];
      finally
        Free;
      end;

  if (Category = 'GameCube') then
    Result := cGameCube
  else if (Category = 'Nintendo DS') then
    Result := cNintendoDS
  else if (Pos('Windows', Category) > 0) or (Pos('PC', Category) > 0) then
    Result := cPCGames
  else if (Category = 'PlayStation 2') then
    Result := cPlayStation2
  else if (Pos('PlayStation 3', Category) > 0) or (Pos('PS3 Download', Category) > 0) then
    Result := cPlayStation3
  else if (Category = 'Sony PSP') then
    Result := cPlayStationPortable
  else if (Pos('Nintendo Wii', Category) > 0) then
    Result := cWii
  else if (Category = 'Xbox') then
    Result := cXbox
  else if (Category = 'Xbox 360') then
    Result := cXbox360;
end;

function TAmazonCom.AmazonDetailedPageRequest;
var
  RequestID: Double;
begin
  RequestID := HTTPManager.Get(AWebsite, AFollowUpRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  Result := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
end;

function TAmazonCom.GetName;
begin
  Result := 'Amazon.com';
end;

function TAmazonCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TAmazonCom.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cDescription];

  if _TemplateTypeID = cMovie then
    _ComponentIDs := _ComponentIDs + [cRuntime, cVideoSystem];

  Result := LongWord(_ComponentIDs);
end;

function TAmazonCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TAmazonCom.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TAmazonCom.Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController);
const
  website = 'http://www.amazon.com/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;

  _Title, _search_alias: string;
  _Count: Integer;

  ResponseStrSearchResult: string;
  RequestID: Double;

  procedure deep_search(AWebsitecode: string);
  var
    _tracklist: string;
  begin
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := '<li> <b>Run Time:<\/b> (\d+) minutes<\/li>';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
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
          InputString := AWebsitecode;
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
          InputString := AWebsitecode;
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
          InputString := AWebsitecode;
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

  procedure other_search(AWebsitecode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := AWebsitecode;

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

  procedure game_table_search(AWebsitecode: string; AFilter: Boolean = False);
  begin
    with TRegExpr.Create do
      try
        InputString := AWebsitecode;
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
      _search_alias := 'search-alias=movies-tv';
    cSoftware:
      _search_alias := 'search-alias=software';
    cOther:
      _search_alias := 'search-alias=misc';
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
            if Pos('class="formats" colspan=2>Platforms', string(Match[1])) > 0 then
              game_table_search(Match[1], True)

            else if _TemplateTypeID = AmazonExtractGameCategory(Match[1], True) then
              game_table_search(Match[1]);
          end
          else
            other_search(Match[1]);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;

      (*
        if _TemplateTypeID in [cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360] then
        Expression := '<a class="title" href="(.*?)">.*?<\/a>.*?<span class="platform">(.*?)<\/span>'
        else
        Expression := '<a class="title" href="(.*?)">.*?<\/a>';
        // Expression := 'class="productTitle"><a href="(.*?)".*?<span class="format">(.*?)<\/span>';

        if Exec(InputString) then
        begin
        repeat
        if _TemplateTypeID in [cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360] then
        begin
        if ((_TemplateTypeID = cGameCube) and (Match[2] = 'GameCube')) or { }
        ((_TemplateTypeID = cNintendoDS) and (Match[2] = 'Nintendo DS')) or { }
        ((_TemplateTypeID = cPCGames) and (Pos('Windows', string(Match[2])) > 0)) or { }
        ((_TemplateTypeID = cPlayStation2) and (Match[2] = 'PlayStation2')) or { }
        ((_TemplateTypeID = cPlayStation3) and (Match[2] = 'PLAYSTATION 2')) or { }
        ((_TemplateTypeID = cPlayStationPortable) and (Match[2] = 'Sony PSP')) or { }
        ((_TemplateTypeID = cWii) and (Match[2] = 'Nintendo Wii')) or { }
        ((_TemplateTypeID = cXbox) and (Match[2] = 'Xbox')) or { }
        ((_TemplateTypeID = cXbox360) and (Match[2] = 'Xbox 360')) then
        deep_search(AmazonDetailedPageRequest(RequestID, Match[1]))
        end
        else
        deep_search(AmazonDetailedPageRequest(RequestID, Match[1]));
        Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
        *)
    finally
      Free;
    end;
  end;
end;

end.
