unit uThaliaDe;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TThaliaDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.thalia.de/';

    function GetBaseSearchType(const ATypeID: TTypeID): string;
    function GetGameSearchType(const ATypeID: TTypeID): string;
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

{ TThaliaDe }

function TThaliaDe.GetBaseSearchType(const ATypeID: TTypeID): string;
begin
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

  if ATypeID in cGames then
    Result := 'SPIEL'
  else
  begin
    case ATypeID of
      cAudio:
        Result := 'MUSIK';
      cEBook:
        Result := 'EBOOK';
      cMovie:
        Result := 'FILM';
      cSoftware:
        Result := 'SOFTWARE';
      cOther:
        Result := 'ANY';
    end;
  end;
end;

function TThaliaDe.GetGameSearchType(const ATypeID: TTypeID): string;
begin
  case ATypeID of
    cNintendoDS:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5350&sswg=ANY&asnlinkname=Kategorien_Nintendo+3DS+%26+2DS&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '5350';
    cPCGames:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5442&sswg=ANY&asnlinkname=Kategorien_PC-Games&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '5442';
    cPlayStation3:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5399&sswg=ANY&asnlinkname=Kategorien_Playstation+3&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '5399';
    cPlayStation4:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=14958&sswg=ANY&asnlinkname=Kategorien_Playstation+4&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '14958';
    cPlayStationVita:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5427&sswg=ANY&asnlinkname=Kategorien_Playstation+Vita&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '5427';
    cWii:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5369&sswg=ANY&asnlinkname=Kategorien_Nintendo+Wii&fltKATEGORIEN_1=5067&asn=true&sq=Mario&timestamp=1446157777931
      Result := '5369';
    cWiiU:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=11096&sswg=ANY&asnlinkname=Kategorien_Nintendo+Wii+U&fltKATEGORIEN_1=5067&asn=true&sq=Mario&timestamp=1446157777931
      Result := '11096';
    cXbox360:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5228&sswg=ANY&asnlinkname=Kategorien_Xbox+360&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '5228';
    cXboxOne:
      // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=14975&sswg=ANY&asnlinkname=Kategorien_Xbox+One&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy&timestamp=1446157389683
      Result := '14975';
  end;
end;

function TThaliaDe.GetName;
begin
  Result := 'Thalia.de';
end;

function TThaliaDe.InternalGetAvailableTypeIDs;
begin
  Result := [ low(TTypeID) .. high(TTypeID)] - [cXXX];
end;

function TThaliaDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cLanguage, cDescription];

  if not(ATypeID = cOther) then
    Result := Result + [cGenre];

  if (ATypeID = cMovie) then
    Result := Result + [cRuntime];
end;

function TThaliaDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TThaliaDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TThaliaDe.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'data-image="(.*?)"';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
          end;
        finally
          Free;
        end;

    if ACanUse(cLanguage) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<dt>\s+Sprache.*?d>(.*?)<\/';

          if Exec(InputString) then
          begin
            s := Match[1];

            // Remove (Untertitel
            if (Pos('(', s) > 0) then
              s := copy(s, 1, Pos('(', s));

            if (Pos('/', s) > 0) or (Pos(',', s) > 0) or (Pos('|', s) > 0) then
            begin
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
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
            begin
              AControlController.FindControl(cLanguage).AddProposedValue(GetName, s);
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<dt>\s+Genre.*?d>(.*?)<\/';

          if Exec(InputString) then
          begin
            s := Match[1];

            if (Pos('/', s) > 0) or (Pos(',', s) > 0) or (Pos('|', s) > 0) then
            begin
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
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
            begin
              AControlController.FindControl(cGenre).AddProposedValue(GetName, s);
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<dt>\s+Spieldauer.*?d>(.*?) Minuten';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Trim(Match[1]));
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Beschreibung.*?">(.*?)<\/dd>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;

  end;

var
  LTitle: string;
  LCount: Integer;

  LSearchString: string;
  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LSearchString := 'shop/home/suche/' + GetBaseSearchType(ATypeID) + '/?sswg=' + GetBaseSearchType(ATypeID) + '&sq=' + HTTPEncode(LTitle);
  if ATypeID in cGames then
    LSearchString := LSearchString + '&fltKATEGORIEN_2=' + GetGameSearchType(ATypeID);

  // http://www.thalia.de/shop/home/suche/ANY/?fltKATEGORIEN_2=5350&sswg=ANY&asnlinkname=Kategorien_Nintendo+3DS+%26+2DS&fltKATEGORIEN_1=5067&asn=true&sq=Final+Fantasy

  LResponeStr := GETRequest(WEBSITE + LSearchString, LRequestID1);

  if not(Pos('Treffer', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(LResponeStr, 'ProductList', 'ProductList');
        Expression := '<\/div>\s+<\/div>\s+<a href="(.*?)" data-test=';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else if not(Pos('pvTopDetails', LResponeStr) = 0) then
  begin
    deep_search(LResponeStr);
  end;

  Result := True;
end;

function TThaliaDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
