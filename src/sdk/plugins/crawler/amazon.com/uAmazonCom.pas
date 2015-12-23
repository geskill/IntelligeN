unit uAmazonCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TAmazonCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.amazon.com/';

    function GetBaseSearchType(const ATypeID: TTypeID): string;
    function IsSystem(const ATypeID: TTypeID; const ASystem: string): Boolean;

    function AmazonHTMLDescription2Text(AHtmlContent: string): string;
    function AmazonOriginalSize(ASizedImage: string): string;
    function AmazonSearchRequest(AWebsite, ASearchAlias, ATitle: string; out AFollowUpRequest: Double): string;
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

{ TAmazonCom }

function TAmazonCom.GetBaseSearchType(const ATypeID: TTypeID): string;
begin
  if ATypeID in cGames then
    Result := 'videogames'
  else
  begin
    case ATypeID of
      cAudio:
        Result := 'popular'; // digital-music
      cEBook:
        Result := 'stripbooks';
      cMovie:
        Result := 'movies-tv';
      cSoftware:
        Result := 'software';
      cOther:
        Result := 'aps';
    end;
  end;
end;

function TAmazonCom.IsSystem(const ATypeID: TTypeID; const ASystem: string): Boolean;
begin
  Result := False;

  case ATypeID of
    cNintendoDS:
      begin
        Result := (ASystem = 'Nintendo DS') or (ASystem = 'Nintendo 3DS');
      end;
    cPCGames:
      begin
        Result := (Pos('Windows', ASystem) > 0) or (Pos('PC', ASystem) > 0);
      end;
    cPlayStation3:
      begin
        Result := (ASystem = 'PlayStation 3');
      end;
    cPlayStation4:
      begin
        Result := (ASystem = 'PlayStation 4');
      end;
    cPlayStationVita:
      begin
        Result := (ASystem = 'PlayStation Vita');
      end;
    cWii:
      begin
        Result := (ASystem = 'Nintendo Wii');
      end;
    cWiiU:
      begin
        Result := (ASystem = 'Nintendo Wii U');
      end;
    cXbox360:
      begin
        Result := (ASystem = 'Xbox 360');
      end;
    cXboxOne:
      begin
        Result := (ASystem = 'Xbox One');
      end;
  end;
end;

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
  LSearchString: string;
begin
  LSearchString := AWebsite + 's/ref=nb_sb_noss?url=search-alias=' + HTTPEncode(ASearchAlias) + '&field-keywords=' + HTTPEncode(ATitle) + '&x=0&y=0';

  Result := GETRequest(LSearchString, AFollowUpRequest);
end;

function TAmazonCom.GetName;
begin
  Result := 'Amazon.com';
end;

function TAmazonCom.InternalGetAvailableTypeIDs;
begin
  Result := [ low(TTypeID) .. high(TTypeID)] - [cXXX];
end;

function TAmazonCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cDescription];

  if (ATypeID = cMovie) then
    Result := Result + [cRuntime, cVideoSystem];
end;

function TAmazonCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TAmazonCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TAmazonCom.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    LTrackList, s: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'data-old-hires="(.*?)"';

          if Exec(InputString) then
          begin
            if not(Pos('no-img', string(Match[1])) > 0) then
              AControlController.FindControl(cPicture).AddProposedValue(GetName, AmazonOriginalSize(Match[1]));
          end;
        finally
          Free;
        end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<li> <b>Run Time:<\/b> (\d+) minutes<\/li>';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cVideoSystem) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
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

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'iframeContent = "(.*?)"';

          if Exec(InputString) then
          begin
            s := HTTPDecode(Match[1]);

            with TRegExpr.Create do
            begin
              try
                InputString := AWebsiteSourceCode;
                Expression := '<tr class="\w+">\s+<td>\s+(.*?)\s<\/td>';

                if Exec(InputString) then
                begin
                  LTrackList := '';
                  repeat
                    LTrackList := LTrackList + Trim(Match[1]) + sLineBreak;
                  until not ExecNext;
                  AControlController.FindControl(cDescription).AddProposedValue(GetName, copy(LTrackList, 1, length(LTrackList) - 2));
                end;

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

          end;
        finally
          Free;
        end;
    end;

  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr, s: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LResponeStr := AmazonSearchRequest(WEBSITE, GetBaseSearchType(ATypeID), LTitle, LRequestID1);

  if not(Pos('result-count', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<li id="result_\d+"(.*?)<\/li>';

        if Exec(InputString) then
        begin
          repeat
            s := Match[0];

            with TRegExpr.Create do
            begin
              try
                InputString := s;

                if Pos('twister', s) > 0 then
                  Expression := 'a-text-normal" title="(.*?)" href="(.*?)"'
                else
                  Expression := 'a-link-normal\s+a-text-normal" title="(.*?)" href="(.*?)"';

                if Exec(InputString) then
                begin
                  repeat

                    if (not(ATypeID in cGames)) or ((ATypeID in cGames) and IsSystem(ATypeID, Match[2])) then
                    begin
                      LResponeStr := GETFollowUpRequest(Match[1], LRequestID1, LRequestID2);

                      deep_search(LResponeStr);
                    end;

                  until not ExecNext;
                end;
              finally
                Free;
              end;
            end;

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TAmazonCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
