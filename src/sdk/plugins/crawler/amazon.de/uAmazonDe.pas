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
  protected { . }
  const
    WEBSITE = 'http://www.amazon.de/';
  public
    function GetName: WideString; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;
  end;

implementation

{ TAmazonDe }

function TAmazonDe.GetName: WideString;
begin
  Result := 'Amazon.de';
end;

function TAmazonDe.InternalExecute;

  procedure deep_search(AWebsitecode: string);
  var
    LTrackList: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
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
          InputString := AWebsitecode;
          Expression := '<li> <b>Spieldauer:<\/b> (\d+) Minuten<\/li>';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cVideoSystem) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
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
          InputString := AWebsitecode;
          Expression := '<tr class="\w+">\s+<td>\s+(.*?)\s<\/td>';

          if Exec(InputString) then
          begin
            LTrackList := '';
            repeat
              LTrackList := LTrackList + Trim(Match[1]) + sLineBreak;
            until not ExecNext;
            AControlController.FindControl(cDescription).AddProposedValue(GetName, copy(LTrackList, 1, length(LTrackList) - 2));
          end;
        finally
          Free;
        end;

      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'productDescription" class="a-section a-spacing-small">(.*?)(<a class="a-link-normal"|<\/div>)';

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
end;

end.
