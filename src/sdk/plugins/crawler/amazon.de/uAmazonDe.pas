unit uAmazonDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // JSON
  uLkJSON,
  // Common
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uAmazonCom,
  // Utils
  uHTMLUtils, uStringUtils, uURLUtils;

type
  TAmazonDe = class(TAmazonCom)
  protected { . }
  const
    WEBSITE = 'http://www.amazon.de/';

    function GetBaseSearchType(const ATypeID: TTypeID): string; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;
  end;

implementation

{ TAmazonDe }

function TAmazonDe.GetBaseSearchType(const ATypeID: TTypeID): string;
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
        Result := 'dvd';
      cSoftware:
        Result := 'software';
      cOther:
        Result := 'aps';
    end;
  end;
end;

function TAmazonCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TAmazonCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TAmazonCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TAmazonDe.GetName: WideString;
begin
  Result := 'Amazon.de';
end;

function TAmazonDe.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    I: Integer;
    LTrackList, s, img: string;
    LImageList: TStringList;
    LJSONobject: TlkJSONobject;
  begin
    if ACanUse(cPicture) then
    begin
      LImageList := TStringList.Create;
      try
        with TRegExpr.Create do
          try
            InputString := AWebsiteSourceCode;

            Expression := 'data-old-hires="(.*?)"';
            if Exec(InputString) then
            begin
              if AmazonCanAddImage(Match[1]) and (LImageList.IndexOf(AmazonOriginalImageSize(Match[1])) = -1) then
                LImageList.Add(AmazonOriginalImageSize(Match[1]));
            end;

            Expression := 'data-a-dynamic-image="(.*?)"';
            if Exec(InputString) then
            begin
              if not SameStr('', Match[1]) then
              begin
                try
                  LJSONobject := TlkJSON.ParseText(HTMLDecode(Match[1])) as TlkJSONobject;
                  for I := LJSONobject.Count - 1 downto 0 do
                  begin
                    img := LJSONobject.NameOf[I];
                    if AmazonCanAddImage(img) and (LImageList.IndexOf(AmazonOriginalImageSize(img)) = -1) then
                      LImageList.Add(AmazonOriginalImageSize(img));
                  end;
                finally
                  LJSONobject.Free;
                end;
              end;
            end;
          finally
            Free;
          end;

        for I := 0 to LImageList.Count - 1 do
          AControlController.FindControl(cPicture).AddProposedValue(GetName, LImageList.Strings[I]);
      finally
        LImageList.Free;
      end;
    end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<li> <b>Spieldauer:<\/b> (\d+) Minuten<\/li>';

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
            s := URLDecode(Match[1]);

            with TRegExpr.Create do
            begin
              try
                InputString := s;
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
                    AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(ReduceWhitespace(HTML2TextAndDecode(Match[1]))));
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

      with TRegExpr.Create do
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
        finally
          Free;
        end;

      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'productDescription" class="a-section a-spacing-small.*?">(.*?)(<a class="a-link-normal"|<\/div>)';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(ReduceWhitespace(HTML2TextAndDecode(Match[1]))));
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

                    if (not(ATypeID in cGames)) or ((ATypeID in cGames) and IsSystem(ATypeID, Match[1])) then
                    begin
                      LResponeStr := GETFollowUpRequest(HTMLDecode(Match[2]), LRequestID1, LRequestID2);

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

end.
