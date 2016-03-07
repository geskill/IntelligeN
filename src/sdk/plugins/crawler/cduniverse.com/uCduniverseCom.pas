unit uCduniverseCom;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCduniverseCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.cduniverse.com/';
    function GetSearchType(const ATypeID: TTypeID): string;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

{ TCduniverseCom }

function TCduniverseCom.GetSearchType(const ATypeID: TTypeID): string;
begin
  case ATypeID of
    cAudio:
      Result := 'music';
    cEBook:
      Result := 'books';
    cMovie:
      Result := 'movie';
    cNintendoDS:
      Result := 'games&setpref=NDS';
    cNintendo3DS:
      Result := 'games&setpref=3DS';
    cPCGames:
      Result := 'games&setpref=PCG';
    cPlayStation3:
      Result := 'games&setpref=PS3';
    cPlayStation4:
      Result := 'games&setpref=PS4';
    cPlayStationVita:
      Result := 'games&setpref=PSV';
    cSoftware:
      Result := 'games';
    cWii:
      Result := 'games&setpref=WII';
    cWiiU:
      Result := 'games&setpref=WIIU';
    cXbox360:
      Result := 'XB360';
    cXboxOne:
      Result := 'XB1';
    cXXX:
      Result := 'ice';
    cOther:
      Result := 'all';
  end;
end;

function TCduniverseCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TCduniverseCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TCduniverseCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TCduniverseCom.GetName;
begin
  Result := 'cduniverse.com';
end;

function TCduniverseCom.InternalGetAvailableTypeIDs;
begin
  Result := [low(TTypeID) .. high(TTypeID)];
end;

function TCduniverseCom.InternalGetAvailableControlIDs;
begin
  Result := [cTags, cCreator, cPublisher, cPicture, cGenre, cDescription];

  if (ATypeID in [cMovie, cXXX]) then
    Result := Result + [cDirector];

  if (ATypeID in [cAudio, cMovie]) then
    Result := Result + [cRuntime];
end;

function TCduniverseCom.InternalGetControlIDDefaultValue;
begin
  Result := True;

  if (AControlID in [cTags]) then
    Result := False;
end;

function TCduniverseCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TCduniverseCom.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
    LTracklist: string;
  begin
    if ACanUse(cTags) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Starring<\/td><td>(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '">(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(HTML2TextAndDecode(Match[1]));
                    until not ExecNext;

                    AControlController.FindControl(cTags).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'UpdateLinkForJS\(this, ''(.*?)''';

          if Exec(InputString) then
          begin
            if Exec(InputString) then
              AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
          end
          else
          begin
            Expression := 'itemprop="image" src="(.*?)"';

            if Exec(InputString) then
              AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
          end;
        finally
          Free;
        end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<nobr>(Recording|Running) Time <\/nobr><\/td><td>(\d+)';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[2], GetName)
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := ExtractTextBetween(AWebsiteSourceCode, 'Category</td>', '</table>');
          Expression := '"standardhyperlink".*?>(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Developer<\/td><td>(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<a.*?>(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(HTML2TextAndDecode(Match[1]));
                    until not ExecNext;

                    AControlController.FindControl(cCreator).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end
                  else
                  begin
                    AControlController.FindControl(cCreator).AddProposedValue(GetName, Trim(HTML2TextAndDecode(s)));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cDirector) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Director<\/td><td>(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<a.*?>(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(HTML2TextAndDecode(Match[1]));
                    until not ExecNext;

                    AControlController.FindControl(cDirector).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end
                  else
                  begin
                    AControlController.FindControl(cDirector).AddProposedValue(GetName, Trim(HTML2TextAndDecode(s)));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cPublisher) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '(Label|Studio|Publisher)<\/td><td>(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[2];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<a.*?>(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(HTML2TextAndDecode(Match[1]));
                    until not ExecNext;

                    AControlController.FindControl(cPublisher).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end
                  else
                  begin
                    AControlController.FindControl(cPublisher).AddProposedValue(GetName, Trim(HTML2TextAndDecode(s)));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'class="mp3gridfont".*?>(\d+)<\/td>.*?class="mp3gridfont".*?>(.*?)&';

          if Exec(InputString) then
          begin
            LTracklist := '';
            repeat
              LTracklist := LTracklist + Match[2] + '. ' + Match[3] + sLineBreak;
            until not ExecNext;
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(LTracklist));
          end;
        finally
          Free;
        end;
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '"description">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
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

  LHTTPRequest: IHTTPRequest;
  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'sresult.asp?style=' + GetSearchType(ATypeID) + '&HT_Search_Info=' + HTTPEncode(LTitle));
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
    Cookies.Add('IAmAnAdult=yes');
  end;

  LRequestID1 := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID1);

  LResponeStr := HTTPManager.GetResult(LRequestID1).HTTPResult.SourceCode;

  if not(Pos('id="efef1"', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '(2|4)px;"><a\s+href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + HTMLDecode(Match[2]), LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TCduniverseCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
