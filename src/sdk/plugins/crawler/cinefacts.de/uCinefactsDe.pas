unit uCinefactsDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
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
  TCinefactsDe = class(TCrawlerPlugIn)
  private
    FCount: Integer;
  protected { . }
  const
    WEBSITE = 'http://www.cinefacts.de/';

    function ThumbToLargeImage(AImageURL: string): string;
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

function TCinefactsDe.ThumbToLargeImage;
begin
  Result := StringReplace(AImageURL, '/thumbs/', '/', [rfIgnoreCase])
end;

function TCinefactsDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TCinefactsDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TCinefactsDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TCinefactsDe.GetName;
begin
  Result := 'Cinefacts.de';
end;

function TCinefactsDe.InternalGetAvailableTypeIDs;
begin
  Result := [cMovie];
end;

function TCinefactsDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cRuntime, cDescription];
end;

function TCinefactsDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TCinefactsDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TCinefactsDe.InternalGetRetrieveData;

  procedure MainMoviePage(AWebsitecode: string);
  var
    LGenreList: TStrings;
    LFilmkritik, LGenre: string;
  begin
    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Inhalt: <\/strong>(.*?)<\/';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));

          InputString := ExtractTextBetween(AWebsitecode, '<h3>Filmkritik', '</article>');
          Expression := '(class="item_text">|class="thisReview">)(.*?)(<span class="review|<\/span>)';

          if Exec(InputString) then
          begin
            LFilmkritik := '';

            repeat
              LFilmkritik := LFilmkritik + sLineBreak + sLineBreak + Trim(ReduceWhitespace(HTML2Text(Match[2])));
            until not ExecNext;

            if not SameStr('', LFilmkritik) then
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(LFilmkritik));
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Genre: (.*?) \|';

          if Exec(InputString) then
          begin
            LGenreList := SplittString(',', Trim(Match[1]));
            try
              for LGenre in LGenreList do
                AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(LGenre));
            finally
              LGenreList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'itemprop="duration" datetime="PT(\d+)M';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
  end;

  procedure MainImagesPage(AWebsitecode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(AWebsitecode, '<h3>Poster', '<h3>Szenenbilder');
        Expression := 'src="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cPicture).AddProposedValue(GetName, ThumbToLargeImage(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

  procedure MainDVDBlurayPage(AWebsitecode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(AWebsitecode, '<article>', '</section>');
        Expression := 'src="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cPicture).AddProposedValue(GetName, ThumbToLargeImage(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2, LRequestID3, LRequestID4: Double;

  LResponeStr, LFilteredContent: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LResponeStr := GETRequest(WEBSITE + 'search/site/q/' + HTTPEncode(LTitle), LRequestID1);

  LFilteredContent := ExtractTextBetween(LResponeStr, '<h3>Filme', '<h3>Stars');

  if not(Pos('class="s_link"', LFilteredContent) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LFilteredContent;
        Expression := 'class="s_link" href="\/(.*?)">';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

            MainMoviePage(LResponeStr);

            if ACanUse(cPicture) then
            begin
              LResponeStr := GETFollowUpRequest(WEBSITE + Match[1] + '/Bildergalerie', LRequestID2, LRequestID3);

              MainImagesPage(LResponeStr);

              LResponeStr := GETFollowUpRequest(WEBSITE + Match[1] + '/DVD-Blu-ray', LRequestID3, LRequestID4);

              MainDVDBlurayPage(LResponeStr);
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

function TCinefactsDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
