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
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCinefactsDe = class(TCrawlerPlugIn)
  private
    FCount: Integer;
  protected { . }
  const
    WEBSITE = 'http://www.cduniverse.com/';

    function ThumbToLargeImage(AImageURL: string): string;
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

function TCinefactsDe.ThumbToLargeImage;
begin
  Result := StringReplace(AImageURL, '/thumbs/', '/', [rfIgnoreCase])
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

function TCinefactsDe.InternalExecute;

  procedure MainMoviePage(AWebsitecode: string);
  var
    LGenreList: TStrings;
    LFilmkritik, LGenre: string;
  begin
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Inhalt:<\/strong>(.*?)<\/';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));

          InputString := ExtractTextBetween(AWebsitecode, '<article>', '</article>');
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

    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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

    if (AControlController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Laufzeit: (\d+) ';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
  end;

  procedure MainImagesPage(AWebsitecode: string);
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
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
  end;

  procedure MainDVDBlurayPage(AWebsitecode: string);
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
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
  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  RequestID := HTTPManager.Get(THTTPRequest.Create(curl + '/search/site/q/' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(ResponseStrSearchResult, '<h3>Filme', '<h3>Stars');
      Expression := 'class="s_link" href="(.*?)">';

      if Exec(InputString) then
      begin
        repeat

          MainMoviePage(SimpleGETRequest(curl + Match[1], RequestID));

          if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
          begin
            MainImagesPage(SimpleGETRequest(curl + Match[1] + '/Bildergalerie', RequestID));

            MainDVDBlurayPage(SimpleGETRequest(curl + Match[1] + '/DVD-Blu-ray', RequestID));
          end;

          Inc(FCount);

        until not(ExecNext and ((FCount < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

function TCinefactsDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
