unit uCinefactsDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TCinefactsDe = class(TCrawlerPlugIn)
  private
    FCount: Integer;
  protected
    function SimpleGETRequest(AURL: string; AIdHTTPHelper: TIdHTTPHelper): string;
    function ThumbToLargeImage(AImageURL: string): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TCinefactsDe.SimpleGETRequest;
var
  ReplyData: TStringStream;
begin
  ReplyData := TStringStream.Create('', CP_UTF8);
  try
    AIdHTTPHelper.Get(AURL, ReplyData);

    Result := ReplyData.DataString;
  finally
    ReplyData.Free;
  end;
end;

function TCinefactsDe.ThumbToLargeImage;
begin
  Result := StringReplace(AImageURL, '/thumbs/', '/', [rfIgnoreCase])
end;

function TCinefactsDe.GetName;
begin
  Result := 'Cinefacts.de';
end;

function TCinefactsDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TCinefactsDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cRuntime, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TCinefactsDe.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TCinefactsDe.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TCinefactsDe.Exec;
const
  curl = 'http://www.cinefacts.de';
var
  _ComponentIDs: TComponentIDs;
  _Title: WideString;

  procedure MainMoviePage(AWebsitecode: string);
  var
    LGenreList: TStrings;
    LFilmkritik, LGenre: string;
  begin
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Inhalt:<\/strong>(.*?)<\/';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);

          InputString := ExtractTextBetween(AWebsitecode, '<article>', '</article>');
          Expression := '(class="item_text">|class="thisReview">)(.*?)(<span class="review|<\/span>)';

          if Exec(InputString) then
          begin
            LFilmkritik := '';

            repeat
              LFilmkritik := LFilmkritik + sLineBreak + sLineBreak + Trim(ReduceWhitespace(HTML2Text(Match[2])));
            until not ExecNext;

            if not SameStr('', LFilmkritik) then
              AComponentController.FindControl(cDescription).AddValue(Trim(LFilmkritik), GetName);
          end;
        finally
          Free;
        end;

    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Genre: (.*?) \|';

          if Exec(InputString) then
          begin
            LGenreList := SplittString(',', Trim(Match[1]));
            try
              for LGenre in LGenreList do
                AComponentController.FindControl(cGenre).AddValue(Trim(LGenre), GetName);
            finally
              LGenreList.Free;
            end;
          end;
        finally
          Free;
        end;

    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := AWebsitecode;
          Expression := 'Laufzeit: (\d+) ';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
        finally
          Free;
        end;
  end;

  procedure MainImagesPage(AWebsitecode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ExtractTextBetween(AWebsitecode, '<h3>Poster', '<h3>Szenenbilder');
          Expression := 'src="(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cPicture).AddValue(ThumbToLargeImage(Match[1]), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;

  procedure MainDVDBlurayPage(AWebsitecode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ExtractTextBetween(AWebsitecode, '<article>', '</section>');
          Expression := 'src="(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cPicture).AddValue(ThumbToLargeImage(Match[1]), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;

var
  IdHTTPHelper: TIdHTTPHelper;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  FCount := 0;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    IdHTTPHelper.Request.Referer := curl;

    ResponseStrSearchResult := SimpleGETRequest(curl + '/search/site/q/' + HTTPEncode(_Title), IdHTTPHelper);

    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(ResponseStrSearchResult, '<h3>Filme', '<h3>Stars');
        Expression := 'class="s_link" href="(.*?)">';

        if Exec(InputString) then
        begin
          repeat

            MainMoviePage(SimpleGETRequest(curl + Match[1], IdHTTPHelper));

            if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
            begin
              MainImagesPage(SimpleGETRequest(curl + Match[1] + '/Bildergalerie', IdHTTPHelper));

              MainDVDBlurayPage(SimpleGETRequest(curl + Match[1] + '/DVD-Blu-ray', IdHTTPHelper));
            end;

            Inc(FCount);

          until not(ExecNext and ((FCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;

  finally
    IdHTTPHelper.Free;
  end;
end;

end.
