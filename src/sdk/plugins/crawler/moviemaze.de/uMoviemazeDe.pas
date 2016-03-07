unit uMoviemazeDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TMoviemazeDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.moviemaze.de/';

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

{ TMoviemazeDe }

function TMoviemazeDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TMoviemazeDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TMoviemazeDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TMoviemazeDe.GetName;
begin
  Result := 'Moviemaze.de';
end;

function TMoviemazeDe.InternalGetAvailableTypeIDs;
begin
  Result := [cMovie];
end;

function TMoviemazeDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cDirector, cGenre, cRuntime, cDescription];
end;

function TMoviemazeDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TMoviemazeDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TMoviemazeDe.InternalGetRetrieveData;

  procedure deep_search_picture(AWebsiteSourceCode: string);
  var
    LMovieID, LPictureNumber: string;
  begin
    with TRegExpr.Create do
      try
        InputString := AWebsiteSourceCode;
        Expression := '<a href="\/media\/poster\/(\d+)\/(\d+)\/';

        if Exec(InputString) then
        begin
          repeat
            LMovieID := PadLeft(Match[1], '0', 4);
            LPictureNumber := PadLeft(Match[2], '0', 2);

            AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + 'filme/' + LMovieID + '/poster_lg' + LPictureNumber + '.jpg');
          until not ExecNext;
        end
        else
        begin
          Expression := 'img class="sidebar-img" itemprop="image" src="(.*?)"';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], '_thumb', '', []));
          end;
        end;
      finally
        Free;
      end;
  end;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cDirector) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'itemprop="director".*?>(.*?)<\/div>\s+<\/div>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := 'name">(.*?)<\/';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(Match[1]);
                    until not ExecNext;

                    AControlController.FindControl(cDirector).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
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

    if ACanUse(cGenre) then
      with TRegExpr.Create do
      begin
        try
          InputString := AWebsiteSourceCode;
          Expression := '<dd itemprop="genre">(.*?)<\/dd>';

          if Exec(InputString) then
          begin
            s := Match[1];

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
          end;
        finally
          Free;
        end;
      end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
      begin
        try
          InputString := AWebsiteSourceCode;
          Expression := '<dt>Länge:<\/dt>\s+<dd>(\d+)';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
      end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          // plot
          InputString := AWebsiteSourceCode;
          Expression := '"description">(.*?)<\/';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, HTMLDecode(Match[1]));

          // critics
          Expression := 'itemprop="articleBody">(.*?)<\/article>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2TextAndDecode(Match[1])));
        finally
          Free;
        end;
    end;

  end;

var
  LTitle: string;
  LCount: Integer;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LRequestID1, LRequestID2, LRequestID3: Double;

  LResponeStr, s: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'suche/result.phtml');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('searchword', LTitle);
  end;

  LRequestID1 := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID1);

  LResponeStr := HTTPManager.GetResult(LRequestID1).HTTPResult.SourceCode;

  if not(Pos('results-movie', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(LResponeStr, 'results-movie', '</ul>');
        Expression := 'class="title" href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat

            if Pos('filme', string(Match[1])) > 0 then
            begin
              LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

              deep_search(LResponeStr);

              if ACanUse(cPicture) then
              begin
                s := Match[1];
                with TRegExpr.Create do
                begin
                  try
                    InputString := s;
                    Expression := '\/(\d+)\/(.*?)$';

                    if Exec(InputString) then
                    begin
                      LResponeStr := GETFollowUpRequest(WEBSITE + 'media/poster/' + Match[1] + '/' + Match[2], LRequestID2, LRequestID3);

                      deep_search_picture(LResponeStr);
                    end;
                  finally
                    Free;
                  end;
                end;
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

function TMoviemazeDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
