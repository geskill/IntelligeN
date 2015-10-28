unit uXrelTo;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
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
  TXrelTo = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.xrel.to/';
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

function TXrelTo.GetName;
begin
  Result := 'Xrel.to';
end;

function TXrelTo.InternalGetAvailableTypeIDs;
begin
  Result := [ low(TTypeID) .. high(TTypeID)] - [cAudio, cEBook];
end;

function TXrelTo.InternalGetAvailableControlIDs;
begin
  Result := [cReleaseDate, cTitle, cNFO];

  if not(ATypeID = cXXX) then
    Result := Result + [cPicture, cDescription];

  if not(ATypeID in [cSoftware, cXXX]) then
    Result := Result + [cGenre];

  if (ATypeID in cGames) then
    Result := Result + [cCreator, cPublisher];

  if (ATypeID = cMovie) then
    Result := Result + [cDirector, cRuntime, cAudioStream, cVideoStream];
end;

function TXrelTo.InternalGetControlIDDefaultValue;
begin
  Result := True;

  if (cPicture = AControlID) or (cNFO = AControlID) then
    Result := False;
end;

function TXrelTo.InternalGetDependentControlIDs;
begin
  Result := [cReleaseName];
end;

function TXrelTo.InternalExecute;
const
  XREL_MONTH: array [0 .. 11] of string = ('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez');
var
  LReleasename: string;

  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ProductInformationPageLink, ResponseStrReleaseInformation, ResponseStrProductInformation, s: string;

  LStringDate: string;
  LStringList: TStringList;
begin
  LReleasename := AControlController.FindControl(cReleaseName).Value;

  HTTPRequest := THTTPRequest.Create(WEBSITE + 'search.html?xrel_search_query=' + LReleasename);
  HTTPRequest.Referer := WEBSITE + 'home.html';

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrReleaseInformation := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if Pos('nfo_title', ResponseStrReleaseInformation) > 0 then
  begin
    if ACanUse(cReleaseDate) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStrReleaseInformation;
          Expression := 'Zeit: <\/div> <div class="l_right">\s+(\d+)\. ([a-zA-Z]{3}) (\d{4}),';

          if Exec(InputString) then
          begin
            repeat
              LStringDate := PadLeft(Match[1], '0', 2) + '.' + PadLeft(IntToStr(IndexText(Match[2], XREL_MONTH) + 1), '0', 2) + '.' + Match[3];
              // TODO: Improve localized date
              AControlController.FindControl(cReleaseDate).AddProposedValue(GetName, LStringDate);
            until not ExecNext;
          end;
        finally
          Free;
        end;
      end;
    end;

    if ACanUse(cNFO) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStrReleaseInformation;
          Expression := '<pre>(.*?)<\/pre>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cNFO).AddProposedValue(GetName, Trim(HTML2Text(Match[1], False)));
            until not ExecNext;
          end;
        finally
          Free;
        end;
      end;
    end;

    with TRegExpr.Create do
    begin
      try
        InputString := ResponseStrReleaseInformation;
        Expression := '<a href=\"\/([^\"]+?)\">Produktinformationen<\/a>';

        if Exec(InputString) then
        begin
          repeat
            ProductInformationPageLink := Match[1];
          until not ExecNext;
        end;
      finally
        Free;
      end;
    end;

    if (ATypeID = cMovie) then
    begin
      if ACanUse(cAudioStream) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Audio-Stream: </div> <div class="l_right"> (.*?) </div>';
            if Exec(InputString) then
              AControlController.FindControl(cAudioStream).AddProposedValue(GetName, Match[1]);
          finally
            Free;
          end;

      if ACanUse(cVideoStream) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Video-Stream: </div> <div class="l_right"> (.*?) </div>';

            if Exec(InputString) then
              AControlController.FindControl(cVideoStream).AddProposedValue(GetName, Match[1]);
          finally
            Free;
          end;
    end;

    if (cTitle in AControlIDs) or (cDescription in AControlIDs) or (cGenre in AControlIDs) then
    begin
      RequestID2 := HTTPManager.Get(WEBSITE + ProductInformationPageLink, RequestID1, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID2);

      ResponseStrProductInformation := HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode;

      if ACanUse(cTitle) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrProductInformation;
            Expression := '<h3>(.*?)<\/h3>';

            if Exec(InputString) then
            begin
              repeat
                AControlController.FindControl(cTitle).AddProposedValue(GetName, HTML2Text(Match[1]));
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      if ACanUse(cPicture) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := '<div id="poster" style="line-height:0;"><div>  <img src="\/([^\"]+?)"';

            if Exec(InputString) then
            begin
              repeat
                if not(Match[1] = '') then
                begin
                  s := WEBSITE + Match[1];

                  AControlController.FindControl(cPicture).AddProposedValue(GetName, s);
                end;
              until not ExecNext;
            end;
          finally
            Free;
          end;

      if ACanUse(cRuntime) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Laufzeit:</div> <div class="l_right" title="(\d+) ';

            if Exec(InputString) then
              AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
          finally
            Free;
          end;

      if ACanUse(cDescription) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrProductInformation;
            Expression := '<div class="article_text" style="margin:0;"> (.*?)(<table| <\/div>)';

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

      if ACanUse(cGenre) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Genre:</div> <div class="l_right">(.*?)<\/div>';

            if Exec(InputString) then
            begin
              repeat
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
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      if (ATypeID = cMovie) and ACanUse(cDirector) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Regisseur: <\/div>(.*?)<div class="clear">';

            if Exec(InputString) then
            begin
              s := Match[1];

              LStringList := TStringList.Create;
              try
                with TRegExpr.Create do
                begin
                  try
                    InputString := s;
                    Expression := '<a href=".*?">(.*?)<\/a>';

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
      end;

      if (ATypeID in cGames) and ACanUse(cCreator) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Entwickler:</div> <div class="l_right">(.*?)<\/div>';

            if Exec(InputString) then
            begin
              AControlController.FindControl(cCreator).AddProposedValue(GetName, Match[1]);
            end;
          finally
            Free;
          end;
      end;

      if (ATypeID in cGames) and ACanUse(cPublisher) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Herausgeber:</div> <div class="l_right">(.*?)<\/div>';

            if Exec(InputString) then
            begin
              AControlController.FindControl(cPublisher).AddProposedValue(GetName, Match[1]);
            end;
          finally
            Free;
          end;
      end;

    end;
  end;
end;

function TXrelTo.GetResultsLimitDefaultValue;
begin
  Result := 0;
end;

end.
