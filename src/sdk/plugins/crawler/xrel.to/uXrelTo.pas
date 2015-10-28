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
  uHTMLUtils;

type
  TXrelTo = class(TCrawlerPlugIn)
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

function FormatInt(I, Count: Integer): string;
begin
  Result := IntToStr(I);
  while Length(Result) < Count do
    Result := '0' + Result;
end;

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
    Result := Result + [cDescription];

  if (ATypeID = cMovie) or (ATypeID = cXbox360) then
    Result := Result + [cPicture];

  if (ATypeID = cMovie) or (ATypeID = cPCGames) then
    Result := Result + [cGenre];

  if (ATypeID = cMovie) then
    Result := Result + [cAudioStream, cRuntime, cVideoStream];
end;

function TXrelTo.InternalGetControlIDDefaultValue;
begin
  Result := True;

  if (cPicture = AControlID) or (cNFO = AControlID) then
    Result := False;
end;

function TXrelTo.InternalGetDependentControlIDs;
begin
  Result := [];
end;

function TXrelTo.InternalExecute;
const
  xrelmonth: array [0 .. 11] of string = ('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez');
  xrelurl = 'http://www.xrel.to/';
var
  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ReleaseName, ProductInformationPageLink, ResponseStrReleaseInformation, ResponseStrProductInformation, s: string;
begin
  ReleaseName := AControlController.FindControl(cReleaseName).Value;

  HTTPRequest := THTTPRequest.Create(xrelurl + 'search.html?xrel_search_query=' + ReleaseName);
  HTTPRequest.Referer := xrelurl + 'home.html';

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
              AControlController.FindControl(cReleaseDate).AddProposedValue(GetName, FormatInt(StrToInt(Match[1]), 2) + '.' + FormatInt(IndexText(Match[2], xrelmonth) + 1, 2) + '.' + Match[3]);
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
      RequestID2 := HTTPManager.Get(xrelurl + ProductInformationPageLink, RequestID1, TPlugInHTTPOptions.Create(Self));

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
                  s := xrelurl + Match[1];

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
                  AControlController.FindControl(cGenre).AddProposedValue(GetName, s);
              until not ExecNext;
            end;
          finally
            Free;
          end;
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
