unit uXrelTo;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TXrelTo = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override;
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

function TXrelTo.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360, cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TXrelTo.GetAvailableControlIDs;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cReleaseDate, cTitle, cNFO];

  if not(_TemplateTypeID = cXXX) then
    _ComponentIDs := _ComponentIDs + [cDescription];

  if (_TemplateTypeID = cMovie) or (_TemplateTypeID = cXbox360) then
    _ComponentIDs := _ComponentIDs + [cPicture];

  if (_TemplateTypeID = cMovie) or (_TemplateTypeID = cPCGames) then
    _ComponentIDs := _ComponentIDs + [cGenre];

  if (_TemplateTypeID = cMovie) then
    _ComponentIDs := _ComponentIDs + [cAudioStream, cRuntime, cVideoStream];

  Result := LongWord(_ComponentIDs);
end;

function TXrelTo.GetControlIDDefaultValue;
var
  _ComponentID: TControlID;
begin
  _ComponentID := TControlID(AControlID);

  Result := True;

  if (cPicture = _ComponentID) or (cNFO = _ComponentID) then
    Result := False;
end;

function TXrelTo.GetResultsLimitDefaultValue;
begin
  Result := 0;
end;

function TXrelTo.Exec;
const
  xrelmonth: array [0 .. 11] of string = ('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez');
  xrelurl = 'http://www.xrel.to/';
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;

  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ReleaseName, ProductInformationPageLink, ResponseStrReleaseInformation, ResponseStrProductInformation, s: string;
begin
  _TemplateTypeID := TTypeID(ATypeID);
  LongWord(_ComponentIDs) := AControlIDs;

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
    if (cReleaseDate in _ComponentIDs) and Assigned(AControlController.FindControl(cReleaseDate)) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStrReleaseInformation;
          Expression := 'Zeit: <\/div> <div class="l_right">\s+(\d+)\. ([a-zA-Z]{3}) (\d{4}),';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cReleaseDate).AddProposedValue(GetName, FormatInt(StrToInt(Match[1]), 2) + '.' + FormatInt(IndexText(Match[2], xrelmonth) + 1,
                  2) + '.' + Match[3]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
      end;
    end;

    if (cNFO in _ComponentIDs) then
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

    if (_TemplateTypeID = cMovie) then
    begin
      if (AControlController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Audio-Stream: </div> <div class="l_right"> (.*?) </div>';
            if Exec(InputString) then
              AControlController.FindControl(cAudioStream).AddProposedValue(GetName, Match[1]);
          finally
            Free;
          end;

      if (AControlController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
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

    if (cTitle in _ComponentIDs) or (cDescription in _ComponentIDs) or (cGenre in _ComponentIDs) then
    begin
      RequestID2 := HTTPManager.Get(xrelurl + ProductInformationPageLink, RequestID1, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID2);

      ResponseStrProductInformation := HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode;

      if (AControlController.FindControl(cTitle) <> nil) and (cTitle in _ComponentIDs) then
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

      if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
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

      if (AControlController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Laufzeit:</div> <div class="l_right" title="(\d+) ';

            if Exec(InputString) then
              AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
          finally
            Free;
          end;

      if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
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

      if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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

end.
