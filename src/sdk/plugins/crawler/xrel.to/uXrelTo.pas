unit uXrelTo;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TXrelTo = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
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

function TXrelTo.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360, cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TXrelTo.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

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

function TXrelTo.GetComponentIDDefaultValue;
var
  _ComponentID: TComponentID;
begin
  _ComponentID := TComponentID(ComponentID);

  Result := True;

  if (cPicture = _ComponentID) or (cNFO = _ComponentID) then
    Result := False;
end;

function TXrelTo.GetLimitDefaultValue;
begin
  Result := 0;
end;

procedure TXrelTo.Exec;
const
  xrelmonth: array [0 .. 11] of string = ('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez');
  xrelurl = 'http://www.xrel.to/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;

  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ReleaseName, ProductInformationPageLink, ResponseStrReleaseInformation, ResponseStrProductInformation, s: string;
begin
  _TemplateTypeID := TTemplateTypeID(ATemplateTypeID);
  LongWord(_ComponentIDs) := AComponentIDs;

  ReleaseName := AComponentController.FindControl(cReleaseName).Value;

  HTTPRequest := THTTPRequest.Create(xrelurl + 'search.html?xrel_search_query=' + ReleaseName);
  HTTPRequest.Referer := xrelurl + 'home.html';

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrReleaseInformation := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if Pos('nfo_title', ResponseStrReleaseInformation) > 0 then
  begin
    if (cReleaseDate in _ComponentIDs) and Assigned(AComponentController.FindControl(cReleaseDate)) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStrReleaseInformation;
          Expression := 'Zeit: <\/div> <div class="l_right">\s+(\d+)\. ([a-zA-Z]{3}) (\d{4}),';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cReleaseDate).AddValue(FormatInt(StrToInt(Match[1]), 2) + '.' + FormatInt(IndexText(Match[2], xrelmonth) + 1,
                  2) + '.' + Match[3], GetName);
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
              AComponentController.FindControl(cNFO).AddValue(Trim(HTML2Text(Match[1], False)), GetName);
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
      if (AComponentController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Audio-Stream: </div> <div class="l_right"> (.*?) </div>';
            if Exec(InputString) then
              AComponentController.FindControl(cAudioStream).AddValue(Match[1], GetName);
          finally
            Free;
          end;

      if (AComponentController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Video-Stream: </div> <div class="l_right"> (.*?) </div>';

            if Exec(InputString) then
              AComponentController.FindControl(cVideoStream).AddValue(Match[1], GetName);
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

      if (AComponentController.FindControl(cTitle) <> nil) and (cTitle in _ComponentIDs) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrProductInformation;
            Expression := '<h3>(.*?)<\/h3>';

            if Exec(InputString) then
            begin
              repeat
                AComponentController.FindControl(cTitle).AddValue(HTML2Text(Match[1]), GetName);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
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

                  AComponentController.FindControl(cPicture).AddValue(s, GetName);
                end;
              until not ExecNext;
            end;
          finally
            Free;
          end;

      if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
        with TRegExpr.Create do
          try
            InputString := ResponseStrProductInformation;
            Expression := 'Laufzeit:</div> <div class="l_right" title="(\d+) ';

            if Exec(InputString) then
              AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
          finally
            Free;
          end;

      if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrProductInformation;
            Expression := '<div class="article_text" style="margin:0;"> (.*?)(<table| <\/div>)';

            if Exec(InputString) then
            begin
              repeat
                AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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
                          AComponentController.FindControl(cGenre).AddValue(Trim(Match[1]), GetName);
                        until not ExecNext;
                      end;
                    finally
                      Free;
                    end;
                  end;
                end
                else
                  AComponentController.FindControl(cGenre).AddValue(s, GetName);
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
