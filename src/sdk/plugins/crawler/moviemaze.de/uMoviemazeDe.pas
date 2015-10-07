unit uMoviemazeDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TMoviemazeDe = class(TCrawlerPlugIn)
  protected
    function MoviemazeHTMLDescription2Text(AHtmlContent: string): string;
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

{ TMoviemazeDe }

function TMoviemazeDe.MoviemazeHTMLDescription2Text(AHtmlContent: string): string;
var
  Text: string;
begin
  Text := HTML2Text(AHtmlContent, False, True);
  try
    Result := HTMLDecode(Text);
  except
    Result := Text;
  end;
end;

function TMoviemazeDe.GetName;
begin
  Result := 'Moviemaze.de';
end;

function TMoviemazeDe.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TMoviemazeDe.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cRuntime, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TMoviemazeDe.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TMoviemazeDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

procedure TMoviemazeDe.Exec;
const
  website: string = 'http://www.moviemaze.de/';
var
  _ComponentIDs: TControlIDs;
  _Title: string;
  _Count: Integer;

  HTTPParams: IHTTPParams;

  RequestID1, RequestID2, RequestID3: Double;

  ResponseStrSearchResult: string;

  procedure deep_picture_search(aWebsitecode: string);
  var
    _incnumber, _picturenumber: string;
  begin
    with TRegExpr.Create do
      try
        InputString := aWebsitecode;
        Expression := '<a href="\/media\/poster\/(\d+)\/(\d+)\/';

        if Exec(InputString) then
        begin
          repeat
            _incnumber := Match[1];
            if length(_incnumber) < 4 then
              _incnumber := '0' + _incnumber;

            _picturenumber := Match[2];
            if length(_picturenumber) < 2 then
              _picturenumber := '0' + _picturenumber;

            AControlController.FindControl(cPicture).AddProposedValue(GetName, website + 'filme/' + _incnumber + '/poster_lg' + _picturenumber + '.jpg');
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

  procedure deep_search(aWebsitecode: string);
  var
    s: string;
  begin
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
      begin
        try
          ModifierS := False;
          InputString := aWebsitecode;
          Expression := '<span class="fett">Genre:<\/span><\/td>\s+<td class="standard" valign="top">\s+(.*?)\s+<\/td>';

          if Exec(InputString) then
          begin
            s := MoviemazeHTMLDescription2Text(Match[1]);
            if Pos(',', s) > 0 then
            begin
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '([^, ]+)';

                  if Exec(InputString) then
                  begin
                    repeat
                      AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
                    until not ExecNext;
                  end;
                finally
                  Free;
                end;
              end;
            end
            else
              AControlController.FindControl(cGenre).AddProposedValue(GetName, s);
          end;
        finally
          Free;
        end;
      end;
    if (AControlController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
      begin
        try
          InputString := aWebsitecode;
          Expression := 'nge:<\/span><\/td>\s+<td class="standard" valign="top"><nobr>\s+(\d+) ';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
      end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          // plot
          InputString := aWebsitecode;
          Expression := '"plot">\s+(.*?)<\/div>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, HTMLDecode(Match[1]));

          // critics
          Expression := '"summary">(.*?)<\/div><br\/>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, MoviemazeHTMLDescription2Text(Match[1]));
        finally
          Free;
        end;
    end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
      begin
        try
          ModifierS := False;
          InputString := aWebsitecode;
          Expression := '<tr><td><a href="\/media\/poster\/(.*?)"';

          if Exec(InputString) then
          begin
            RequestID3 := HTTPManager.Get(website + '/media/poster/' + Match[1], RequestID2, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID3);

            deep_picture_search(HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode);
          end;
        finally
          Free;
        end;
      end;
  end;

begin
  LongWord(_ComponentIDs) := AControlIDs;

  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  HTTPParams := THTTPParams.Create;
  HTTPParams.AddFormField('searchword', _Title);

  RequestID1 := HTTPManager.Post(THTTPRequest.Create(website + 'suche/result.phtml'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if (Pos('download selection', ResponseStrSearchResult) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ResponseStrSearchResult;
        Expression := '"><a href="\/filme(.*?)">';

        if Exec(InputString) then
        begin
          repeat

            RequestID2 := HTTPManager.Get(website + 'filme' + HTMLDecode(Match[1]), RequestID1, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID2);

            deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else
    deep_search(ResponseStrSearchResult);
end;

end.
