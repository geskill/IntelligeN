unit uMoviemazeDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TMoviemazeDe = class(TCrawlerPlugIn)
  protected
    function MoviemazeHTMLDescription2Text(AHtmlContent: string): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
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

function TMoviemazeDe.GetName: WideString;
begin
  Result := 'Moviemaze.de';
end;

function TMoviemazeDe.GetAvailableTemplateTypeIDs: Integer;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TMoviemazeDe.GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cRuntime, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TMoviemazeDe.GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean;
begin
  Result := True;
end;

function TMoviemazeDe.GetLimitDefaultValue: Integer;
begin
  Result := 5;
end;

procedure TMoviemazeDe.Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController);
const
  website: string = 'http://www.moviemaze.de/';
var
  _ComponentIDs: TComponentIDs;
  _Title, s: string;
  _Count: Integer;
  _params, _postreply: TStringStream;

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

            AComponentController.FindControl(cPicture).AddValue(website + 'filme/' + _incnumber + '/poster_lg' + _picturenumber + '.jpg', GetName);
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
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
                      AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
                    until not ExecNext;
                  end;
                finally
                  Free;
                end;
              end;
            end
            else
              AComponentController.FindControl(cGenre).AddValue(s, GetName);
          end;
        finally
          Free;
        end;
      end;
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
      begin
        try
          InputString := aWebsitecode;
          Expression := 'nge:<\/span><\/td>\s+<td class="standard" valign="top"><nobr>\s+(\d+) ';

          if Exec(InputString) then
            AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
        finally
          Free;
        end;
      end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          // plot
          InputString := aWebsitecode;
          Expression := '"plot">\s+(.*?)<\/div>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(HTMLDecode(Match[1]), GetName);

          // critics
          Expression := '"summary">(.*?)<\/div><br\/>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(MoviemazeHTMLDescription2Text(Match[1]), GetName);
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
      begin
        try
          ModifierS := False;
          InputString := aWebsitecode;
          Expression := '<tr><td><a href="\/media\/poster\/(.*?)"';

          if Exec(InputString) then
          begin
            with TIdHTTPHelper.Create(Self) do
              try
                deep_picture_search(Get(website + '/media/poster/' + Match[1]));
              finally
                Free;
              end;
          end;
        finally
          Free;
        end;
      end;
  end;

begin
  LongWord(_ComponentIDs) := AComponentIDs;

  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
  begin
    with TIdHTTPHelper.Create(Self) do
      try
        _params := TStringStream.Create('', CP_UTF8);
        _postreply := TStringStream.Create('', CP_UTF8);
        try
          with _params do
          begin
            WriteString('searchword=' + _Title);
          end;

          Post(website + 'suche/result.phtml', _params, _postreply);

          if (Pos('download selection', _postreply.DataString) = 0) then
            with TRegExpr.Create do
            begin
              try
                InputString := _postreply.DataString;
                Expression := '"><a href="\/filme(.*?)">';

                if Exec(InputString) then
                begin
                  repeat
                    deep_search(Get(website + 'filme' + HTMLDecode(Match[1])));
                    Inc(_Count);
                  until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
                end;
              finally
                Free;
              end;
            end
            else
              deep_search(_postreply.DataString);
        finally
          _postreply.Free;
          _params.Free;
        end;
      finally
        Free;
      end;
  end;
end;

end.
