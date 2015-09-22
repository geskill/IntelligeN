library moviemazede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uMoviemazeDe in 'uMoviemazeDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TMoviemazeDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
