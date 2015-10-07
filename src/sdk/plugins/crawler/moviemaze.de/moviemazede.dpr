library moviemazede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uMoviemazeDe in 'uMoviemazeDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TMoviemazeDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
