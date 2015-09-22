library bigfishgamesde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uBigfishgamesDe in 'uBigfishgamesDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TBigfishgamesDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';


begin
end.
