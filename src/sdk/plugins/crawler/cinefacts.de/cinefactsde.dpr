library cinefactsde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCinefactsDe in 'uCinefactsDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCinefactsDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
