library cduniversecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCduniverseCom in 'uCduniverseCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCduniverseCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
