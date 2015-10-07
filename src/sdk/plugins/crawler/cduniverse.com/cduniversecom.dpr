library cduniversecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCduniverseCom in 'uCduniverseCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCduniverseCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
