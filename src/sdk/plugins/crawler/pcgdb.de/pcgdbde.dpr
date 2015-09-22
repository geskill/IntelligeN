library pcgdbde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uPcgdbDe in 'uPcgdbDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TPcgdbDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
