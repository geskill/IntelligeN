library pcgdbde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uPcgdbDe in 'uPcgdbDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TPcgdbDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
