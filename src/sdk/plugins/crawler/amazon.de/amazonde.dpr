library amazonde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uAmazonDe in 'uAmazonDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TAmazonDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
