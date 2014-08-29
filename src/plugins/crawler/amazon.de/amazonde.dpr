library amazonde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uAmazonDe in 'uAmazonDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TAmazonDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
