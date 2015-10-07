library gamestarde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamestarDe in 'uGamestarDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TGamestarDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
