library zelluloidde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uZelluloidDe in 'uZelluloidDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TZelluloidDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
