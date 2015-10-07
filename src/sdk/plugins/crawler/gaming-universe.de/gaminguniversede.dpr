library gaminguniversede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamingUniverseDe in 'uGamingUniverseDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TGamingUniverseDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
