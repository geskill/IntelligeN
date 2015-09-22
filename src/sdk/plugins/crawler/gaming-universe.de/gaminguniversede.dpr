library gaminguniversede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamingUniverseDe in 'uGamingUniverseDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TGamingUniverseDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
