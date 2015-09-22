library gamestarde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamestarDe in 'uGamestarDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TGamestarDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
