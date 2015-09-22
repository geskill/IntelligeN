library scepereu;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uSceperEu in 'uSceperEu.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TSceperEu.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
