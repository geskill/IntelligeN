library wiiboxartcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uWiiboxartCom in 'uWiiboxartCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TWiiboxartCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
