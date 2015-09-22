library gametrailerscom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGametrailersCom in 'uGametrailersCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TGametrailersCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
