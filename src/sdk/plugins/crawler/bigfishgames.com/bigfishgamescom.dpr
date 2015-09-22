library bigfishgamescom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uBigfishgamesCom in 'uBigfishgamesCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TBigfishgamesCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';


begin
end.
