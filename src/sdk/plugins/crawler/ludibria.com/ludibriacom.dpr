library ludibriacom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uLudibriaCom in 'uLudibriaCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLudibriaCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
