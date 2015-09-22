library nfodbru;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uNfodbRu in 'uNfodbRu.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNfodbRu.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
