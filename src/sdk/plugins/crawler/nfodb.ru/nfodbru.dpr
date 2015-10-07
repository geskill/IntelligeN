library nfodbru;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uNfodbRu in 'uNfodbRu.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TNfodbRu.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
