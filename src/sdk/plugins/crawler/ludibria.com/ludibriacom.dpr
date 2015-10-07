library ludibriacom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uLudibriaCom in 'uLudibriaCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLudibriaCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
