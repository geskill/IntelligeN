library erodvdnl;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uErodvdNl in 'uErodvdNl.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TErodvdNl.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
