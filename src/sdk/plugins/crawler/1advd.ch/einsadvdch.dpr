library einsadvdch;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  u1advdCh in 'u1advdCh.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := T1advdCh.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
