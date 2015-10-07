library %BasicName%;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  u%FullName% in 'u%FullName%.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := T%FullName%.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
