library coverparadiesto;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCoverParadiesTo in 'uCoverParadiesTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCoverParadiesTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
