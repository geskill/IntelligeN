library thaliade;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uThaliaDe in 'uThaliaDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TThaliaDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
