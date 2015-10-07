library bigfishgamescom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uBigfishgamesCom in 'uBigfishgamesCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TBigfishgamesCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';


begin
end.
