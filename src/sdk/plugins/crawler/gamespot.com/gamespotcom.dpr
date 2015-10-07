library gamespotcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamespotCom in 'uGamespotCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TGamespotCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
