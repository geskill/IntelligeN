library gamespotcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uGamespotCom in 'uGamespotCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TGamespotCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
