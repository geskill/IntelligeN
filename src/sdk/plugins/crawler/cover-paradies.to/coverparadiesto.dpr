library coverparadiesto;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCoverParadiesTo in 'uCoverParadiesTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCoverParadiesTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
