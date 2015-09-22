library %BasicName%;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  u%FullName% in 'u%FullName%.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := T%FullName%.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
