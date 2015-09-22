library zelluloidde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uZelluloidDe in 'uZelluloidDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TZelluloidDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
