library thaliade;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uThaliaDe in 'uThaliaDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TThaliaDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
