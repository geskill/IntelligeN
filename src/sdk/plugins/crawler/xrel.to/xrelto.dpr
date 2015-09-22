library xrelto;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uXrelTo in 'uXrelTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TXrelTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
