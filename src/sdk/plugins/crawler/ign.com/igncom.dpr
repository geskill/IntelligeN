library igncom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uIgnCom in 'uIgnCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TIgnCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
