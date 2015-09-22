library sexvideoallcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uSexvideoallCom in 'uSexvideoallCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TSexvideoallCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
