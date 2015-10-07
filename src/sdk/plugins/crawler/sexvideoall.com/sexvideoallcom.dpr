library sexvideoallcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uSexvideoallCom in 'uSexvideoallCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TSexvideoallCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
