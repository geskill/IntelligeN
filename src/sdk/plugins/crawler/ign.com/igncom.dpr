library igncom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uIgnCom in 'uIgnCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TIgnCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
