library xonaircom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uXonairCom in 'uXonairCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TXonairCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
