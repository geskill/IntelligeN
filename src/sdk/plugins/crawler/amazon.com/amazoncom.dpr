library amazoncom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uAmazonCom in 'uAmazonCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TAmazonCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
