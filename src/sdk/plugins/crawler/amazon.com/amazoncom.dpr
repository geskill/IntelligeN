library amazoncom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uAmazonCom in 'uAmazonCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TAmazonCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
