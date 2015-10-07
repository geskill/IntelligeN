library binsearchinfo;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uBinsearchInfo in 'uBinsearchInfo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TBinsearchInfo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
