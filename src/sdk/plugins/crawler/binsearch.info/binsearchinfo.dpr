library binsearchinfo;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uBinsearchInfo in 'uBinsearchInfo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TBinsearchInfo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
