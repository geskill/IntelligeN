library exsnoiporg;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uExsnoipOrg in 'uExsnoipOrg.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TExsnoipOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
