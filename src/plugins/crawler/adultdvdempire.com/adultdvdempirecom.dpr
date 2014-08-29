library adultdvdempirecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uAdultdvdempireCom in 'uAdultdvdempireCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TAdultdvdempireCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
