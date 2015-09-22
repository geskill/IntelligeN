library imdbcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uImdbCom in 'uImdbCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TImdbCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
