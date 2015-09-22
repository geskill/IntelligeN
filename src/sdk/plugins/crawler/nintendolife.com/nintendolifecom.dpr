library nintendolifecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uNintendolifeCom in 'uNintendolifeCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNintendolifeCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
