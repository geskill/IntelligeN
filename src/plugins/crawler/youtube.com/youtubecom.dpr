library youtubecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uYoutubeCom in 'uYoutubeCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TYoutubeCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
