library youtubecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uYoutubeCom in 'uYoutubeCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TYoutubeCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
