library nintendolifecom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uNintendolifeCom in 'uNintendolifeCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TNintendolifeCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
