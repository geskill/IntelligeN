library imdbcom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uImdbCom in 'uImdbCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TImdbCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
