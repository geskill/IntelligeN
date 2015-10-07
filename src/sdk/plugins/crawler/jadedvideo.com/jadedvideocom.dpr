library jadedvideocom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uJadedvideoCom in 'uJadedvideoCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TJadedvideoCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
