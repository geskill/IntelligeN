library jadedvideocom;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uJadedvideoCom in 'uJadedvideoCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TJadedvideoCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
