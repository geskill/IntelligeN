library imagesgooglede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uImagesGoogleDe in 'uImagesGoogleDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TImagesGoogleDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';
begin
end.
