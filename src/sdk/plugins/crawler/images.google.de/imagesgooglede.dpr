library imagesgooglede;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uImagesGoogleDe in 'uImagesGoogleDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TImagesGoogleDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';
begin
end.
