library fastpicru;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uFastpicRu in 'uFastpicRu.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFastpicRu.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
