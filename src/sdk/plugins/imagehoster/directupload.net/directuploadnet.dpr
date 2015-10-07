library directuploadnet;

{$R *.dres}

uses
  uPlugInInterface,
  uDirectuploadNet in 'uDirectuploadNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDirectuploadNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
