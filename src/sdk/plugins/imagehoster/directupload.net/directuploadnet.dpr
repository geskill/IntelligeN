library directuploadnet;

{$R *.dres}

uses
  uPlugInInterface,
  uDirectuploadNet in 'uDirectuploadNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDirectuploadNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
