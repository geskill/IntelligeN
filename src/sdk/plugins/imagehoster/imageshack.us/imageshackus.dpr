library imageshackus;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uImageshackUs in 'uImageshackUs.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TImageshackUs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
