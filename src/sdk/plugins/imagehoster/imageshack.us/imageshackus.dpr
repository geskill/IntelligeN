library imageshackus;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uImageshackUs in 'uImageshackUs.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TImageshackUs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
