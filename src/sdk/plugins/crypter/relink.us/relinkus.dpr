library relinkus;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uRelinkUs in 'uRelinkUs.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TRelinkUs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
