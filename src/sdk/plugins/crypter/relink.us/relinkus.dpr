library relinkus;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uRelinkUs in 'uRelinkUs.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): WordBool; stdcall; export;
begin
  try
    PlugIn := TRelinkUs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
