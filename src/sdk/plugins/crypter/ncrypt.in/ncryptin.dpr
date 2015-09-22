library ncryptin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uNcryptIn in 'uNcryptIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): WordBool; stdcall; export;
begin
  try
    PlugIn := TNcryptIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
