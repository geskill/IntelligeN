library ncryptin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uNcryptIn in 'uNcryptIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TNcryptIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
