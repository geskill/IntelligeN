library criptto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uCriptto in 'uCriptto.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCriptto.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
