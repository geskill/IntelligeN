library tolinkto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uTolinkto in 'uTolinkto.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TTolinkto.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
