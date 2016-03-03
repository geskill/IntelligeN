library relinkto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uRelinkTo in 'uRelinkTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TRelinkTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
