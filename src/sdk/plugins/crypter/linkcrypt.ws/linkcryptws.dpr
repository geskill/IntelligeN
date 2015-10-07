library linkcryptws;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uLinkcryptWs in 'uLinkcryptWs.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLinkcryptWs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
