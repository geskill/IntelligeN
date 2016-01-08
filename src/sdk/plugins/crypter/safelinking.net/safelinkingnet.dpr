library safelinkingnet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uSafelinkingNet in 'uSafelinkingNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tsafelinkingnet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
