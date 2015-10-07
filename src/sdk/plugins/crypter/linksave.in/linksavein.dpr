library linksavein;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uLinksaveIn in 'uLinksaveIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLinksaveIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
