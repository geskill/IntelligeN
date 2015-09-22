library linksavein;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uLinksaveIn in 'uLinksaveIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): WordBool; stdcall; export;
begin
  try
    PlugIn := TLinksaveIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
