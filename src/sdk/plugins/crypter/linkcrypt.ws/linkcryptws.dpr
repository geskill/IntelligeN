library linkcryptws;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uLinkcryptWs in 'uLinkcryptWs.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): WordBool; stdcall; export;
begin
  try
    PlugIn := TLinkcryptWs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
