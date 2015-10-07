library wcryptin;

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uWcryptIn in 'uWcryptIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TWcryptIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
