library wcryptin;

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uWcryptIn in 'uWcryptIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TWcryptIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
