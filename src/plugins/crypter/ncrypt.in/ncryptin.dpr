library ncryptin;

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uNcryptIn in 'uNcryptIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNcryptIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
