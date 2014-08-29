library relinkus;

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uRelinkUs in 'uRelinkUs.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TRelinkUs.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
