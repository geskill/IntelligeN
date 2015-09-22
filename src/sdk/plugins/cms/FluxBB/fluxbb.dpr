library fluxbb;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  ufluxbb in 'ufluxbb.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tfluxbb.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
