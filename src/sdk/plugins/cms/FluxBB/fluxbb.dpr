library fluxbb;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  ufluxbb in 'ufluxbb.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tfluxbb.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
