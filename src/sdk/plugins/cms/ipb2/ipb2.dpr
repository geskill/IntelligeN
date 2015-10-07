library ipb2;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uipb2 in 'uipb2.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tipb2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
