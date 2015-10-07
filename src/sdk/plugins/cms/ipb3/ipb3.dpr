library ipb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uipb3 in 'uipb3.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tipb3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
