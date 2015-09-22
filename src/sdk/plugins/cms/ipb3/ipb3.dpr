library ipb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uipb3 in 'uipb3.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tipb3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
