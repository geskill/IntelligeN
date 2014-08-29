library loadit;

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uLoadIT in 'uLoadIT.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLoadIT.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
