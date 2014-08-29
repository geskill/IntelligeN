library phpbb2;

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uphpbb2 in 'uphpbb2.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tphpbb2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
