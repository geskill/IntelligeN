library phpbb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uphpbb3 in 'uphpbb3.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tphpbb3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
