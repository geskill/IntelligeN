library dle;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDLE in 'uDLE.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDLE.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
