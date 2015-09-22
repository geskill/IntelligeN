library icms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uICMS in 'uICMS.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TICMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
