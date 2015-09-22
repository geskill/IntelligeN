library wordpress;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uWordPress in 'uWordPress.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TWordPress.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
