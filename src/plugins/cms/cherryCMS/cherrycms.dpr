library cherrycms;

uses
  uPlugInInterface,
  uPlugInCMSClass,
  ucherryCMS in 'ucherryCMS.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TcherryCMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
