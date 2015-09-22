library ucms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uuCMS in 'uuCMS.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TuCMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
