library wuploadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uWuploadCom in 'uWuploadCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TWuploadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
