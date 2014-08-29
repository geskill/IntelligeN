library kickloadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uKickloadCom in 'uKickloadCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TKickloadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
