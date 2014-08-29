library speedshareorg;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uSpeedshareOrg in 'uSpeedshareOrg.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TSpeedshareOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
