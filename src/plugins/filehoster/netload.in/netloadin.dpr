library netloadin;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uNetloadIn in 'uNetloadIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNetloadIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
