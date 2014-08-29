library netuploadedcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uNetuploadedCom in 'uNetuploadedCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNetuploadedCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
