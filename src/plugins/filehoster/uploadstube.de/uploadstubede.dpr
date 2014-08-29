library uploadstubede;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadstubeDe in 'uUploadstubeDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TUploadstubeDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
