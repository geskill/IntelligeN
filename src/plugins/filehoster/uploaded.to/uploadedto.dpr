library uploadedto;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadedTo in 'uUploadedTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TUploadedTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
