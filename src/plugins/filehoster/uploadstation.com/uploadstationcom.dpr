library uploadstationcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadstationCom in 'uUploadstationCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TUploadstationCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
