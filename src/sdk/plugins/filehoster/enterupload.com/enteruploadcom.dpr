library enteruploadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uEnteruploadCom in 'uEnteruploadCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TEnteruploadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
