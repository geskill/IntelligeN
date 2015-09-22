library picfrontorg;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uPicfrontOrg in 'uPicfrontOrg.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TPicfrontOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
