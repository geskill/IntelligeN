library cloudzernet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uCloudzerNet in 'uCloudzerNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCloudzerNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
