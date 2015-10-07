library cloudzernet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uCloudzerNet in 'uCloudzerNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCloudzerNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
