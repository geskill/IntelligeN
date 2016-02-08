library cloudflare;

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  uCloudflare in 'uCloudflare.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCloudflare.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
