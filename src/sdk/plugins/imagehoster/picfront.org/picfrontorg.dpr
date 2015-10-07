library picfrontorg;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uPicfrontOrg in 'uPicfrontOrg.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TPicfrontOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
