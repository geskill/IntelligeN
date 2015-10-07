library picloadorg;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uPicloadOrg in 'uPicloadOrg.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TPicloadOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
