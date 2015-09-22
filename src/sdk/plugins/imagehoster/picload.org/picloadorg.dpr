library picloadorg;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uPicloadOrg in 'uPicloadOrg.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TPicloadOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
