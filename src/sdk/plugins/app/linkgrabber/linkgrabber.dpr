library linkgrabber;

{$R *.dres}

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  uLinkGrabber in 'uLinkGrabber.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLinkGrabber.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
