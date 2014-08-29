library linkgrabber;

uses
  uPlugInInterface,
  uPlugInAppClass,
  uLinkGrabber in 'uLinkGrabber.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IAppPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLinkGrabber.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
