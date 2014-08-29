library turbobitnet;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uTurbobitNet in 'uTurbobitNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TTurbobitNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
