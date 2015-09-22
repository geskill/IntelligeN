library letitbitnet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLetitbitNet in 'uLetitbitNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLetitbitNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
