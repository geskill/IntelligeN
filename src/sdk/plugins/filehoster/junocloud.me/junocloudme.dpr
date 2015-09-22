library junocloudme;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uJunocloudMe in 'uJunocloudMe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TJunocloudMe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
