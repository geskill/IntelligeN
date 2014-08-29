library filejunglecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilejungleCom in 'uFilejungleCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilejungleCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
