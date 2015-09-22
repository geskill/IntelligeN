library filestoreto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilestoreTo in 'uFilestoreTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilestoreTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
