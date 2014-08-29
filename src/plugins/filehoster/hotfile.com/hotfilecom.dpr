library hotfilecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uHotfileCom in 'uHotfileCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := THotfileCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
