library ifileit;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uIfileIt in 'uIfileIt.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TIfileIt.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
