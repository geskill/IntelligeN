library zsharenet;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uZshareNet in 'uZshareNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TZshareNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
