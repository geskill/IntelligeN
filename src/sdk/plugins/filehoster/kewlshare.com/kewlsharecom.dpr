library kewlsharecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uKewlshareCom in 'uKewlshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TKewlshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
