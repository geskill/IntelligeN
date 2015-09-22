library remixsharecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uRemixshareCom in 'uRemixshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TRemixshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
