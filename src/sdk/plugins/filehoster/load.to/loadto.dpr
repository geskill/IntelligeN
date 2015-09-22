library loadto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLoadTo in 'uLoadTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLoadTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
