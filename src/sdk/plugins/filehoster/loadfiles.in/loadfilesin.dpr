library loadfilesin;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLoadfilesIn in 'uLoadfilesIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLoadfilesIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.