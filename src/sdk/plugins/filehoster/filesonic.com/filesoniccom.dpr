library filesoniccom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilesonicCom in 'uFilesonicCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilesonicCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
