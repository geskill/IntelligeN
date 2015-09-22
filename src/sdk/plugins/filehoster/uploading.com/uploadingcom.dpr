library uploadingcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadingCom in 'uUploadingCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TUploadingCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
