library megauploadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uMegauploadCom in 'uMegauploadCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TMegauploadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
