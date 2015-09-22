library tinypiccom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uTinypicCom in 'uTinypicCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TTinypicCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
