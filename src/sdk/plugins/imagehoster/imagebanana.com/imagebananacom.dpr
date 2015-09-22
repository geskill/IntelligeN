library imagebananacom;

{$R *.dres}

uses
  uPlugInInterface,
  uImagebananaCom in 'uImagebananaCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TImagebananaCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
