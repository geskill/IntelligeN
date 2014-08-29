library youpicin;

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uYoupicIn in 'uYoupicIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TYoupicIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
