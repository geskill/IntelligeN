library youpicin;

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uYoupicIn in 'uYoupicIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TYoupicIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
