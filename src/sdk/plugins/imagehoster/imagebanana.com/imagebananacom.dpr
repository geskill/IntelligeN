library imagebananacom;

{$R *.dres}

uses
  uPlugInInterface,
  uImagebananaCom in 'uImagebananaCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TImagebananaCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
