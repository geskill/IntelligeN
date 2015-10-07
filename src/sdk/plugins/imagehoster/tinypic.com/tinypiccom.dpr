library tinypiccom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uTinypicCom in 'uTinypicCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TTinypicCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
