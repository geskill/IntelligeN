library speedshareorg;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uSpeedshareOrg in 'uSpeedshareOrg.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TSpeedshareOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
