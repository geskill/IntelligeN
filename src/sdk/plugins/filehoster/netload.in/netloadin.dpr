library netloadin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uNetloadIn in 'uNetloadIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TNetloadIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
