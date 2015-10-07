library netuploadedcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uNetuploadedCom in 'uNetuploadedCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TNetuploadedCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
