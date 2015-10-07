library uploadstationcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadstationCom in 'uUploadstationCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TUploadstationCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
