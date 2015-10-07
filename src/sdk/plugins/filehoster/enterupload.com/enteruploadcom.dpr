library enteruploadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uEnteruploadCom in 'uEnteruploadCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TEnteruploadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
