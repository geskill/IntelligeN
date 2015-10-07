library megauploadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uMegauploadCom in 'uMegauploadCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TMegauploadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
