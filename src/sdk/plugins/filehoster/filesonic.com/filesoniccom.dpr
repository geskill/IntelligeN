library filesoniccom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilesonicCom in 'uFilesonicCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilesonicCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
