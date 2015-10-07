library kickloadcom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uKickloadCom in 'uKickloadCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TKickloadCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
