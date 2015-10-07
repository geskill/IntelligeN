library extabitcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uExtabitCom in 'uExtabitCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TExtabitCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
