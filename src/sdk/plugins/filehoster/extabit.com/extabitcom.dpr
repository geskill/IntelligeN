library extabitcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uExtabitCom in 'uExtabitCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TExtabitCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
