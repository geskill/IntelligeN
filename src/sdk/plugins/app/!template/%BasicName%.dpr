library %BasicName%;

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  u%FullName% in 'u%FullName%.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := T%FullName%.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
