library mirrorsort;

{$R *.dres}

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  uMirrorSort in 'uMirrorSort.pas' {fMirrorSort};

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TMirrorSort.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
