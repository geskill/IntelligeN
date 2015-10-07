library filejunglecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilejungleCom in 'uFilejungleCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilejungleCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
