library filestoreto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilestoreTo in 'uFilestoreTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilestoreTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
