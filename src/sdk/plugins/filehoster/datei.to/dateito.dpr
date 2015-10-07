library dateito;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uDateiTo in 'uDateiTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDateiTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
