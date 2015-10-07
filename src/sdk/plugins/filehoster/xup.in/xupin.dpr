library xupin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uXupIn in 'uXupIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TXupIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
