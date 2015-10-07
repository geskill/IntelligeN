library hotfilecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uHotfileCom in 'uHotfileCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := THotfileCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
