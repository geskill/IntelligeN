library ugotfilecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUgotfileCom in 'uUgotfileCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TUgotfileCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
