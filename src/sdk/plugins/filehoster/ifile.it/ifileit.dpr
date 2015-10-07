library ifileit;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uIfileIt in 'uIfileIt.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TIfileIt.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
