library intelligenxml1;

uses
  uPlugInInterfaceAdv,
  uPlugInFileFormatClass,
  uintelligenxml1 in 'uintelligenxml1.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileFormatPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tintelligenxml1.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
