library intelligenxml1;

uses
  uPlugInInterface,
  uPlugInFileFormatClass,
  uintelligenxml1 in 'uintelligenxml1.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileFormatPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tintelligenxml1.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
