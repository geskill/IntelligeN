library extractedpostcode1;

uses
  uPlugInInterface,
  uPlugInFileFormatClass,
  uExtractedPostCode1 in 'uExtractedPostCode1.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileFormatPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Textractedpostcode.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
