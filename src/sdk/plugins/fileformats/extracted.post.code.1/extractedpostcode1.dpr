library extractedpostcode1;

uses
  uPlugInInterfaceAdv,
  uPlugInFileFormatClass,
  uExtractedPostCode1 in 'uExtractedPostCode1.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileFormatPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Textractedpostcode.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
