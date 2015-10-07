library releasenamereader;

uses
  uPlugInInterfaceAdv,
  uPlugInFileFormatClass,
  uReleasenameReader in 'uReleasenameReader.pas',
  uSelectTemplateFileName in '..\uSelectTemplateFileName.pas' {SelectTemplateFileName};

{$R *.res}

function LoadPlugin(var APlugIn: IFileFormatPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TReleasenameReader.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
