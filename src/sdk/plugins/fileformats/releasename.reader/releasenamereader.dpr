library releasenamereader;

uses
  uPlugInInterface,
  uPlugInFileFormatClass,
  uReleasenameReader in 'uReleasenameReader.pas',
  uSelectTemplateFileName in '..\uSelectTemplateFileName.pas' {SelectTemplateFileName};

{$R *.res}

function LoadPlugin(var PlugIn: IFileFormatPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TReleasenameReader.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
