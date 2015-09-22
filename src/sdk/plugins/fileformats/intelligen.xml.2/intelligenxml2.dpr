library intelligenxml2;

uses
  Forms,
  uPlugInInterface,
  uPlugInFileFormatClass,
  uintelligenxml2 in 'uintelligenxml2.pas',
  uSelectTemplateFileName in '..\uSelectTemplateFileName.pas' {SelectTemplateFileName};

{$R *.res}

function LoadPlugin(var PlugIn: IFileFormatPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := Tintelligenxml2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
