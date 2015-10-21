library intelligenxml2;

uses
  uPlugInInterfaceAdv,
  uPlugInFileFormatClass,
  uintelligenxml2 in 'uintelligenxml2.pas',
  uSelectTemplateFileName in '..\uSelectTemplateFileName.pas' {SelectTemplateFileName};

{$R *.res}

function LoadPlugin(var APlugIn: IFileFormatPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TIntelligeNXML2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
