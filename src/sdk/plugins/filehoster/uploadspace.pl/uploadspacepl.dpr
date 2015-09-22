library uploadspacepl;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadspacePl in 'uUploadspacePl.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TUploadspacePl.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
