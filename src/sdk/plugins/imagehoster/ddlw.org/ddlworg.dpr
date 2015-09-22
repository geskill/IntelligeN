library ddlworg;

{$R *.dres}

uses
  uPlugInInterface,
  uDdlwOrg in 'uDdlwOrg.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDdlwOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
