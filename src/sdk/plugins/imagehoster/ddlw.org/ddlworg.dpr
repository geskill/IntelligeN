library ddlworg;

{$R *.dres}

uses
  uPlugInInterface,
  uDdlwOrg in 'uDdlwOrg.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDdlwOrg.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
