library sharelinksbiz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uShareLinksBiz in 'uShareLinksBiz.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TShareLinksBiz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
