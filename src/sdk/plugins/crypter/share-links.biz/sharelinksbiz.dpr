library sharelinksbiz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uShareLinksBiz in 'uShareLinksBiz.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TShareLinksBiz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
