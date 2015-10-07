library htw;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uHTW in 'uHTW.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := THTW.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
