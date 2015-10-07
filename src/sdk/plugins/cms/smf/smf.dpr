library smf;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  usmf in 'usmf.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TSMF.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
