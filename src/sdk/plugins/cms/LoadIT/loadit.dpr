library loadit;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uLoadIT in 'uLoadIT.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLoadIT.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
