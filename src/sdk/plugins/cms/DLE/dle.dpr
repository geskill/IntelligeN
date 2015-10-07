library dle;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDLE in 'uDLE.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDLE.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
