library phpbb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uphpbb3 in 'uphpbb3.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tphpbb3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
