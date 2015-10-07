library phpbb2;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uphpbb2 in 'uphpbb2.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := Tphpbb2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
