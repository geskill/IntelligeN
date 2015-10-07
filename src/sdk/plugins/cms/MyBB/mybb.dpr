library mybb;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uMyBB in 'uMyBB.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TMyBB.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
