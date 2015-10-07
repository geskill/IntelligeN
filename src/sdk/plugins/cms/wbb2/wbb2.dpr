library wbb2;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB2 in 'uwBB2.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TwBB2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
