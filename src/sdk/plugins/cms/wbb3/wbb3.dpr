library wbb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB3 in 'uwBB3.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TwBB3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
