library wbb4;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB4 in 'uwBB4.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TwBB4.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
