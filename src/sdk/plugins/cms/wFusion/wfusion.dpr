library wfusion;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwFusion in 'uwFusion.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TwFusion.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
