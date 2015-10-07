library wordpress;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uWordPress in 'uWordPress.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TWordPress.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
