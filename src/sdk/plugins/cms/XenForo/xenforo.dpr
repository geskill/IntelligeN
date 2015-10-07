library xenforo;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uXenForo in 'uXenForo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TXenForo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
