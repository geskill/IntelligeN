library icms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uICMS in 'uICMS.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TICMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
