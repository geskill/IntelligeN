library discuz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDiscuz in 'uDiscuz.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDiscuz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
