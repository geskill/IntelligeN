library ucms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uuCMS in 'uuCMS.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TuCMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
