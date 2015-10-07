library vbulletin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uvBulletin in 'uvBulletin.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TvBulletin.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin

end.
