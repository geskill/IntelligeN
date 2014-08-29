library vbulletin;

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uvBulletin in 'uvBulletin.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TvBulletin.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin

end.
