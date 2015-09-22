library wbb3;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB3 in 'uwBB3.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TwBB3.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
