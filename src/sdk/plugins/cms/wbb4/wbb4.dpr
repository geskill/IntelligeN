library wbb4;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB4 in 'uwBB4.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TwBB4.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
