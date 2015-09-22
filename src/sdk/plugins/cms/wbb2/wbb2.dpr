library wbb2;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uwBB2 in 'uwBB2.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TwBB2.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
