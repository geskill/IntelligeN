library xenforo;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uXenForo in 'uXenForo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TXenForo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
