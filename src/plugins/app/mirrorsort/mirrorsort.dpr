library mirrorsort;

uses
  uPlugInInterface,
  uPlugInAppClass,
  uMirrorSort in 'uMirrorSort.pas' {fMirrorSort};

{$R *.res}

function LoadPlugin(var PlugIn: IAppPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TMirrorSort.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
