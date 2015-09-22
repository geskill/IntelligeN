library rapidgatornet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uRapidgatorNet in 'uRapidgatorNet.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TRapidgatorNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
