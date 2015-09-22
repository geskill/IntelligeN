library sendspacecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uSendspaceCom in 'uSendspaceCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TSendspaceCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
