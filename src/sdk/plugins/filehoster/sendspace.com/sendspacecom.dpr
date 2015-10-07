library sendspacecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uSendspaceCom in 'uSendspaceCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TSendspaceCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
