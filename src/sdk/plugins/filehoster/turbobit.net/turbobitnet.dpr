library turbobitnet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uTurbobitNet in 'uTurbobitNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TTurbobitNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
