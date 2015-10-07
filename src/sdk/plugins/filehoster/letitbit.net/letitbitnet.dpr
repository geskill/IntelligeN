library letitbitnet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLetitbitNet in 'uLetitbitNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLetitbitNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
