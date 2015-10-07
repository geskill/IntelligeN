library loadto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLoadTo in 'uLoadTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLoadTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
