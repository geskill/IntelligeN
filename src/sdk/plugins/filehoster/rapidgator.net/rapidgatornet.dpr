library rapidgatornet;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uRapidgatorNet in 'uRapidgatorNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TRapidgatorNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
