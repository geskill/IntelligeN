library shraglecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uShragleCom in 'uShragleCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TShragleCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
