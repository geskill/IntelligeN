library uploadstubede;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uUploadstubeDe in 'uUploadstubeDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TUploadstubeDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
