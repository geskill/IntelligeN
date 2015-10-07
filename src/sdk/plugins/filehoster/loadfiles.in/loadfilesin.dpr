library loadfilesin;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uLoadfilesIn in 'uLoadfilesIn.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TLoadfilesIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
