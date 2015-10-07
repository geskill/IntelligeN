library zsharenet;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uZshareNet in 'uZshareNet.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TZshareNet.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
