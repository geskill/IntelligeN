library filefrogto;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilefrogTo in 'uFilefrogTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilefrogTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
