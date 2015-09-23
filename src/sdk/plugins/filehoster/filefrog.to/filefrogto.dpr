library filefrogto;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilefrogTo in 'uFilefrogTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilefrogTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.