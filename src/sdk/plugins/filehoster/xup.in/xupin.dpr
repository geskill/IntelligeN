library xupin;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uXupIn in 'uXupIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TXupIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
