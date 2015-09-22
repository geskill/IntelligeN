library filefactorycom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilefactoryCom in 'uFilefactoryCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilefactoryCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
