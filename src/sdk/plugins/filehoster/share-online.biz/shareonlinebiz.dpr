library shareonlinebiz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uShareOnlineBiz in 'uShareOnlineBiz.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TShareOnlineBiz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
