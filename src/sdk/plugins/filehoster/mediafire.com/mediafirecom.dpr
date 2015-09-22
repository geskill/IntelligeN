library mediafirecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uMediafireCom in 'uMediafireCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TMediafireCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
