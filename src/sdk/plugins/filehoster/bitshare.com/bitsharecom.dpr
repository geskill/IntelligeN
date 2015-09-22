library bitsharecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uBitshareCom in 'uBitshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TBitshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
