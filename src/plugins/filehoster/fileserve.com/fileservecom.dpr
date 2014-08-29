library fileservecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFileserveCom in 'uFileserveCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFileserveCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
