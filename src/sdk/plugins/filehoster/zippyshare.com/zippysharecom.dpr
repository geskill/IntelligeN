library zippysharecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uZippyshareCom in 'uZippyshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TZippyshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
