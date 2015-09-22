library freaksharecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFreakshareCom in 'uFreakshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFreakshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
