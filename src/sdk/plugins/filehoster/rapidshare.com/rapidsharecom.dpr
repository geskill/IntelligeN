library rapidsharecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uRapidshareCom in 'uRapidshareCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TRapidshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
