library terabitto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uTerabitTo in 'uTerabitTo.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TTerabitTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
