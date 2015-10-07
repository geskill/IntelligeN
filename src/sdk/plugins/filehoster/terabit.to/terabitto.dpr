library terabitto;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uTerabitTo in 'uTerabitTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TTerabitTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
