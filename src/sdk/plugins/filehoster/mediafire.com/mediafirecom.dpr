library mediafirecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uMediafireCom in 'uMediafireCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TMediafireCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin

end.
