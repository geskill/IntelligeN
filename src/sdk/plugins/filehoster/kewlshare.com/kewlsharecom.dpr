library kewlsharecom;

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uKewlshareCom in 'uKewlshareCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TKewlshareCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
