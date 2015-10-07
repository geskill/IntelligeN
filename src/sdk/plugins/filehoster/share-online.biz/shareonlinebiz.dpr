library shareonlinebiz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uShareOnlineBiz in 'uShareOnlineBiz.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TShareOnlineBiz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
