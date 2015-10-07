library filefactorycom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilefactoryCom in 'uFilefactoryCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilefactoryCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
