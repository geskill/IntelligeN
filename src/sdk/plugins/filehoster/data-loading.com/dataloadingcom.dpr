library dataloadingcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uDataLoadingCom in 'uDataLoadingCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDataLoadingCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
