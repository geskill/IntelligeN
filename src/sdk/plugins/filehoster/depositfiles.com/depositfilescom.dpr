library depositfilescom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uDepositfilesCom in 'uDepositfilesCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDepositfilesCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
