library depositfilescom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uDepositfilesCom in 'uDepositfilesCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDepositfilesCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
