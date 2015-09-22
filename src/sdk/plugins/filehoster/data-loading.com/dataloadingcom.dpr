library dataloadingcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uDataLoadingCom in 'uDataLoadingCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDataLoadingCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
