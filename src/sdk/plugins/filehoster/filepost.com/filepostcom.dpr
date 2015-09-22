library filepostcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilepostCom in 'uFilepostCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TFilepostCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
