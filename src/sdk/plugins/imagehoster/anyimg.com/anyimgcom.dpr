library anyimgcom;

{$R *.dres}

uses
  uPlugInInterface,
  uAnyimgCom in 'uAnyimgCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TAnyimgCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
