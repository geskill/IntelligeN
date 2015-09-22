library imgurcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uImgurCom in 'uImgurCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IImageHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TImgurCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
