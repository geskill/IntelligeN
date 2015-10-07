library imgurcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInImageHosterClass,
  uImgurCom in 'uImgurCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TImgurCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
