library anyimgcom;

{$R *.dres}

uses
  uPlugInInterface,
  uAnyimgCom in 'uAnyimgCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IImageHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TAnyimgCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
