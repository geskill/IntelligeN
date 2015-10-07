library filepostcom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFilepostCom in 'uFilepostCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilepostCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
