library fileservecom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uFileserveCom in 'uFileserveCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFileserveCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
