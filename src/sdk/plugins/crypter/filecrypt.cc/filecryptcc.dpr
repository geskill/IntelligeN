library filecryptcc;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uFilecryptCc in 'uFilecryptCc.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrypterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TFilecryptCc.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
