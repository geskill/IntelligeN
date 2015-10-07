library crockocom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uCrockoCom in 'uCrockoCom.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IFileHosterPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCrockoCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
