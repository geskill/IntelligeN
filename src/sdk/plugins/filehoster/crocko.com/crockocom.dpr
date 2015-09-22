library crockocom;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInFileHosterClass,
  uCrockoCom in 'uCrockoCom.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IFileHosterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCrockoCom.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports LoadPlugin name 'LoadPlugIn';

begin
end.
