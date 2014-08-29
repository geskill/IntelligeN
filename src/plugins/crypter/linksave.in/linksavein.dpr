library linksavein;

uses
  uPlugInInterface,
  uPlugInCrypterClass,
  uLinksaveIn in 'uLinksaveIn.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrypterPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TLinksaveIn.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
