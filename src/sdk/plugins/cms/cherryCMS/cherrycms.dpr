library cherrycms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  ucherryCMS in 'ucherryCMS.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TcherryCMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
