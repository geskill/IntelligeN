library joomla;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uJoomla in 'uJoomla.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TJoomla.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
