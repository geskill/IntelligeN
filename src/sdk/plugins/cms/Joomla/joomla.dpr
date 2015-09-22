library joomla;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uJoomla in 'uJoomla.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TJoomla.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
