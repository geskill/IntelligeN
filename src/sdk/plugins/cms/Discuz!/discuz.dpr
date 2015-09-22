library discuz;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDiscuz in 'uDiscuz.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDiscuz.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
