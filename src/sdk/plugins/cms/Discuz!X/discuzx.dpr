library discuzx;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDiscuzX in 'uDiscuzX.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TDiscuzX.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
