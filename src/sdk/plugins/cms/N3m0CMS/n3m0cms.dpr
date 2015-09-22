library n3m0cms;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uN3m0CMS in 'uN3m0CMS.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICMSPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TN3m0CMS.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
