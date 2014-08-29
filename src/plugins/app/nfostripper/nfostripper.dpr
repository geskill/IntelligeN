library nfostripper;

uses
  uPlugInInterface,
  uPlugInAppClass,
  uNfoStripper in 'uNfoStripper.pas',
  uNFOHelper in 'uNFOHelper.pas';

{$R *.res}

function LoadPlugin(var PlugIn: IAppPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TNfoStripper.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
