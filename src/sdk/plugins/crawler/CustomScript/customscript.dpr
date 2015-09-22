library customscript;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCustomScript in 'uCustomScript.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCustomScript.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
