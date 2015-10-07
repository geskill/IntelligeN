library customscript;



uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCustomScript in 'uCustomScript.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCustomScript.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
