library xrelto;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uXrelTo in 'uXrelTo.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; stdcall; export;
begin
  try
    APlugIn := TXrelTo.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
