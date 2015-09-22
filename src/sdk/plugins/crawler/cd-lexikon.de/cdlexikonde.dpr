library cdlexikonde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCdLexikonDe in 'uCdLexikonDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TCdLexikonDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
