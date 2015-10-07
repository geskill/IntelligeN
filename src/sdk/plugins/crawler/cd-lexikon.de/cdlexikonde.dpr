library cdlexikonde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uCdLexikonDe in 'uCdLexikonDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TCdLexikonDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
