library pornofilmvertriebde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uPornofilmVertriebDe in 'uPornofilmVertriebDe.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICrawlerPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TPornofilmVertriebDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
