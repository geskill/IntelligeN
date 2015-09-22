library pornofilmvertriebde;

uses
  uPlugInInterface,
  uPlugInCrawlerClass,
  uPornofilmVertriebDe in 'uPornofilmVertriebDe.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICrawlerPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TPornofilmVertriebDe.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
