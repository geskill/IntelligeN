library blogger;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uBlogger in 'uBlogger.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TBlogger.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
