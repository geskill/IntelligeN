library vbulletin_randomquestion;

uses
  uPlugInInterface,
  uPlugInCAPTCHAClass,
  uvBulletin_RandomQuestion in 'uvBulletin_RandomQuestion.pas';

{$R *.res}

function LoadPlugin(var PlugIn: ICAPTCHAPlugIn): Boolean; stdcall; export;
begin
  try
    PlugIn := TvBulletin_RandomQuestion.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
