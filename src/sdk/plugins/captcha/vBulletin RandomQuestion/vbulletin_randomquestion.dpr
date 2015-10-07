library vbulletin_randomquestion;

uses
  uPlugInInterface,
  uPlugInCAPTCHAClass,
  uvBulletin_RandomQuestion in 'uvBulletin_RandomQuestion.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICAPTCHAPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TvBulletin_RandomQuestion.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
