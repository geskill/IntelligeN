library dirwatch;

{$R *.dres}

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  uDirWatch in 'uDirWatch.pas',
  uDirWatchSettings in 'uDirWatchSettings.pas',
  uDirWatchSettingsForm in 'uDirWatchSettingsForm.pas' {fDirWatchSettingsForm},
  uDirWatchEngine in 'uDirWatchEngine.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDirWatch.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
