library customscript;

{$R *.dres}

uses
  uPlugInInterfaceAdv,
  uPlugInAppClass,
  uCustomScript in 'uCustomScript.pas',
  uCustomScriptSettingsForm in 'uCustomScriptSettingsForm.pas' {fCustomScriptSettingsForm},
  uCustomScriptSettings in 'uCustomScriptSettings.pas',
  uCustomScriptEngine in 'uCustomScriptEngine.pas';

{$R *.res}

function LoadPlugin(var APlugIn: IAppPlugIn): WordBool; safecall; export;
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
