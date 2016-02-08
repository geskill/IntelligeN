program PluginWizard;

uses
  Forms,
  SysUtils,
  uPluginWizard in 'uPluginWizard.pas' {fPluginWizard};

{$R *.res}

var
  TemplatePath: TFileName;

begin
  if (ParamCount > 0) then
    TemplatePath := ParamStr(1)
  else
    raise Exception.Create('No template path assigned.');

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfPluginWizard, fPluginWizard);

  fPluginWizard.TemplatePath := TemplatePath;

  Application.Run;
end.
