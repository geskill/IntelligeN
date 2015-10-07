program UpdateManager;

uses
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  ExceptionLog7,
  Forms,
  Dialogs,
  uMain in 'uMain.pas' {fMain},
  uDynamicExport in 'uDynamicExport.pas',
  uApiUpdateModel in 'uApiUpdateModel.pas',
  uApiUpdateInterface in 'uApiUpdateInterface.pas',
  uApiUpdateConst in 'uApiUpdateConst.pas',
  uApiUpdateController in 'uApiUpdateController.pas',
  uApiUpdateSettings in 'uApiUpdateSettings.pas',
  uApiServerXMLReader in 'uApiServerXMLReader.pas',
  uApiServerInterface in 'uApiServerInterface.pas',
  uApiServerClasses in 'uApiServerClasses.pas',
  uHTTPLogger in 'uHTTPLogger.pas' {HTTPLogger};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := '';
{$IFDEF DEBUG}
  UseLatestCommonDialogs := False;
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.CreateForm(TfMain, fMain);
{$IFDEF DEBUG}
  Application.CreateForm(THTTPLogger, HTTPLogger);
{$ENDIF}
  Application.Run;
end.
