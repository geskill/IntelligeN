program UpdateManager;

uses
  Forms,
  Dialogs,
  uMain in 'uMain.pas' {fMain},
  IntelligeNFileSystem in 'IntelligeNFileSystem.pas',
  uApiUpdateModel in 'uApiUpdateModel.pas',
  uApiUpdateInterface in 'uApiUpdateInterface.pas',
  uApiUpdateConst in 'uApiUpdateConst.pas',
  uApiUpdateController in 'uApiUpdateController.pas',
  uApiUpdateSettings in 'uApiUpdateSettings.pas',
  uApiServerXMLReader in 'uApiServerXMLReader.pas',
  uApiServerInterface in 'uApiServerInterface.pas',
  uApiServerClasses in 'uApiServerClasses.pas';

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
  Application.Run;
end.
