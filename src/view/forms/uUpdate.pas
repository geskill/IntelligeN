unit uUpdate;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, StdCtrls,
  cxProgressBar, cxLabel, cxTextEdit, cxMemo, cxRichEdit,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiSettings, uApiUpdate;

type
  TUpdate = class(TForm)
    cxLMessage: TcxLabel;
    cxPBUpdateStatus: TcxProgressBar;
    bRestartProgram: TButton;
    bClose: TButton;
    cxREUpdateInfo: TcxRichEdit;
    cxLShowInfo: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cxLShowInfoClick(Sender: TObject);
    procedure bRestartProgramClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    FUpdateController: TUpdateController;
  public
    property UpdateController: TUpdateController read FUpdateController;
    procedure update_position(APosition: Integer);
    procedure update_searching;
    procedure update_available(AUpdateInformation: string = '');
    procedure update_unnecessary;
    procedure update_finished;
    procedure update_error(AErrorMsg: string = 'unknown error');
  end;

var
  Update: TUpdate;

implementation

uses
  uMain;
{$R *.dfm}

procedure TUpdate.FormCreate(Sender: TObject);
begin
  // START CRACK DETECTION
  if not(Main.V = 0) then
    Exit;
  // END CRACK DETECTION
  FUpdateController := TUpdateController.Create(True);

  with SettingsManager.Settings do
  begin
    if CheckForUpdates then
      UpdateController.Start;
  end;

  // START CRACK DETECTION
  if not(Main.V = 0) then
    Halt;
  // END CRACK DETECTION
end;

procedure TUpdate.FormDestroy(Sender: TObject);
begin
  FUpdateController.Terminate;
  FUpdateController.Free;
end;

procedure TUpdate.cxLShowInfoClick(Sender: TObject);
begin
  Height := 400;
  cxREUpdateInfo.Height := 228;
end;

procedure TUpdate.bRestartProgramClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(GetHiddenDataDir + 'update\exec_update.bat'), nil, PChar(GetHiddenDataDir + 'update'), SW_SHOW);
  Application.Terminate;
end;

procedure TUpdate.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdate.update_position(APosition: Integer);
begin
  cxPBUpdateStatus.Position := APosition - 1;
end;

procedure TUpdate.update_searching;
begin
  cxLMessage.Caption := 'searching ...';
  cxPBUpdateStatus.Visible := False;
end;

procedure TUpdate.update_available(AUpdateInformation: string = '');
begin
  cxLMessage.Caption := 'an update is available downloading ...';
  cxPBUpdateStatus.Position := 0;
  cxREUpdateInfo.Lines.Text := AUpdateInformation;
  cxPBUpdateStatus.Visible := True;
  cxLShowInfo.Visible := True;
  bRestartProgram.Visible := False;
end;

procedure TUpdate.update_unnecessary;
begin
  cxLMessage.Caption := 'no update is available';
  cxPBUpdateStatus.Visible := False;
  cxLShowInfo.Visible := False;
  bRestartProgram.Visible := False;
end;

procedure TUpdate.update_finished;
begin
  cxLMessage.Caption := 'update finished';
  cxPBUpdateStatus.Position := 100;
  bRestartProgram.Visible := True;
end;

procedure TUpdate.update_error(AErrorMsg: string);
begin
  if (AErrorMsg = '') then
    AErrorMsg := 'unknown error (maybe timeout, try again later)';
  cxLMessage.Caption := 'error: ' + AErrorMsg;
end;

end.
