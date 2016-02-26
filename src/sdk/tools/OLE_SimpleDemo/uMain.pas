unit uMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  // Delphi - com object
  ComObj,
  // IntelligeN 2009
  IntelligeN_TLB;

type
  TMain = class(TForm)
    bStartIntelligeN: TButton;
    bCloseTab: TButton;
    bLoadFromFile: TButton;
    eFileName: TEdit;
    lFileName: TLabel;
    spSearchFile: TSpeedButton;
    OpenDialog: TOpenDialog;
    bUpdateStatus: TButton;
    lCrawlerActive: TLabel;
    eStatusCrawler: TEdit;
    lHosterStatus: TLabel;
    lPublishStatus: TLabel;
    eStatusHoster: TEdit;
    eStatusPublish: TEdit;
    bStartCrawler: TButton;
    bStartImageUpload: TButton;
    bDirectlinksHosterCheck: TButton;
    bStartCrypter: TButton;
    bStartPublish: TButton;
    bSave: TButton;   procedure bStartIntelligeNClick(Sender: TObject);
    procedure bCloseTabClick(Sender: TObject);
    procedure bLoadFromFileClick(Sender: TObject);
    procedure spSearchFileClick(Sender: TObject);
    procedure bUpdateStatusClick(Sender: TObject);
    procedure bStartCrawlerClick(Sender: TObject);
    procedure bStartImageUploadClick(Sender: TObject);
    procedure bDirectlinksHosterCheckClick(Sender: TObject);
    procedure bStartCrypterClick(Sender: TObject);
    procedure bStartPublishClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    IntelligeNInstance: IIntelligeN2009;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.bStartIntelligeNClick(Sender: TObject);
begin
  IntelligeNInstance := CreateOleObject('IntelligeN.IntelligeN2009') as IIntelligeN2009;
end;

procedure TMain.bCloseTabClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    if (IntelligeNInstance.CanCloseTabSheet(IntelligeNInstance.ActiveTabSheetIndex)) then
      IntelligeNInstance.CloseTabSheet(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bLoadFromFileClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.OpenTabSheet(eFileName.Text);
end;

procedure TMain.spSearchFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    eFileName.Text := OpenDialog.FileName;
end;

procedure TMain.bUpdateStatusClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
  begin
    if (not IntelligeNInstance.IsCrawlerActive) then
      eStatusCrawler.Text := 'Inactive'
    else
      eStatusCrawler.Text := 'Active';

    if (not IntelligeNInstance.IsFileHosterActive) then
      eStatusHoster.Text := 'Inactive'
    else
      eStatusHoster.Text := 'Active';

    if (not IntelligeNInstance.IsPublishActive) then
      eStatusPublish.Text := 'Inactive'
    else
      eStatusPublish.Text := 'Active';
  end;
end;

procedure TMain.bStartCrawlerClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.CallCrawler(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bStartImageUploadClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.CallImageHosterRemoteUpload(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bDirectlinksHosterCheckClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.CallFileHosterCheck(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bStartCrypterClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.CallCrypterCrypt(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bStartPublishClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.CallPublish(IntelligeNInstance.ActiveTabSheetIndex);
end;

procedure TMain.bSaveClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.SaveTabSheet(IntelligeNInstance.ActiveTabSheetIndex, '', '', False);
end;

end.
