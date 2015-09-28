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
    bCloseIntelligeN: TButton;
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
    procedure bCloseIntelligeNClick(Sender: TObject);
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

procedure TMain.bCloseIntelligeNClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.close;

  IntelligeNInstance := nil;
end;

procedure TMain.bLoadFromFileClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.openfile(eFileName.Text);
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
    if (IntelligeNInstance.crawleractive = 0) then
      eStatusCrawler.Text := 'Inactive'
    else
      eStatusCrawler.Text := 'Active';

    if (IntelligeNInstance.hostermanageractive = 0) then
      eStatusHoster.Text := 'Inactive'
    else
      eStatusHoster.Text := 'Active';

    if (IntelligeNInstance.publishactive = 0) then
      eStatusPublish.Text := 'Inactive'
    else
      eStatusPublish.Text := 'Active';
  end;
end;

procedure TMain.bStartCrawlerClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.callcrawler;
end;

procedure TMain.bStartImageUploadClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.callremoteupload;
end;

procedure TMain.bDirectlinksHosterCheckClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.callcheckdirectlinks;
end;

procedure TMain.bStartCrypterClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.callcrypter;
end;

procedure TMain.bStartPublishClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.callpublish;
end;

procedure TMain.bSaveClick(Sender: TObject);
begin
  if Assigned(IntelligeNInstance) then
    IntelligeNInstance.savefile('');
end;

end.
