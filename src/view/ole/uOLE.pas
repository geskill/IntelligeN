unit uOLE;
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  // Delphi
  SysUtils, ComServ, ComObj, ActiveX, StdVcl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Ole
  IntelligeN_TLB;

type
  TIntelligeN2009 = class(TAutoObject, IIntelligeN2009)
  protected
    procedure openfile(const AFileName: WideString); safecall;
    procedure savefile(const AFileName: WideString); safecall;
    procedure close; safecall;
    procedure callcrawler; safecall;
    procedure callremoteupload; safecall;
    procedure callcheckdirectlinks; safecall;
    procedure callcrypter; safecall;
    procedure callpublish; safecall;
    function crawleractive: SYSINT; stdcall;
    function hostermanageractive: SYSINT; stdcall;
    function publishactive: SYSINT; stdcall;
  end;

implementation

uses
  uMain;

procedure TIntelligeN2009.openfile(const AFileName: WideString);
begin
  Main.fMain.OpenToNewTab(AFileName);
end;

procedure TIntelligeN2009.savefile(const AFileName: WideString);
begin
  if SameText(AFileName, '') then
    Main.fMain.SaveCurrentTab
  else
    Main.fMain.ActiveTabSheetController.Save(AFileName, 'intelligen.xml.2');
end;

procedure TIntelligeN2009.close;
begin
  Main.fMain.RemoveCurrentTab;
end;

procedure TIntelligeN2009.callcrawler;
begin
  if (Main.fMain.TabSheetCount > 0) then
    Main.fMain.CallAutoCompletion;
end;

procedure TIntelligeN2009.callremoteupload;
var
  Picture: IPicture;
begin
  if (Main.fMain.TabSheetCount > 0) then
    Picture := Main.fMain.ActiveTabSheetController.ControlController.FindControl(cPicture) as IPicture;
  if Assigned(Picture) then
    Picture.RemoteUpload();
end;

procedure TIntelligeN2009.callcheckdirectlinks;
var
  I, J: Integer;
begin
  if (Main.fMain.TabSheetCount > 0) then
    with Main.fMain.ActiveTabSheetController.MirrorController do
      for I := 0 to MirrorCount - 1 do
        for J := 0 to Mirror[I].DirectlinkCount - 1 do
          Mirror[I].Directlink[J].CheckStatus;
end;

procedure TIntelligeN2009.callcrypter;
var
  I, J: Integer;
begin
  if (Main.fMain.TabSheetCount > 0) then
    with Main.fMain.ActiveTabSheetController.MirrorController do
      for I := 0 to MirrorCount - 1 do
        for J := 0 to Mirror[I].CrypterCount - 1 do
          Mirror[I].Crypter[J].CreateFolder;
end;

procedure TIntelligeN2009.callpublish;
begin
  if (Main.fMain.TabSheetCount > 0) then
    Main.fMain.callpublish;
end;

function TIntelligeN2009.crawleractive: SYSINT;
begin
  if Main.fMain.CrawlerManager.IsIdle then
    Result := 0
  else
    Result := 1;
end;

function TIntelligeN2009.hostermanageractive: SYSINT;
begin
  if Main.fMain.FileHosterManager.IsIdle then
    Result := 0
  else
    Result := 1;
end;

function TIntelligeN2009.publishactive: SYSINT;
begin
  if Main.fMain.PublishManager.IsIdle then
    Result := 0
  else
    Result := 1;
end;

initialization

TAutoObjectFactory.Create(ComServer, TIntelligeN2009, Class_IntelligeN2009, ciMultiInstance, tmApartment);

end.
