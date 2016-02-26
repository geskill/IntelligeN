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
    function NewTabSheet(const ATemplateName: WideString): SYSINT; stdcall;
    function OpenTabSheet(const AFileName: WideString = ''): SYSINT; stdcall;
    function SaveTabSheet(ATabIndex: SYSINT; const AFileName: WideString = ''; const AFileFormat: WideString = ''; AForceDialog: WordBool = False): WordBool; stdcall;
    function CanCloseTabSheet(ATabIndex: SYSINT): WordBool; stdcall;
    function CloseTabSheet(ATabIndex: SYSINT): WordBool; stdcall;
    procedure CallPublish(ATabIndex: SYSINT); safecall;
    function IsPublishActive: WordBool; stdcall;
    procedure CallCrawler(ATabIndex: SYSINT); safecall;
    function IsCrawlerActive: WordBool; stdcall;
    procedure CallCrypterCrypt(ATabIndex: SYSINT); safecall;
    procedure CallCrypterCheck(ATabIndex: SYSINT); safecall;
    function IsCrypterActive: WordBool; stdcall;
    procedure CallFileHosterCheck(ATabIndex: SYSINT); safecall;
    function IsFileHosterActive: WordBool; stdcall;
    procedure CallImageHosterRemoteUpload(ATabIndex: SYSINT); safecall;
    function IsImageHosterActive: WordBool; stdcall;
    function ActiveTabSheetIndex: SYSINT; stdcall;
    function TabSheetCount: SYSINT; stdcall;
  end;

implementation

uses
  uMain;

{ TIntelligeN2009 }

function TIntelligeN2009.NewTabSheet(const ATemplateName: WideString): SYSINT;
begin
  Result := Main.fMain.NewTabSheet(ATemplateName);
end;

function TIntelligeN2009.OpenTabSheet(const AFileName: WideString = ''): SYSINT;
begin
  Result := Main.fMain.OpenTabSheet(AFileName);
end;

function TIntelligeN2009.SaveTabSheet(ATabIndex: SYSINT; const AFileName: WideString = ''; const AFileFormat: WideString = ''; AForceDialog: WordBool = False): WordBool;
begin
  Result := Main.fMain.SaveTabSheet(ATabIndex, AFileName, AFileFormat, AForceDialog);
end;

function TIntelligeN2009.CanCloseTabSheet(ATabIndex: SYSINT): WordBool;
begin
  Result := Main.fMain.CanCloseTabSheet(ATabIndex);
end;

function TIntelligeN2009.CloseTabSheet(ATabIndex: SYSINT): WordBool;
begin
  Result := Main.fMain.CloseTabSheet(ATabIndex);
end;

procedure TIntelligeN2009.CallPublish(ATabIndex: SYSINT);
begin
  Main.fMain.CallPublish(ATabIndex);
end;

function TIntelligeN2009.IsPublishActive: WordBool;
begin
  Result := not Main.fMain.PublishManager.IsIdle;
end;

procedure TIntelligeN2009.CallCrawler(ATabIndex: SYSINT);
begin
  Main.fMain.CallCrawler(ATabIndex);
end;

function TIntelligeN2009.IsCrawlerActive: WordBool;
begin
  Result := not Main.fMain.CrawlerManager.IsIdle;
end;

procedure TIntelligeN2009.CallCrypterCrypt(ATabIndex: SYSINT);
begin
  Main.fMain.CallCrypterCrypt(ATabIndex);
end;

procedure TIntelligeN2009.CallCrypterCheck(ATabIndex: SYSINT);
begin
  Main.fMain.CallCrypterCheck(ATabIndex);
end;

function TIntelligeN2009.IsCrypterActive: WordBool;
begin
  Result := not Main.fMain.CrypterManager.IsIdle;
end;

procedure TIntelligeN2009.CallFileHosterCheck(ATabIndex: SYSINT);
begin
  Main.fMain.CallFileHosterCheck(ATabIndex);
end;

function TIntelligeN2009.IsFileHosterActive: WordBool;
begin
  Result := not Main.fMain.FileHosterManager.IsIdle;
end;

procedure TIntelligeN2009.CallImageHosterRemoteUpload(ATabIndex: SYSINT);
begin
  Main.fMain.CallImageHosterRemoteUpload(ATabIndex);
end;

function TIntelligeN2009.IsImageHosterActive: WordBool;
begin
  Result := not Main.fMain.ImageHosterManager.IsIdle;
end;

function TIntelligeN2009.ActiveTabSheetIndex: SYSINT;
begin
  Result := Main.fMain.ActiveTabSheetIndex;
end;

function TIntelligeN2009.TabSheetCount: SYSINT;
begin
  Result := Main.fMain.TabSheetCount;
end;

initialization

TAutoObjectFactory.Create(ComServer, TIntelligeN2009, CLASS_IntelligeN2009, ciMultiInstance, tmApartment);

end.
