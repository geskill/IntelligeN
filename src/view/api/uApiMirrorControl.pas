unit uApiMirrorControl;

interface

uses
  // Delphi
  Windows, SysUtils, Messages, Classes, Generics.Collections, Controls, Graphics, ExtCtrls, StdCtrls, Menus,
  ShellAPI, Clipbrd, Math, Dialogs, Forms,
  // Dev Express
  cxPC, cxEdit, cxTextEdit, cxGraphics, cxGrid, cxGridLevel, cxGridCustomTableView, cxGridTableView, cxLabel,
  cxButtons, dxBar, cxHint,
  // OmniThreadLibrary
  OtlSync,
  // EZTexturePanel
  EZTexturePanel,
  // RegExp
  RegExpr,
  // PopupMenu Mod
  uMydxBarPopupMenu, uMycxRichEdit, // uMyPopupMenu,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiCrypter, uApiMody, uApiSettings, uApiXml,
  // Plugin
  uPlugInConst,
  // Utils
  uApiDLMF, uPathUtils, uStringUtils;

type
  TEZTexturePanel = class(EZTexturePanel.TEZTexturePanel)
  protected
    fOnDropFiles: TWndMethod;
    procedure WMDROPFILES(var Msg: TMessage); message WM_DROPFILES;
  public
    property OnDropFiles: TWndMethod read fOnDropFiles write fOnDropFiles;
  end;

  TMycxTabSheet = class(TcxTabSheet, IDirectlinksMirror)
  private
    FDirectlinksPanel: IDirectlinksPanel;
    FPopupMenu: TMydxBarPopupMenu;
    FmiLoadFromFile: TdxBarButton;
    FmiSe1: TdxBarSeparator;
    FmiMody: TdxBarSubItem;
    FmiModyDo: TdxBarButton;
    FmiSe2: TdxBarSeparator;
    FmiSort: TdxBarButton;
    FmiDouble: TdxBarButton;
    FmiMissing: TdxBarButton;
    FmiRefreshSize: TdxBarButton;
    FMycxRichEdit: TMycxRichEdit;
    FTransparentPanel: TEZTexturePanel;
    FcxGridLinksInfo: TcxGrid;
    FcxGridLinksInfoLevel: TcxGridLevel;
    FcxGridLinksInfoTableView: TcxGridTableView;
    FcxGridLinksInfoTableViewColumn1: TcxGridColumn;
    FcxGridLinksInfoTableViewColumn2: TcxGridColumn;
    FStatusImage, FHosterImage: TImage;
    FPartsLabel, FSizeLabel: TLabel;
    FModyHintStyleController: TcxHintStyleController;
    FModyHintTimer: TTimer;
    FLinksChecked: Boolean;
    FLinksInfoLock: TOmniMREW;
    FLinksInfo: TLinksInfo;
    procedure FmiUndoClick(Sender: TObject);
    procedure FmiCutClick(Sender: TObject);
    procedure FmiCopyClick(Sender: TObject);
    procedure FmiPasteClick(Sender: TObject);
    procedure FmiDeleteClick(Sender: TObject);
    procedure FmiSelectAllClick(Sender: TObject);
    procedure FmiLoadFromFileClick(Sender: TObject);
    procedure FmiModyDoClick(Sender: TObject);
    procedure FmiSortClick(Sender: TObject);
    procedure FmiDoubleClick(Sender: TObject);
    procedure FmiMissingClick(Sender: TObject);
    procedure FmiRefreshSizeClick(Sender: TObject);
    procedure FPopupMenuPopup(Sender: TObject);
    procedure FMycxRichEditChange(Sender: TObject);
    procedure FmDirectlinksDropFiles(var message: TMessage);
    procedure FMycxRichEditEnter(Sender: TObject);
    procedure FMycxRichEditExit(Sender: TObject);
    procedure FMycxRichEditFocusChanged(Sender: TObject);
    procedure FMycxRichEditMouseLeave(Sender: TObject);
    procedure FTransparentPanelMouseEnter(Sender: TObject);
    procedure FModyHintTimerTimer(Sender: TObject);
    function GetStatus: Byte;
    function GetDirectlinksPanel: IDirectlinksPanel;
    procedure SetDirectlinksPanel(ADirectlinksPanel: IDirectlinksPanel);
    function GetSize: Extended;
    procedure SetSize(ASize: Extended);
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;
    function GetLinksInfo: TLinksInfo;
    procedure SetLinksInfo(ALinksInfo: TLinksInfo);
    function GetTitle: WideString;
    procedure SetTitle(ATitle: WideString);
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);
  public
    constructor Create(AOwner: TComponent);
    property DirectlinksPanel: IDirectlinksPanel read GetDirectlinksPanel write SetDirectlinksPanel;
    procedure Mody;
    procedure RefreshInfo;
    procedure CheckStatus;
    function GetFileName: WideString;
    function GetPartName(AFileName: WideString): WideString;
    procedure PostCreate;
    procedure PreDestroy;

    property Status: Byte read GetStatus;
    property Size: Extended read GetSize write SetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property Parts: Integer read GetParts;

    property LinksInfo: TLinksInfo read GetLinksInfo write SetLinksInfo;

    property Title: WideString read GetTitle write SetTitle;
    property Value: WideString read GetValue write SetValue;
    property Focus: Boolean read GetFocus write SetFocus;

    destructor Destroy; override;
  end;

  TDirectlinksPanel = class(TInterfacedObject, IDirectlinksPanel)
  private
    FMirrorControl: IMirrorControl;
    FPopupMenu: TPopupMenu;
    FmiAddTab: TMenuItem;
    FmiRemoveTab: TMenuItem;
    FcxPageControl: TcxPageControl;
    FcxLFirstSubMirrorInfo: TcxLabel;
    procedure FmiAddTabClick(Sender: TObject);
    procedure FmiRemoveTabClick(Sender: TObject);
    procedure FPopupMenuPopup(Sender: TObject);
    procedure FcxPageControlDblClick(Sender: TObject);
    procedure FcxPageControlPageChanging(Sender: TObject; NewPage: TcxTabSheet; var AllowChange: Boolean);
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);
    function GetFileName: WideString;
    function GetActiveMirrorIndex: Integer;
    function GetActiveMirror: IDirectlinksMirror;
    function GetMirror(index: Integer): IDirectlinksMirror;
    function GetMirrorCount: Integer;
    function GetSize: Extended; overload;
    function GetSize(index: Integer): Extended; overload;
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);
  public
    constructor Create(AOwner: TComponent; AMirrorControl: IMirrorControl);
    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;
    function Add(ALinks: WideString = ''): Integer;
    procedure Remove(ATabIndex: Integer);
    property ActiveMirrorIndex: Integer read GetActiveMirrorIndex;
    property ActiveMirror: IDirectlinksMirror read GetActiveMirror;
    property Mirror[index: Integer]: IDirectlinksMirror read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
    property Visible: Boolean read GetVisible write SetVisible;
    destructor Destroy; override;
  end;

  TCrypterPanel = class(TInterfacedObject, ICrypterPanel)
  private
    FMirrorControl: IMirrorControl;
    FPanel: TPanel;
    FcxTextEditLink: TcxTextEdit;
    FcxButtonLinkCheck: TcxButton;
    FcxGridFolderInfo: TcxGrid;
    FcxGridFolderInfoLevel: TcxGridLevel;
    FcxGridFolderInfoTableView: TcxGridTableView;
    FcxGridFolderInfoTableViewColumn1: TcxGridColumn;
    FcxGridFolderInfoTableViewColumn2: TcxGridColumn;
    FCrypter: TCrypterCollectionItem;
    FCrypterFolderInfoLock: TOmniMREW;
    FCrypterFolderInfo: TCrypterFolderInfo;
    procedure FPanelContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure FcxGridFolderInfoTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure FcxTextEditLinkChange(Sender: TObject);
    procedure FcxButtonLinkCheckClick(Sender: TObject);
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);
    function GetName: WideString;
    function GetLink: WideString;
    procedure SetLink(ALink: WideString);
    function GetStatus: Byte;
    function GetSize: Extended;
    function GetPartSize: Extended;
    function GetHoster: WideString;
    function GetHosterShort: WideString;
    function GetParts: Integer;
    function GetStatusImage: WideString;
    function GetStatusImageText: WideString;
    procedure SetCrypter(ACrypter: TCrypterCollectionItem);
    function GetCrypterFolderInfo: TCrypterFolderInfo;
    procedure SetCrypterFolderInfo(ACrypterFolderInfo: TCrypterFolderInfo);
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);
  public
    constructor Create(AOwner: TComponent; AMirrorControl: IMirrorControl);

    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;
    property name: WideString read GetName;
    property Link: WideString read GetLink write SetLink;
    property Status: Byte read GetStatus;
    property Size: Extended read GetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
    property Crypter: TCrypterCollectionItem read FCrypter write SetCrypter;
    property CrypterFolderInfo: TCrypterFolderInfo read GetCrypterFolderInfo write SetCrypterFolderInfo;
    procedure RefreshGrid;
    procedure CreateFolder;
    procedure CheckFolder(const AUseCheckDelay: Boolean = False);
    property Visible: Boolean read GetVisible write SetVisible;
    destructor Destroy; override;
  end;

  TMirrorControl = class(TInterfacedObject, IMirrorControl)
  private
    FPopupMenu: TPopupMenu;
    FmiAddMirror: TMenuItem;
    FmiMirrorIndex: TMenuItem;
    FmiRemoveMirror: TMenuItem;
    FmiMirrorSettings: TMenuItem;
    FmiMirrorColumns: TMenuItem;
    FmiMirrorColumns1: TMenuItem;
    FmiMirrorColumns2: TMenuItem;
    FmiMirrorColumns3: TMenuItem;
    FmiMirrorPosition: TMenuItem;
    FmiMirrorPosition_Top: TMenuItem;
    FmiMirrorPosition_Buttom: TMenuItem;
    FCrypterPopupMenu: TPopupMenu;
    FcxTabControl: TcxTabControl;
    FMirrorController: IMirrorController;
    FDirectlinksPanel: IDirectlinksPanel;
    FCrypterPanelList: TInterfaceList;
    FcxButtonCrypt: TcxButton;
    procedure cxTabControlChange(Sender: TObject);
    procedure cxTabControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FPopupMenuPopup(Sender: TObject);
    procedure FmiAddMirrorClick(Sender: TObject);
    procedure FmiMirrorIndexClick(Sender: TObject);
    procedure FmiRemoveMirrorClick(Sender: TObject);
    procedure FmiMirrorColumnsClick(Sender: TObject);
    procedure FmiMirrorPositionClick(Sender: TObject);
    procedure FmiCrypterClick(Sender: TObject);
    procedure FmiAllCrypterClick(Sender: TObject);
    function GetTabControlTabWidth: Integer;
    function GetMirrorController: IMirrorController;
    procedure SetMirrorController(const AMirrorController: IMirrorController);
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetTabIndex: Integer;
    procedure SetTabIndex(ATabIndex: Integer);
    function GetSize: Extended;
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;
    function GetDirectlink: IDirectlinksPanel;
    function GetDirectlinksMirror(index: Integer): WideString;
    procedure SetDirectlinksMirror(index: Integer; ADirectlinks: WideString);
    function GetDirectlinksMirrorCount: Integer;
    function GetCrypter(index: Integer): ICrypterPanel;
    function GetCrypterCount: Integer;
    function AddCrypter(AName: WideString): Integer;
    function RemoveCrypter(AIndex: Integer): Boolean;
    function GetLeft: Integer;
    procedure SetLeft(ALeft: Integer);
    function GetTop: Integer;
    procedure SetTop(ATop: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AWidth: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AHeight: Integer);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);
  public
    constructor Create(AOwner: TComponent; ALeft: Integer = 0; ATop: Integer = 0);
    property MirrorController: IMirrorController read GetMirrorController write SetMirrorController;
    property index: Integer read GetIndex write SetIndex;
    property TabIndex: Integer read GetTabIndex write SetTabIndex;
    property Directlink: IDirectlinksPanel read GetDirectlink;
    property DirectlinksMirror[index: Integer]: WideString read GetDirectlinksMirror;
    property DirectlinksMirrorCount: Integer read GetDirectlinksMirrorCount;
    property Crypter[index: Integer]: ICrypterPanel read GetCrypter;
    property CrypterCount: Integer read GetCrypterCount;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    destructor Destroy; override;
  end;

implementation

uses
  uMain, uSettings;

resourcestring
  StrRemovedLinks = 'Removed links:';
  StrMissingParts = 'Missing parts:';
  StrLoadFromFile = 'Load from file';
  StrCheckLinks = 'Check links';
  StrCheck = 'Check';
  StrSize = 'Size';
  StrParts = 'Parts';
  StrOffline = 'offline';
  StrOnline = 'online';
  StrUnknown = 'unknown';
  StrNotyet = 'notyet';
  StrMixed = 'mixed';
  StrNoinfo = 'noinfo';
  StrTop = 'Top';
  StrButtom = 'Buttom';

procedure FilterContainerFile(FileName: TFilename; cxRichEdit: TMycxRichEdit);
begin
  if ((ExtractFileExt(FileName) = '.ccf') or (ExtractFileExt(FileName) = '.ncf') or (ExtractFileExt(FileName) = '.rsdf') or (ExtractFileExt(FileName) = '.dlc')) then
    cxRichEdit.Lines.Text := TDLMF.ContainerFileToPlainEx(FileName, '')
  else
    with TStringList.Create do
      try
        LoadFromFile(FileName);
        cxRichEdit.Lines.Text := UnicodeString(Text);
      finally
        Free;
      end;
end;

procedure TEZTexturePanel.WMDROPFILES(var Msg: TMessage);
begin
  if Assigned(fOnDropFiles) then
    fOnDropFiles(Msg)
  else
    inherited;
end;

{ ****************************************************************************** }
{$REGION 'TMycxTabSheet'}

procedure TMycxTabSheet.FmiUndoClick(Sender: TObject);
begin
  FMycxRichEdit.Undo;
end;

procedure TMycxTabSheet.FmiCutClick(Sender: TObject);
begin
  FMycxRichEdit.CutToClipboard;
end;

procedure TMycxTabSheet.FmiCopyClick(Sender: TObject);
begin
  FMycxRichEdit.CopyToClipboard;
end;

procedure TMycxTabSheet.FmiPasteClick(Sender: TObject);
begin
  FMycxRichEdit.PasteFromClipboard;
end;

procedure TMycxTabSheet.FmiDeleteClick(Sender: TObject);
begin
  FMycxRichEdit.ClearSelection;
end;

procedure TMycxTabSheet.FmiSelectAllClick(Sender: TObject);
begin
  FMycxRichEdit.SelectAll;
end;

procedure TMycxTabSheet.FmiLoadFromFileClick(Sender: TObject);
const
  CONTAINER_FILES = '*.ccf;*.ncf;*.rsdf;*.dlc';
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := StrTextFiles + ' (*.txt)|*.txt' + '|' + StrContainerFiles + ' (' + CONTAINER_FILES + ')|' + CONTAINER_FILES + '|' + StrAllFiles + ' (*.*)|*.*';
      if Execute then
        FilterContainerFile(FileName, FMycxRichEdit);
    finally
      Free;
    end;
end;

procedure TMycxTabSheet.FmiModyDoClick(Sender: TObject);
begin
  Mody;
end;

procedure TMycxTabSheet.FmiSortClick(Sender: TObject);
begin
  if Sender is TdxBarButton then
    SettingsManager.Settings.Mody.Sort := (Sender as TdxBarButton).Down;
end;

procedure TMycxTabSheet.FmiDoubleClick(Sender: TObject);
begin
  if Sender is TdxBarButton then
    SettingsManager.Settings.Mody.RemoveDouble := (Sender as TdxBarButton).Down;
end;

procedure TMycxTabSheet.FmiMissingClick(Sender: TObject);
begin
  if Sender is TdxBarButton then
    SettingsManager.Settings.Mody.FindMissing := (Sender as TdxBarButton).Down;
end;

procedure TMycxTabSheet.FmiRefreshSizeClick(Sender: TObject);
begin
  CheckStatus;
end;

procedure TMycxTabSheet.FPopupMenuPopup(Sender: TObject);
begin
  with FPopupMenu do
  begin
    FmiUndo.Enabled := FMycxRichEdit.CanUndo;
    FmiCut.Enabled := (FMycxRichEdit.SelLength > 0);
    FmiCopy.Enabled := (FMycxRichEdit.SelLength > 0);
    FmiPaste.Enabled := not(Clipboard.AsText = '');
    FmiDelete.Enabled := (FMycxRichEdit.SelLength > 0);
    FmiSelectAll.Enabled := True;
  end;

  with SettingsManager.Settings.Mody do
  begin
    FmiModyDo.Enabled := Sort or RemoveDouble or FindMissing;

    FmiSort.Down := Sort;
    FmiDouble.Down := RemoveDouble;
    FmiMissing.Down := FindMissing;
  end;
end;

procedure TMycxTabSheet.FMycxRichEditChange(Sender: TObject);
begin
  if SettingsManager.Settings.Mody.NotifyOnChange then
    Mody;
  if Assigned(FDirectlinksPanel.MirrorControl.MirrorController.OnChange) then
    FDirectlinksPanel.MirrorControl.MirrorController.OnChange.Invoke(Self);
end;

procedure TMycxTabSheet.FmDirectlinksDropFiles(var message: TMessage);
var
  FileCount, Size: Integer;
  FileName: PChar;
begin
  FileCount := DragQueryFile(message.WParam, $FFFFFFFF, nil, 255);

  if (FileCount = 1) then
  begin
    Size := DragQueryFile(message.WParam, 0, nil, 0) + 1;
    FileName := StrAlloc(Size);

    if DragQueryFile(message.WParam, 0, FileName, Size) = 1 then
      { nothing } ;

    if FileExists(FileName) then
      FilterContainerFile(FileName, FMycxRichEdit);

    StrDispose(FileName);
  end;

  DragFinish(message.WParam);
end;

procedure TMycxTabSheet.FMycxRichEditEnter(Sender: TObject);
begin
  if Assigned(FDirectlinksPanel.MirrorControl.MirrorController.OnPopupMenuChange) then
    FDirectlinksPanel.MirrorControl.MirrorController.OnPopupMenuChange.Invoke(Integer(FPopupMenu.ItemLinks));
end;

procedure TMycxTabSheet.FMycxRichEditExit(Sender: TObject);
begin
  if Assigned(FDirectlinksPanel.MirrorControl.MirrorController.OnPopupMenuChange) then
    FDirectlinksPanel.MirrorControl.MirrorController.OnPopupMenuChange.Invoke(Integer(nil));

  FTransparentPanel.Redraw;
  RefreshInfo;
  FTransparentPanel.Visible := True;
end;

procedure TMycxTabSheet.FMycxRichEditFocusChanged(Sender: TObject);
begin
  FTransparentPanel.Visible := False;
end;

procedure TMycxTabSheet.FMycxRichEditMouseLeave(Sender: TObject);
var
  m, p: TPoint;
  c: TWinControl;
  ControlClassName: string;
begin
  with FMycxRichEdit do
  begin
    m := Mouse.CursorPos;
    p := ScreenToClient(m);
    c := FindVCLWindow(m);
    if Assigned(c) then
      ControlClassName := c.ClassName;
    // OutputDebugString(PChar(ControlClassName + ' X: ' + IntToStr(p.X) + ' (' + IntToStr(Width) + ') Y: ' + IntToStr(p.Y) + ' (' + IntToStr(Height) + ')'));
    // minor bug: NativeStyle = True moving mouse fast 45° out buttom left
    if not Focused and not(SameStr('TcxControlScrollBar', ControlClassName) or (SameStr('TcxRichInnerEdit', ControlClassName) and not((p.Y >= Height) or (p.Y <= 0) or (p.X >= Width) or (p.X <= 0)))) then
    begin
      FTransparentPanel.Redraw;
      RefreshInfo;
      FTransparentPanel.Visible := True;
    end;
  end;
end;

procedure TMycxTabSheet.FTransparentPanelMouseEnter(Sender: TObject);
var
  p: TPoint;
begin
  if not SettingsManager.Settings.NativeStyle then
    with FMycxRichEdit do
    begin
      p := ScreenToClient(Mouse.CursorPos);
      // OutputDebugString(PChar(' X: ' + IntToStr(p.X) + ' (' + IntToStr(Width) + ') Y: ' + IntToStr(p.Y) + ' (' + IntToStr(Height) + ')'));
      if ((p.Y >= Height - 1) or (p.Y <= 0) or (p.X >= Width - 1) or (p.X <= 0)) then
        Exit;
    end;

  FTransparentPanel.Visible := False;
end;

procedure TMycxTabSheet.FModyHintTimerTimer(Sender: TObject);
begin
  FModyHintStyleController.HideHint;
  TTimer(Sender).Enabled := False;
end;

function TMycxTabSheet.GetStatus;
begin
  if not(FLinksChecked or SameStr(Hoster, '')) then
  begin
    FLinksChecked := True;
    CheckStatus;
  end;
  FLinksInfoLock.EnterReadLock;
  try
    Result := FLinksInfo.Status;
  finally
    FLinksInfoLock.ExitReadLock;
  end;
end;

function TMycxTabSheet.GetDirectlinksPanel;
begin
  Result := FDirectlinksPanel;
end;

procedure TMycxTabSheet.SetDirectlinksPanel(ADirectlinksPanel: IDirectlinksPanel);
begin
  FDirectlinksPanel := ADirectlinksPanel;
end;

function TMycxTabSheet.GetSize;
begin
  if not(FLinksChecked or SameStr(Hoster, '')) then
  begin
    FLinksChecked := True;
    CheckStatus;
  end;
  FLinksInfoLock.EnterReadLock;
  try
    Result := FLinksInfo.Size;
  finally
    FLinksInfoLock.ExitReadLock;
  end;
end;

procedure TMycxTabSheet.SetSize(ASize: Extended);
begin
  FLinksInfoLock.EnterWriteLock;
  try
    FLinksInfo.Size := ASize;
  finally
    FLinksInfoLock.ExitWriteLock;
  end;
end;

function TMycxTabSheet.GetPartSize;
begin
  // FLinksChecked or SameStr(Hoster, '') need to be false
  // then CheckStatus; can be processed
  if not(FLinksChecked or SameStr(Hoster, '')) then
  begin
    FLinksChecked := True;
    CheckStatus;
  end;
  FLinksInfoLock.EnterReadLock;
  try
    Result := FLinksInfo.PartSize;
  finally
    FLinksInfoLock.ExitReadLock;
  end;
end;

function TMycxTabSheet.GetHoster: WideString;
begin
  Result := GetHoster(False);
end;

function TMycxTabSheet.GetHoster(AShortName: Boolean): WideString;
var
  I: Integer;
begin
  Result := '';
  with FMycxRichEdit.Lines do
    for I := 0 to Min(Count - 1, 4) do
    begin
      Result := THosterConfiguration.GetCustomisedHoster(RemoveW(ExtractUrlHost(Strings[I])), AShortName);
      if not(Result = '') then
        Break;
    end;
end;

function TMycxTabSheet.GetParts;
var
  I: Integer;
begin
  Result := 0;
  with FMycxRichEdit.Lines do
    for I := 0 to Count - 1 do
      if not(Strings[I] = '') then
        Inc(Result);
end;

function TMycxTabSheet.GetLinksInfo;
begin
  // FLinksChecked or SameStr(Hoster, '') need to be false
  // then CheckStatus; can be processed
  if not(FLinksChecked or SameStr(Hoster, '')) then
  begin
    FLinksChecked := True;
    CheckStatus;
  end;
  FLinksInfoLock.EnterReadLock;
  try
    Result := FLinksInfo;
  finally
    FLinksInfoLock.ExitReadLock;
  end;
end;

procedure TMycxTabSheet.SetLinksInfo(ALinksInfo: TLinksInfo);
begin
  FLinksInfoLock.EnterWriteLock;
  try
    FLinksInfo := ALinksInfo;
  finally
    FLinksInfoLock.ExitWriteLock;
  end;
end;

function TMycxTabSheet.GetTitle;
begin
  Result := Caption;
end;

procedure TMycxTabSheet.SetTitle(ATitle: WideString);
begin
  Caption := ATitle;
end;

function TMycxTabSheet.GetValue;
begin
  Result := FMycxRichEdit.Lines.Text;
end;

procedure TMycxTabSheet.SetValue(AValue: WideString);
begin
  FMycxRichEdit.Lines.Text := AValue;
end;

function TMycxTabSheet.GetFocus;
begin
  Result := FMycxRichEdit.Focused;
end;

procedure TMycxTabSheet.SetFocus(AFocus: Boolean);
begin
  if AFocus and FMycxRichEdit.CanFocusEx then
    FMycxRichEdit.SetFocus;
end;

constructor TMycxTabSheet.Create;
begin
  inherited Create(AOwner);

  FDirectlinksPanel := nil;

  FPopupMenu := TMydxBarPopupMenu.Create(Self);
  with FPopupMenu do
  begin
{$REGION 'FPopupMenu.Items'}
    FmiUndo.OnClick := FmiUndoClick;
    FmiCut.OnClick := FmiCutClick;
    FmiCopy.OnClick := FmiCopyClick;
    FmiPaste.OnClick := FmiPasteClick;
    FmiDelete.OnClick := FmiDeleteClick;
    FmiSelectAll.OnClick := FmiSelectAllClick;

    FmiLoadFromFile := TdxBarButton.Create(FPopupMenu);
    with FmiLoadFromFile do
    begin
      Caption := StrLoadFromFile;
      ShortCut := Menus.ShortCut($4F, [ssCtrl]);
      ImageIndex := 1;
      OnClick := FmiLoadFromFileClick;
    end;
    with ItemLinks.Add do
    begin
      // index := FmiUndo.index;
      Item := FmiLoadFromFile;
    end;

    FmiSe1 := TdxBarSeparator.Create(FPopupMenu);
    with FmiSe1 do
    begin
      ShowCaption := False;
    end;
    with ItemLinks.Add do
    begin
      // index := FmiS1.index;
      Item := FmiSe1;
    end;

    FmiMody := TdxBarSubItem.Create(FPopupMenu);
    with FmiMody do
    begin
      Caption := 'Mody';

      FmiModyDo := TdxBarButton.Create(FPopupMenu);
      with FmiModyDo do
      begin
        Caption := StrAll;
        ShortCut := Menus.ShortCut($44, [ssCtrl]);
        OnClick := FmiModyDoClick;
      end;
      with ItemLinks.Add do
      begin
        // index := FmiModyDo.index;
        Item := FmiModyDo;
      end;

      FmiSe2 := TdxBarSeparator.Create(FPopupMenu);
      with FmiSe2 do
      begin
        ShowCaption := False;
      end;
      with ItemLinks.Add do
      begin
        // index := FmiSe2.index;
        Item := FmiSe2;
      end;

      FmiSort := TdxBarButton.Create(FPopupMenu);
      with FmiSort do
      begin
        ButtonStyle := bsChecked;
        Caption := StrSort;
        ImageIndex := 17;
        OnClick := FmiSortClick;
      end;
      with ItemLinks.Add do
      begin
        // index := FmiSort.index;
        Item := FmiSort;
      end;

      FmiDouble := TdxBarButton.Create(FPopupMenu);
      with FmiDouble do
      begin
        ButtonStyle := bsChecked;
        Caption := StrRemoveDouble;
        ImageIndex := 18;
        OnClick := FmiDoubleClick;
      end;
      with ItemLinks.Add do
      begin
        // index := FmiDouble.index;
        Item := FmiDouble;
      end;

      FmiMissing := TdxBarButton.Create(FPopupMenu);
      with FmiMissing do
      begin
        ButtonStyle := bsChecked;
        Caption := StrNotifyMissing;
        OnClick := FmiMissingClick;
      end;
      with ItemLinks.Add do
      begin
        // index := FmiMissing.index;
        Item := FmiMissing;
      end;
    end;
    with ItemLinks.Add do
    begin
      // index := FmiUndo.index;
      Item := FmiMody;
    end;

    FmiRefreshSize := TdxBarButton.Create(FPopupMenu);
    with FmiRefreshSize do
    begin
      Caption := StrCheckLinks;
      OnClick := FmiRefreshSizeClick;
    end;
    with ItemLinks.Add do
      Item := FmiRefreshSize;
{$ENDREGION}
    OnPopup := FPopupMenuPopup;
  end;

  with FLinksInfo do
  begin
    Status := 255;
    Size := 0;
    PartSize := 0;
  end;
  FLinksChecked := False;

  FMycxRichEdit := TMycxRichEdit.Create(Self);
  with FMycxRichEdit do
  begin
    Parent := TWinControl(Self);

    Align := alClient;

    PopupMenu := FPopupMenu;

    with Properties do
    begin
      MemoMode := True;

      ScrollBars := ssBoth;

      OnChange := FMycxRichEditChange;
    end;

    with Style.Font do
      name := 'Courier New';

    StyleFocused.BorderStyle := ebsThick;

    OnDropFiles := FmDirectlinksDropFiles;
    OnEnter := FMycxRichEditEnter;
    OnExit := FMycxRichEditExit;
    OnFocusChanged := FMycxRichEditFocusChanged;
    OnMouseLeave := FMycxRichEditMouseLeave;
  end;

  FTransparentPanel := TEZTexturePanel.Create(Self);
  with FTransparentPanel do
  begin
    Parent := TWinControl(Self);

    Align := alClient;
    BevelOuter := bvNone;

    with Settings do
    begin
      TextureType := ttBlend;
      AlphaBlend := 220;
      AlphaBlendColor := clWhite;
    end;

    OnDropFiles := FmDirectlinksDropFiles;
    OnMouseEnter := FTransparentPanelMouseEnter;
  end;

  FcxGridLinksInfo := TcxGrid.Create(FTransparentPanel);
  with FcxGridLinksInfo do
  begin
    Parent := TWinControl(FTransparentPanel);

    Anchors := [akLeft, akTop, akRight, akBottom];

    Height := FTransparentPanel.Height - 12;
    Left := 6;
    Top := 6;
    Width := FTransparentPanel.Width - 12;

    OnMouseEnter := FTransparentPanelMouseEnter;

    FcxGridLinksInfoLevel := Levels.Add;
    FcxGridLinksInfoTableView := CreateView(TcxGridTableView) as TcxGridTableView;
    FcxGridLinksInfoLevel.GridView := FcxGridLinksInfoTableView;

    with FcxGridLinksInfoTableView.OptionsView do
    begin
      ColumnAutoWidth := True;

      GroupByBox := False;
      ScrollBars := ssVertical;
    end;

    FcxGridLinksInfoTableViewColumn1 := FcxGridLinksInfoTableView.CreateColumn;
    FcxGridLinksInfoTableViewColumn1.Caption := 'Name';
    FcxGridLinksInfoTableViewColumn2 := FcxGridLinksInfoTableView.CreateColumn;
    with FcxGridLinksInfoTableViewColumn2 do
    begin
      Caption := 'Value';
      PropertiesClass := TcxTextEditProperties;
      Properties.readonly := True;
      // OnCustomDrawCell := FcxGridFolderInfoTableViewColumn2CustomDrawCell;
    end;

    with FcxGridLinksInfoTableView do
    begin
      Items[0].PropertiesClass := TcxLabelProperties;

      with DataController do
        try
          BeginUpdate;
          RecordCount := 4;
          Values[0, 0] := 'Status';
          Values[1, 0] := 'Hoster';
          Values[2, 0] := StrSize;
          Values[3, 0] := StrParts;
        finally
          EndUpdate;
        end;
    end;

    Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvGrid);
  end;

  FStatusImage := TImage.Create(FTransparentPanel);
  with FStatusImage do
  begin
    Parent := FTransparentPanel;

    Left := 1;
    Top := 1;

    Width := 16;
    Height := 16;

    Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  end;

  FHosterImage := TImage.Create(FTransparentPanel);
  with FHosterImage do
  begin
    Parent := FTransparentPanel;

    Left := 19;
    Top := 1;

    Width := 16;
    Height := 16;

    Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  end;

  FPartsLabel := TLabel.Create(FTransparentPanel);
  with FPartsLabel do
  begin
    Parent := FTransparentPanel;

    AutoSize := False;

    Left := 37;
    Top := 2;

    Width := 16;
    Height := 16;

    Font.Style := [fsBold];

    Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  end;

  FSizeLabel := TLabel.Create(FTransparentPanel);
  with FSizeLabel do
  begin
    Parent := FTransparentPanel;

    AutoSize := False;

    Left := 1;
    Top := 19;

    Width := 16 + 2 + 16 + 2 + 16;
    Height := 16;

    // Font.Style := [fsBold];

    Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  end;

  FModyHintStyleController := TcxHintStyleController.Create(Self);
  with FModyHintStyleController do
  begin
    Global := False;
    with HintStyle as TcxHintStyle do
    begin
      CaptionFont.Style := [fsBold];
      CallOutPosition := cxbpAuto;
      IconSize := cxisSmall;
      IconType := cxhiWarning;
    end;
  end;

  FModyHintTimer := TTimer.Create(Self);
  with FModyHintTimer do
  begin
    Enabled := False;
    Interval := 5000;
    OnTimer := FModyHintTimerTimer;
  end;
end;

procedure TMycxTabSheet.Mody;
var
  StringList: TStrings;
  ModyResult: TModyResult;
  p: TPoint;
begin
  ModyResult := TMody.Mody(Self);
  try
    StringList := TStringList.Create;
    try
      with SettingsManager.Settings.Mody do
        if RemoveDouble or FindMissing then
        begin
          StringList.Add(StrRemovedLinks);
          StringList.Add(ModyResult.RemovedDouble.Text);
          StringList.Add(StrMissingParts);
          StringList.Add(ModyResult.MissingParts.Text);

          p := FMycxRichEdit.ClientToScreen(FMycxRichEdit.ClientBounds.TopLeft);

          FModyHintStyleController.ShowHint(p.X, p.Y, 'Mody', StringList.Text, 0);
          FModyHintTimer.Enabled := True;

          StringList.Clear;
        end;
    finally
      StringList.Free;
    end;
  finally
    ModyResult.RemovedDouble.Free;
    ModyResult.MissingParts.Free;
  end;
end;

procedure TMycxTabSheet.RefreshInfo;
var
  _FieldIndex: Integer;
  _StringStatus, _Hoster: string;
  _LinksInfo: TLinksInfo;
  _PlugInCollectionItem: TPlugInCollectionItem;

  function FindRecordIndexByText(const aText: string): Integer;
  var
    RecordIndex: Integer;
  begin
    Result := -1;

    with FcxGridLinksInfoTableView.DataController do
    begin
      for RecordIndex := 0 to RecordCount - 1 do
        if (Values[RecordIndex, 0] = aText) then
        begin
          Result := RecordIndex;
          Break;
        end;
    end;
  end;

begin
  FcxGridLinksInfo.Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvGrid);

  FStatusImage.Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  FHosterImage.Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  FPartsLabel.Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);
  FSizeLabel.Visible := (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon);

  _Hoster := Hoster;
  _LinksInfo := LinksInfo;

  if (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvGrid) then
  begin
    _FieldIndex := FindRecordIndexByText('Status');
    if (_FieldIndex > -1) then
    begin
      case _LinksInfo.Status of
        0:
          _StringStatus := StrOffline;
        1:
          _StringStatus := StrOnline;
        2:
          _StringStatus := StrUnknown;
        3:
          _StringStatus := StrNotyet;
        4:
          _StringStatus := StrMixed;
        255:
          _StringStatus := StrNoinfo;
      end;

      FcxGridLinksInfoTableView.DataController.Values[_FieldIndex, 1] := _StringStatus;
    end;

    _FieldIndex := FindRecordIndexByText('Hoster');
    if (_FieldIndex > -1) then
    begin
      if SameStr(_Hoster, '') then
        FcxGridLinksInfoTableView.DataController.Values[_FieldIndex, 1] := 'n/a'
      else
        FcxGridLinksInfoTableView.DataController.Values[_FieldIndex, 1] := string(_Hoster) + ' (' + string(GetHoster(True)) + ')';
    end;

    _FieldIndex := FindRecordIndexByText(StrSize);
    if (_FieldIndex > -1) then
      FcxGridLinksInfoTableView.DataController.Values[_FieldIndex, 1] := FloatToStr(_LinksInfo.Size) + ' MB (' + FloatToStr(_LinksInfo.PartSize) + ' MB)';

    _FieldIndex := FindRecordIndexByText(StrParts);
    if (_FieldIndex > -1) then
      FcxGridLinksInfoTableView.DataController.Values[_FieldIndex, 1] := IntToStr(Parts);
  end
  else if (SettingsManager.Settings.ComponentParser.DirectlinksView = dlvIcon) then
  begin
    Main.ILContainerStatusImages.GetIcon(_LinksInfo.Status, FStatusImage.Picture.Icon);

    with SettingsManager.Settings.Plugins do
      _PlugInCollectionItem := TPlugInCollectionItem(FindPlugInCollectionItemFromCollection(_Hoster, FileHoster));

    if Assigned(_PlugInCollectionItem) then
      FHosterImage.Picture.Icon.Assign(_PlugInCollectionItem.Icon);

    FPartsLabel.Caption := IntToStr(Parts);
    FSizeLabel.Caption := FloatToStr(_LinksInfo.Size) + ' MB';
  end;
end;

procedure TMycxTabSheet.CheckStatus;
begin
  if not SameStr(Hoster, '') then
    FDirectlinksPanel.MirrorControl.MirrorController.TabSheetController.PageController.HosterManager.AddHosterCheckJob(Self);
end;

function TMycxTabSheet.GetFileName;
const
  FileExt: array [0 .. 1] of string = ('part', '7z');
var
  Link: string;
  I, J: Integer;
begin
  with TStringList.Create do
    try
      Text := Value;

      for I := 0 to Count - 1 do
      begin
        Link := ChangeFileExt(ChangeFileExt(GetPartName(Strings[I]), ''), '');

        for J := 0 to length(FileExt) - 1 do
          if not(Pos('.' + FileExt[J], Link) = 0) then
            Link := ChangeFileExt(Link, '');

        if not(Link = '') then
          Break;
      end;
    finally
      Free;
    end;

  Result := Link;
end;

function TMycxTabSheet.GetPartName(AFileName: WideString): WideString;
var
  _Index, _Count: Integer;
  _Found: Boolean;
begin
  Result := ExtractUrlFileName(AFileName);
  _Index := 0;
  _Found := False;
  FLinksInfoLock.EnterReadLock;
  try
    _Count := length(FLinksInfo.Links);

    while (_Index < _Count) and not _Found do
    begin
      _Found := (AFileName = FLinksInfo.Links[_Index].Link);

      if not _Found then
        Inc(_Index);
    end;

    if _Found then
      Result := FLinksInfo.Links[_Index].FileName;
  finally
    FLinksInfoLock.ExitReadLock;
  end;
end;

procedure TMycxTabSheet.PostCreate;
begin
  DragAcceptFiles(FMycxRichEdit.Handle, True);
  DragAcceptFiles(FTransparentPanel.Handle, True);
end;

procedure TMycxTabSheet.PreDestroy;
begin
  DragAcceptFiles(FTransparentPanel.Handle, False);
  DragAcceptFiles(FMycxRichEdit.Handle, False);
end;

destructor TMycxTabSheet.Destroy;
begin
  FModyHintTimer.Free;
  FModyHintStyleController.Free;

  FSizeLabel.Free;
  FPartsLabel.Free;
  FHosterImage.Free;
  FStatusImage.Free;

  FcxGridLinksInfoTableViewColumn2.Free;
  FcxGridLinksInfoTableViewColumn1.Free;
  FcxGridLinksInfoTableView.Free;
  FcxGridLinksInfoLevel.Free;
  FcxGridLinksInfo.Free;

  FTransparentPanel.Free;

  FMycxRichEdit.Free;

  FmiRefreshSize.Free;
  FmiMissing.Free;
  FmiDouble.Free;
  FmiSort.Free;
  FmiMody.Free;
  FmiSe1.Free;
  FmiLoadFromFile.Free;
  FPopupMenu.Free;

  inherited Destroy;

  FDirectlinksPanel := nil;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TDirectlinksPanel'}

procedure TDirectlinksPanel.FmiAddTabClick(Sender: TObject);
begin
  Add;
end;

procedure TDirectlinksPanel.FmiRemoveTabClick(Sender: TObject);
begin
  Remove(ActiveMirrorIndex);
end;

procedure TDirectlinksPanel.FPopupMenuPopup(Sender: TObject);
begin
  FmiRemoveTab.Enabled := (GetMirrorCount > 0);
end;

procedure TDirectlinksPanel.FcxPageControlDblClick(Sender: TObject);
begin
  if (GetMirrorCount = 0) then
    Add;
end;

procedure TDirectlinksPanel.FcxPageControlPageChanging;
begin
  if NewPage <> nil then
    Windows.SetFocus(NewPage.Handle)
  else
    Windows.SetFocus(FcxPageControl.Handle);
end;

function TDirectlinksPanel.GetMirrorControl;
begin
  Result := FMirrorControl;
end;

procedure TDirectlinksPanel.SetMirrorControl(AMirrorControl: IMirrorControl);
begin
  FMirrorControl := AMirrorControl;
end;

function TDirectlinksPanel.GetFileName;
var
  _Index, _Count: Integer;
  _Found: Boolean;
begin
  Result := '';

  _Index := 0;
  _Count := MirrorCount;
  _Found := False;

  while (_Index < _Count) and not _Found do
  begin
    _Found := not(Mirror[_Index].GetFileName = '');

    if not _Found then
      Inc(_Index);
  end;

  if _Found then
    Result := Mirror[_Index].GetFileName;
end;

function TDirectlinksPanel.GetActiveMirrorIndex;
begin
  Result := FcxPageControl.ActivePageIndex;
end;

function TDirectlinksPanel.GetActiveMirror;
begin
  Result := Mirror[ActiveMirrorIndex];
end;

function TDirectlinksPanel.GetMirror(index: Integer): IDirectlinksMirror;
begin
  Result := TMycxTabSheet(FcxPageControl.Pages[index]);
end;

function TDirectlinksPanel.GetMirrorCount: Integer;
begin
  Result := FcxPageControl.PageCount;
end;

function TDirectlinksPanel.GetSize: Extended;
var
  _Index, _Count: Integer;
  _Found: Boolean;
begin
  Result := 0;

  _Index := 0;
  _Count := MirrorCount;
  _Found := False;

  while (_Index < _Count) and not _Found do
  begin
    _Found := (Mirror[_Index].Size > 0);

    if not _Found then
      Inc(_Index);
  end;

  if _Found then
    Result := GetSize(_Index);
end;

function TDirectlinksPanel.GetSize(index: Integer): Extended;
begin
  Result := Mirror[ActiveMirrorIndex].Size;
end;

function TDirectlinksPanel.GetPartSize;
var
  _Index, _Count: Integer;
begin
  Result := 0;

  _Index := 0;
  _Count := MirrorCount;

  while (_Index < _Count) do
  begin
    if (Result < Mirror[ActiveMirrorIndex].PartSize) then
      Result := Mirror[ActiveMirrorIndex].PartSize;

    Inc(_Index);
  end;
end;

function TDirectlinksPanel.GetHoster: WideString;
begin
  Result := GetHoster(False);
end;

function TDirectlinksPanel.GetHoster(AShortName: Boolean): WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to MirrorCount - 1 do
  begin
    Result := Mirror[ActiveMirrorIndex].GetHoster(AShortName);

    if not(Result = '') then
      Break;
  end;
end;

function TDirectlinksPanel.GetParts: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to MirrorCount - 1 do
  begin
    Result := Mirror[ActiveMirrorIndex].Parts;

    if not(Result = -1) then
      Break;
  end;
end;

function TDirectlinksPanel.GetVisible;
begin
  Result := FcxPageControl.Visible;
end;

procedure TDirectlinksPanel.SetVisible(AVisible: Boolean);
begin
  FcxPageControl.Visible := AVisible;
end;

function TDirectlinksPanel.GetFocus: Boolean;
begin
  Result := Mirror[ActiveMirrorIndex].Focus;
end;

procedure TDirectlinksPanel.SetFocus(AFocus: Boolean);
begin
  Mirror[ActiveMirrorIndex].Focus := AFocus;
end;

constructor TDirectlinksPanel.Create;
begin
  MirrorControl := AMirrorControl;

  FPopupMenu := TPopupMenu.Create(AOwner);
  with FPopupMenu do
  begin
    with Items do
    begin
      FmiAddTab := TMenuItem.Create(FPopupMenu);
      with FmiAddTab do
      begin
        Caption := StrAdd;
        ShortCut := Menus.ShortCut($54, [ssCtrl]);
        OnClick := FmiAddTabClick;
      end;
      Add(FmiAddTab);

      FmiRemoveTab := TMenuItem.Create(FPopupMenu);
      with FmiRemoveTab do
      begin
        Caption := StrRemove;
        ShortCut := Menus.ShortCut($57, [ssCtrl]);
        OnClick := FmiRemoveTabClick;
      end;
      Add(FmiRemoveTab);
    end;
    OnPopup := FPopupMenuPopup;
  end;

  FcxPageControl := TcxPageControl.Create(AOwner);
  with FcxPageControl do
  begin
    Parent := TWinControl(AOwner);

    Align := alClient;

    Focusable := False;
    HotTrack := True;

    PopupMenu := FPopupMenu;

    OnDblClick := FcxPageControlDblClick;
    OnPageChanging := FcxPageControlPageChanging;

    Show;
  end;

  FcxLFirstSubMirrorInfo := TcxLabel.Create(FcxPageControl);
  with FcxLFirstSubMirrorInfo do
  begin
    Parent := FcxPageControl;

    Align := alClient;
    AutoSize := False;

    Caption := 'Double-click here to create the first sub-mirror';

    with Properties do
    begin
      with Alignment do
      begin
        Horz := taCenter;
        Vert := taVCenter;
      end;
      WordWrap := True;
    end;

    Transparent := True;

    OnDblClick := FcxPageControlDblClick;

    Show;
  end;
end;

function TDirectlinksPanel.Add;
var
  FcxTabSheet: TMycxTabSheet;
begin
  FcxLFirstSubMirrorInfo.Visible := False;
  FcxTabSheet := TMycxTabSheet.Create(FcxPageControl);
  with FcxTabSheet do
  begin
    DirectlinksPanel := Self;
    PageControl := FcxPageControl;
    Caption := 'Mirror ' + IntToStr(TabIndex + 1);
    PostCreate;

    FMycxRichEdit.Lines.Text := ALinks;

    Result := PageIndex;

    Show;
  end;
end;

procedure TDirectlinksPanel.Remove(ATabIndex: Integer);
var
  I: Integer;
begin
  with TMycxTabSheet(FcxPageControl.Pages[ATabIndex]) do
  begin
    PreDestroy;
    Free;
  end;
  for I := ATabIndex to MirrorCount - 1 do
    Mirror[I].Title := 'Mirror ' + IntToStr(I + 1);

  if (MirrorCount > 0) then
    FcxPageControl.SelectNextPage(True)
  else
    FcxLFirstSubMirrorInfo.Visible := True;
end;

destructor TDirectlinksPanel.Destroy;
var
  I: Integer;
begin
  // for I := FcxPageControl.PageCount - 1 downto 0 do
  // Remove(I);

  FcxLFirstSubMirrorInfo.Free;

  // problem with destroing this item... see: TMirrorController.Remove()
  FcxPageControl.Free;

  FmiRemoveTab.Free;
  FmiAddTab.Free;
  FPopupMenu.Free;

  MirrorControl := nil;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TCrypterPanel'}

procedure TCrypterPanel.FPanelContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TCrypterPanel.FcxGridFolderInfoTableViewColumn2CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);

const
  ImageIndent = 3;

var
  AImageRect, ATextRect: TRect;
  AImageIndex: Integer;
begin
  if FcxGridFolderInfoTableView.DataController.Values[AViewInfo.GridRecord.index, 0] = 'Status' then
  begin
    ACanvas.Brush.Color := AViewInfo.Params.Color;
    ACanvas.FillRect(AViewInfo.Bounds);
    AImageRect := AViewInfo.ContentBounds;
    AImageRect.Left := AImageRect.Left + ImageIndent;
    AImageRect.Right := AImageRect.Left + Main.ILContainerStatusImages.Width + 2;
    ATextRect := AViewInfo.ContentBounds;
    ATextRect.Left := AImageRect.Right + ImageIndent;

    case Status of
      255:
        AImageIndex := -1;
    else
      AImageIndex := Status;
    end;

    cxDrawImage(ACanvas.Handle, AImageRect, AImageRect, nil, Main.ILContainerStatusImages, AImageIndex, idmNormal, False, 0, Main.ImageList.BkColor, False);

    // ACanvas.DrawImage(Main.ImageList, AImageRect.Left, AImageRect.Top, AImageIndex);
    ACanvas.DrawTexT(AViewInfo.Text, ATextRect, DT_SINGLELINE or DT_LEFT);
    ADone := True;
  end;
end;

procedure TCrypterPanel.FcxTextEditLinkChange(Sender: TObject);
begin
  with FcxTextEditLink do
    Hint := Text;
  if Assigned(MirrorControl.MirrorController.OnChange) then
    MirrorControl.MirrorController.OnChange.Invoke(Self);
end;

procedure TCrypterPanel.FcxButtonLinkCheckClick(Sender: TObject);
begin
  if not(FcxTextEditLink.Text = '') then
    CheckFolder;
end;

function TCrypterPanel.GetMirrorControl;
begin
  Result := FMirrorControl;
end;

procedure TCrypterPanel.SetMirrorControl(AMirrorControl: IMirrorControl);
begin
  FMirrorControl := AMirrorControl;
end;

function TCrypterPanel.GetName;
begin
  Result := Crypter.name;
end;

function TCrypterPanel.GetLink;
begin
  Result := FcxTextEditLink.Text;
end;

procedure TCrypterPanel.SetLink(ALink: WideString);
begin
  FcxTextEditLink.Text := ALink;
end;

function TCrypterPanel.GetStatus: Byte;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.Status;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetSize: Extended;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.Size;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetPartSize: Extended;
begin
  Result := 0;
end;

function TCrypterPanel.GetHoster: WideString;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.Hoster;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetHosterShort: WideString;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.HosterShort;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetParts: Integer;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.Parts;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetStatusImage: WideString;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.StatusImage;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

function TCrypterPanel.GetStatusImageText: WideString;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo.StatusImageText;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

procedure TCrypterPanel.SetCrypter(ACrypter: TCrypterCollectionItem);
begin
  FCrypter := ACrypter;
end;

function TCrypterPanel.GetCrypterFolderInfo;
begin
  FCrypterFolderInfoLock.EnterReadLock;
  try
    Result := FCrypterFolderInfo;
  finally
    FCrypterFolderInfoLock.ExitReadLock;
  end;
end;

procedure TCrypterPanel.SetCrypterFolderInfo(ACrypterFolderInfo: TCrypterFolderInfo);
begin
  FCrypterFolderInfoLock.EnterWriteLock;
  try
    FCrypterFolderInfo := ACrypterFolderInfo;
  finally
    FCrypterFolderInfoLock.ExitWriteLock;
  end;
end;

function TCrypterPanel.GetVisible;
begin
  Result := FPanel.Visible;
end;

procedure TCrypterPanel.SetVisible(AVisible: Boolean);
begin
  FPanel.Visible := AVisible;
end;

function TCrypterPanel.GetFocus: Boolean;
begin
  Result := FcxTextEditLink.Focused;
end;

procedure TCrypterPanel.SetFocus(AFocus: Boolean);
begin
  if AFocus and FcxTextEditLink.CanFocusEx then
    FcxTextEditLink.SetFocus;
end;

constructor TCrypterPanel.Create;
begin
  MirrorControl := AMirrorControl;

  FPanel := TPanel.Create(AOwner);
  with FPanel do
  begin
    ParentBackground := False;
    Parent := TWinControl(AOwner);

    Align := alClient;
    Anchors := [akLeft, akTop, akRight, akBottom];
    BevelOuter := bvNone;
    Caption := '';
    Color := clWhite;

    OnContextPopup := FPanelContextPopup;
  end;

  with FCrypterFolderInfo do
    Status := 255;

  FcxTextEditLink := TcxTextEdit.Create(FPanel);
  with FcxTextEditLink do
  begin
    Parent := TWinControl(FPanel);

    Anchors := [akLeft, akTop, akRight];

    Left := 6;
    Top := 3;
    Width := FPanel.Width - 65;

    ShowHint := True;

    StyleFocused.BorderStyle := ebsThick;

    with Properties do
    begin

      OnChange := FcxTextEditLinkChange;
    end;
  end;

  FcxButtonLinkCheck := TcxButton.Create(FPanel);
  with FcxButtonLinkCheck do
  begin
    Parent := TWinControl(FPanel);

    Anchors := [akTop, akRight];

    Caption := StrCheck;

    Height := 21;
    Left := FPanel.Width - 55;
    Top := 2;
    Width := 50;

    OnClick := FcxButtonLinkCheckClick;
  end;

  FcxGridFolderInfo := TcxGrid.Create(FPanel);
  with FcxGridFolderInfo do
  begin
    Parent := TWinControl(FPanel);

    Anchors := [akLeft, akTop, akRight, akBottom];

    Height := FPanel.Height - 20 - 12;
    Left := 6;
    Top := 27;
    Width := FPanel.Width - 12;

    FcxGridFolderInfoLevel := Levels.Add;
    FcxGridFolderInfoTableView := CreateView(TcxGridTableView) as TcxGridTableView;
    FcxGridFolderInfoLevel.GridView := FcxGridFolderInfoTableView;

    with FcxGridFolderInfoTableView.OptionsView do
    begin
      ColumnAutoWidth := True;

      GroupByBox := False;
      ScrollBars := ssVertical;
    end;

    FcxGridFolderInfoTableViewColumn1 := FcxGridFolderInfoTableView.CreateColumn;
    FcxGridFolderInfoTableViewColumn1.Caption := 'Name';
    FcxGridFolderInfoTableViewColumn2 := FcxGridFolderInfoTableView.CreateColumn;
    with FcxGridFolderInfoTableViewColumn2 do
    begin
      Caption := 'Value';
      PropertiesClass := TcxTextEditProperties;
      Properties.readonly := True;
      OnCustomDrawCell := FcxGridFolderInfoTableViewColumn2CustomDrawCell;
    end;

    with FcxGridFolderInfoTableView do
    begin
      Items[0].PropertiesClass := TcxLabelProperties;

      with DataController do
        try
          BeginUpdate;
          RecordCount := 4;
          Values[0, 0] := 'Status';
          Values[1, 0] := 'Hoster';
          Values[2, 0] := StrSize;
          Values[3, 0] := StrParts;
        finally
          EndUpdate;
        end;
    end;
  end;

  Visible := False;
end;

procedure TCrypterPanel.RefreshGrid;
var
  I: Integer;
  StatusString, _Hoster: string;

  function FindRecordIndexByText(const aText: string): Integer;
  var
    RecordIndex: Integer;
  begin
    Result := -1;

    with FcxGridFolderInfoTableView.DataController do
    begin
      for RecordIndex := 0 to RecordCount - 1 do
        if (Values[RecordIndex, 0] = aText) then
        begin
          Result := RecordIndex;
          Break;
        end;
    end;
  end;

begin
  I := FindRecordIndexByText('Status');
  if (I > -1) then
  begin
    case Status of
      0:
        StatusString := StrOffline;
      1:
        StatusString := StrOnline;
      2:
        StatusString := StrUnknown;
      3:
        StatusString := StrNotyet;
      4:
        StatusString := StrMixed;
      255:
        StatusString := StrNoinfo;
    end;

    FcxGridFolderInfoTableView.DataController.Values[I, 1] := StatusString;
  end;

  _Hoster := Hoster;
  I := FindRecordIndexByText('Hoster');
  if (I > -1) then
    if SameStr(_Hoster, '') then
      FcxGridFolderInfoTableView.DataController.Values[I, 1] := 'n/a'
    else
      FcxGridFolderInfoTableView.DataController.Values[I, 1] := string(_Hoster) + ' (' + string(HosterShort) + ')';

  I := FindRecordIndexByText(StrSize);
  if (I > -1) then
    FcxGridFolderInfoTableView.DataController.Values[I, 1] := FloatToStr(Size) + ' MB';

  I := FindRecordIndexByText(StrParts);
  if (I > -1) then
    FcxGridFolderInfoTableView.DataController.Values[I, 1] := IntToStr(Parts);
end;

procedure TCrypterPanel.CreateFolder;
begin
  MirrorControl.MirrorController.TabSheetController.PageController.CrypterManager.AddCrypterJob(Self);
end;

procedure TCrypterPanel.CheckFolder(const AUseCheckDelay: Boolean = False);
begin
  MirrorControl.MirrorController.TabSheetController.PageController.CrypterManager.AddCrypterCheckJob(Self, AUseCheckDelay);
end;

destructor TCrypterPanel.Destroy;
begin
  FCrypter := nil;

  FcxGridFolderInfoTableViewColumn2.Free;
  FcxGridFolderInfoTableViewColumn1.Free;
  FcxGridFolderInfoTableView.Free;
  FcxGridFolderInfoLevel.Free;
  FcxGridFolderInfo.Free;

  FcxButtonLinkCheck.Free;
  FcxTextEditLink.Free;

  FPanel.Free;

  MirrorControl := nil;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TMirrorControl'}

procedure TMirrorControl.cxTabControlChange(Sender: TObject);

var
  I: Integer;
begin
  with FcxTabControl do
  begin
    FDirectlinksPanel.Visible := (TabIndex = 0);

    for I := 0 to CrypterCount - 1 do
      if (TabIndex > 0) then
        Crypter[I].Visible := (TabIndex - 1) = I
      else
        Crypter[I].Visible := False;
  end;

  // Mit gedrückter STRG-Taste werden alle TabIndex's der Mirror des aktuellen Uploads auf den aktuellen TabIndex gesetzt
  if (GetAsyncKeyState(VK_CONTROL) < 0) then
  begin
    for I := 0 to MirrorController.MirrorCount - 1 do
      MirrorController.Mirror[I].TabIndex := FcxTabControl.TabIndex;
  end;
end;

procedure TMirrorControl.cxTabControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(MirrorController.OnSpaceMouseDown) then
    MirrorController.OnSpaceMouseDown.Invoke(Self);
end;

procedure TMirrorControl.FPopupMenuPopup(Sender: TObject);
var
  I: Integer;
  NewMenuItem: TMenuItem;
begin
  with SettingsManager.Settings.ComponentParser do
  begin
    FmiMirrorIndex.Clear;
    for I := 0 to MirrorController.MirrorCount - 1 do
    begin
      NewMenuItem := TMenuItem.Create(FPopupMenu);
      with NewMenuItem do
      begin
        AutoCheck := True;
        RadioItem := True;
        Caption := IntToStr(I + 1);
        Checked := (I = index);
        if (I >= index) then
          Tag := I + 1
        else
          Tag := I;
        OnClick := FmiMirrorIndexClick;
      end;
      FmiMirrorIndex.Add(NewMenuItem);
    end;

    FmiMirrorPosition_Buttom.Checked := MirrorPosition = mpButtom;
    FmiMirrorPosition_Top.Checked := MirrorPosition = mpTop;

    FmiMirrorColumns1.Checked := MirrorColumns = 1;
    FmiMirrorColumns2.Checked := MirrorColumns = 2;
    FmiMirrorColumns3.Checked := MirrorColumns = 3;
  end;
end;

procedure TMirrorControl.FmiAddMirrorClick(Sender: TObject);
begin
  MirrorController.Add;
  Main.fMain.CallComponentparser;
end;

procedure TMirrorControl.FmiMirrorIndexClick(Sender: TObject);
begin
  index := (Sender as TMenuItem).Tag;
  Main.fMain.CallComponentparser;
end;

procedure TMirrorControl.FmiRemoveMirrorClick(Sender: TObject);
begin
  MirrorController.Remove(Self.index);
  // FcxTabControl.Free; // ???
  Main.fMain.CallComponentparser;
end;

procedure TMirrorControl.FmiMirrorColumnsClick(Sender: TObject);
begin
  with SettingsManager.Settings.ComponentParser do
    MirrorColumns := (Sender as TMenuItem).Tag;
  Settings.cxSEMirrorColumns.Value := (Sender as TMenuItem).Tag;
  Main.fMain.CallComponentparser;
end;

procedure TMirrorControl.FmiMirrorPositionClick(Sender: TObject);
begin
  with SettingsManager.Settings.ComponentParser do
    MirrorPosition := TMirrorPosition((Sender as TMenuItem).Tag);
  Settings.cxCOBMirrorPosition.ItemIndex := (Sender as TMenuItem).Tag;
  Main.fMain.CallComponentparser;
end;

procedure TMirrorControl.FmiCrypterClick(Sender: TObject);
begin
  Crypter[(Sender as TMenuItem).MenuIndex].CreateFolder;
end;

procedure TMirrorControl.FmiAllCrypterClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CrypterCount - 1 do
    Crypter[I].CreateFolder;
end;

function TMirrorControl.GetTabControlTabWidth: Integer;
var
  I, CurrentTabWidth: Integer;
begin
  Result := 0;
  with FcxTabControl do
    for I := 0 to Tabs.Count - 1 do
    begin
      CurrentTabWidth := (Tabs[I].FullRect.Right - Tabs[I].FullRect.Left);
      if CurrentTabWidth > Result then
        Result := CurrentTabWidth;
    end;
end;

function TMirrorControl.GetMirrorController;
begin
  Result := FMirrorController;
end;

procedure TMirrorControl.SetMirrorController(const AMirrorController: IMirrorController);
begin
  FMirrorController := AMirrorController;
end;

function TMirrorControl.GetIndex;
begin
  Result := MirrorController.IndexOf(Self);
end;

procedure TMirrorControl.SetIndex(AIndex: Integer);
var
  I, J: Integer;
  CrypterFound: Boolean;
begin
  with MirrorController.Insert(AIndex) do
  begin
    for I := 0 to Self.DirectlinksMirrorCount - 1 do
      Directlink.Add(Self.DirectlinksMirror[I]);
    for I := 0 to Self.CrypterCount - 1 do
    begin
      CrypterFound := False;
      for J := 0 to CrypterCount - 1 do
        if SameText(Self.Crypter[I].name, Crypter[J].name) then
        begin
          Crypter[J].Link := Self.Crypter[I].Link;
          CrypterFound := True;
        end;
      if not CrypterFound then
        with Crypter[AddCrypter(Self.Crypter[I].name)] do
        begin
          Link := Self.Crypter[I].Link;
          CheckFolder;
        end;
    end;
    TabIndex := Self.TabIndex;
    //
    Self.MirrorController.Remove(Self.index);
    Self.FcxTabControl.Free;
    //
    for I := 0 to CrypterCount - 1 do
      Crypter[I].CheckFolder;
  end;

end;

function TMirrorControl.GetTabIndex: Integer;
begin
  Result := FcxTabControl.TabIndex;
end;

procedure TMirrorControl.SetTabIndex(ATabIndex: Integer);
begin
  FcxTabControl.TabIndex := ATabIndex;
end;

function TMirrorControl.GetSize;
var
  I: Integer;
begin
  with TList<Extended>.Create do
    try
      if Directlink.Size > 0 then
        Add(Directlink.Size);

      for I := 0 to CrypterCount - 1 do
      begin
        if GetCrypter(I).Size > 0 then
          Add(GetCrypter(I).Size);
      end;

      if Count > 0 then
      begin
        Sort;

        Result := Items[Count div 2];
      end
      else
      begin
        Result := 0;
      end;
    finally
      Free;
    end;
end;

function TMirrorControl.GetPartSize;
begin
  Result := Directlink.PartSize;
end;

function TMirrorControl.GetHoster: WideString;
begin
  Result := GetHoster(False);
end;

function TMirrorControl.GetHoster(AShortName: Boolean): WideString;
var
  I: Integer;
begin
  Result := Directlink.GetHoster(AShortName);

  if (Result = '') then
    for I := 0 to CrypterCount - 1 do
    begin
      if AShortName then
        Result := GetCrypter(I).HosterShort
      else
        Result := GetCrypter(I).Hoster;

      if not(Result = '') then
        Break;
    end;
end;

function TMirrorControl.GetParts;
var
  I: Integer;
begin
  with TList<Integer>.Create do
    try
      if Directlink.Parts > -1 then
        Add(Directlink.Parts);

      for I := 0 to CrypterCount - 1 do
      begin
        if GetCrypter(I).Parts > -1 then
          Add(GetCrypter(I).Parts);
      end;

      if Count > 0 then
      begin
        Sort;

        Result := Items[Count div 2];
      end
      else
      begin
        Result := 0;
      end;
    finally
      Free;
    end;
end;

function TMirrorControl.GetDirectlink;
begin
  Result := FDirectlinksPanel;
end;

function TMirrorControl.GetDirectlinksMirror(index: Integer): WideString;
begin
  Result := FDirectlinksPanel.Mirror[index].Value;
end;

procedure TMirrorControl.SetDirectlinksMirror(index: Integer; ADirectlinks: WideString);
begin
  FDirectlinksPanel.Mirror[index].Value := ADirectlinks;
end;

function TMirrorControl.GetDirectlinksMirrorCount: Integer;
begin
  Result := FDirectlinksPanel.MirrorCount;
end;

function TMirrorControl.GetCrypter(index: Integer): ICrypterPanel;
begin
  // need to be the AS typecast!
  Result := FCrypterPanelList[index] as ICrypterPanel;
end;

function TMirrorControl.GetCrypterCount: Integer;
begin
  Result := FCrypterPanelList.Count;
end;

function TMirrorControl.AddCrypter;
var
  I: Integer;
  _NewMenuItem: TMenuItem;
  _CrypterPanel: TCrypterPanel;
  _CrypterPanelIntf: ICrypterPanel;
begin
  Result := -1;

  for I := 0 to CrypterCount - 1 do
    if (AName = Crypter[I].name) then
    begin
      Result := I;
      Exit;
    end;

  FcxTabControl.Tabs.Add(AName);

  _CrypterPanel := TCrypterPanel.Create(FcxTabControl, Self);
  with SettingsManager.Settings.Plugins do
    _CrypterPanel.Crypter := TCrypterCollectionItem(FindPlugInCollectionItemFromCollection(AName, Crypter));

  _CrypterPanelIntf := _CrypterPanel;
  Result := FCrypterPanelList.Add(_CrypterPanel);

  _NewMenuItem := TMenuItem.Create(FCrypterPopupMenu);
  FCrypterPopupMenu.Items.Add(_NewMenuItem);
  with _NewMenuItem do
  begin
    Caption := AName;
    // ShortCut := Menus.ShortCut($5A,[ssCtrl]);
    OnClick := FmiCrypterClick;
  end;

  if not Assigned(FcxButtonCrypt) then
  begin
    FcxButtonCrypt := TcxButton.Create(FcxTabControl);
    with FcxButtonCrypt do
    begin
      CanBeFocused := False;
      Caption := 'crypt';

      Left := 16;
      Top := FcxTabControl.Height - 26;
      // Top := 94;
      // Width := GetTabControlTabWidth - Left;

      Kind := cxbkDropDownButton;

      DropDownMenu := FCrypterPopupMenu;

      OnClick := FmiAllCrypterClick;

      Parent := TWinControl(FcxTabControl);
    end;
  end;
  { TODO : Width not correctly, when changing NativeStyle }
  with FcxButtonCrypt do
    Width := GetTabControlTabWidth - Left - 1;
end;

function TMirrorControl.RemoveCrypter;
begin
  if FcxTabControl.TabIndex = AIndex + 1 then
    FcxTabControl.TabIndex := AIndex;
  FcxTabControl.Tabs.Delete(AIndex + 1);
  FCrypterPanelList.Items[AIndex]._Release;
  FCrypterPanelList.Delete(AIndex);
  FCrypterPopupMenu.Items.Items[AIndex].Free;
  if (CrypterCount = 0) then
    FreeAndNil(FcxButtonCrypt)
  else
    with FcxButtonCrypt do
      Width := GetTabControlTabWidth - Left - 1;
end;

function TMirrorControl.GetLeft;
begin
  Result := FcxTabControl.Left;
end;

procedure TMirrorControl.SetLeft(ALeft: Integer);
begin
  FcxTabControl.Left := ALeft;
end;

function TMirrorControl.GetTop;
begin
  Result := FcxTabControl.Top;
end;

procedure TMirrorControl.SetTop(ATop: Integer);
begin
  FcxTabControl.Top := ATop;
end;

function TMirrorControl.GetWidth;
begin
  Result := FcxTabControl.Width;
end;

procedure TMirrorControl.SetWidth(AWidth: Integer);
begin
  FcxTabControl.Width := AWidth;
end;

function TMirrorControl.GetHeight;
begin
  Result := FcxTabControl.Height;
end;

procedure TMirrorControl.SetHeight(AHeight: Integer);
begin
  FcxTabControl.Height := AHeight;
  if Assigned(FcxButtonCrypt) then
    FcxButtonCrypt.Top := AHeight - 26;
end;

function TMirrorControl.GetFocus: Boolean;
begin
  if TabIndex = 0 then
    Result := Directlink.Focus
  else
    Result := Crypter[TabIndex - 1].Focus;
end;

procedure TMirrorControl.SetFocus(AFocus: Boolean);
begin
  if AFocus then
    if TabIndex = 0 then
      Directlink.Focus := True
    else
      Crypter[TabIndex - 1].Focus := True;
end;

constructor TMirrorControl.Create;
var
  I: Integer;
begin
  FPopupMenu := TPopupMenu.Create(AOwner);
  FPopupMenu.OnPopup := FPopupMenuPopup;
  with FPopupMenu.Items do
  begin
    FmiAddMirror := TMenuItem.Create(FPopupMenu);
    with FmiAddMirror do
    begin
      Caption := StrAdd;
      OnClick := FmiAddMirrorClick;
    end;
    Add(FmiAddMirror);

    FmiMirrorIndex := TMenuItem.Create(FPopupMenu);
    with FmiMirrorIndex do
    begin
      Caption := 'Index';
    end;
    Add(FmiMirrorIndex);

    FmiRemoveMirror := TMenuItem.Create(FPopupMenu);
    with FmiRemoveMirror do
    begin
      Caption := StrRemove;
      OnClick := FmiRemoveMirrorClick;
    end;
    Add(FmiRemoveMirror);

    FmiMirrorSettings := TMenuItem.Create(FPopupMenu);
    with FmiMirrorSettings do
    begin
      Caption := StrSettings;

      FmiMirrorColumns := TMenuItem.Create(FPopupMenu);
      with FmiMirrorColumns do
      begin
        Caption := 'Columns';
{$REGION 'Columns'}
        FmiMirrorColumns1 := TMenuItem.Create(FPopupMenu);
        with FmiMirrorColumns1 do
        begin
          AutoCheck := True;
          RadioItem := True;
          Caption := '1';
          Tag := 1;
          OnClick := FmiMirrorColumnsClick;
        end;
        Add(FmiMirrorColumns1);

        FmiMirrorColumns2 := TMenuItem.Create(FPopupMenu);
        with FmiMirrorColumns2 do
        begin
          AutoCheck := True;
          RadioItem := True;
          Caption := '2';
          Tag := 2;
          OnClick := FmiMirrorColumnsClick;
        end;
        Add(FmiMirrorColumns2);

        FmiMirrorColumns3 := TMenuItem.Create(FPopupMenu);
        with FmiMirrorColumns3 do
        begin
          AutoCheck := True;
          RadioItem := True;
          Caption := '3';
          Tag := 3;
          OnClick := FmiMirrorColumnsClick;
        end;
        Add(FmiMirrorColumns3);
{$ENDREGION}
      end;
      Add(FmiMirrorColumns);

      FmiMirrorPosition := TMenuItem.Create(FPopupMenu);
      with FmiMirrorPosition do
      begin
        Caption := 'Position';
{$REGION 'Positions'}
        FmiMirrorPosition_Top := TMenuItem.Create(FPopupMenu);
        with FmiMirrorPosition_Top do
        begin
          AutoCheck := True;
          RadioItem := True;
          Caption := StrTop;
          Tag := 0;
          OnClick := FmiMirrorPositionClick;
        end;
        Add(FmiMirrorPosition_Top);

        FmiMirrorPosition_Buttom := TMenuItem.Create(FPopupMenu);
        with FmiMirrorPosition_Buttom do
        begin
          AutoCheck := True;
          RadioItem := True;
          Caption := StrButtom;
          Tag := 1;
          OnClick := FmiMirrorPositionClick;
        end;
        Add(FmiMirrorPosition_Buttom);
{$ENDREGION}
      end;
      Add(FmiMirrorPosition);
    end;
    Add(FmiMirrorSettings);
  end;

  FCrypterPopupMenu := TPopupMenu.Create(AOwner);

  FcxTabControl := TcxTabControl.Create(AOwner);
  with FcxTabControl do
  begin
    Left := ALeft;
    Top := ATop;
    Height := SettingsManager.Settings.ComponentParser.MirrorHeight;

    Focusable := False;
    // Workaround for: http://www.devexpress.com/issue=B202502
    HideTabs := True;
    HotTrack := True;
    Options := Options + [pcoFixedTabWidthWhenRotated];

    NavigatorPosition := npRightBottom;

    Rotate := True;
    ShowFrame := True;
    TabPosition := tpLeft;

    LookAndFeel.Refresh;

    Tabs.Add(StrDirectlinks);

    PopupMenu := FPopupMenu;

    OnChange := cxTabControlChange;
    OnMouseDown := cxTabControlMouseDown;

    Parent := TWinControl(AOwner);
  end;

  FMirrorController := nil;

  FDirectlinksPanel := TDirectlinksPanel.Create(FcxTabControl, Self);

  FCrypterPanelList := TInterfaceList.Create;

  with SettingsManager.Settings do
  begin
    with Plugins.Crypter do
      for I := 0 to Count - 1 do
        if TPlugInCollectionItem(Items[I]).Enabled then
          AddCrypter(TPlugInCollectionItem(Items[I]).name);

    with ComponentParser do
      if not(DefaultMirrorTabIndex = StrDirectlinks) then
        for I := 1 to FcxTabControl.Tabs.Count - 1 do
          if (DefaultMirrorTabIndex = FcxTabControl.Tabs[I].Caption) then
          begin
            SetTabIndex(I);
            Break;
          end;
  end;
  FcxTabControl.HideTabs := False;
end;

destructor TMirrorControl.Destroy;
begin
  FcxButtonCrypt.Free;

  FCrypterPanelList.Free;

  FDirectlinksPanel := nil;

  FMirrorController := nil;

  FcxTabControl.Free;

  FCrypterPopupMenu.Free;

  FmiMirrorPosition_Buttom.Free;
  FmiMirrorPosition_Top.Free;
  FmiMirrorPosition.Free;
  FmiMirrorColumns3.Free;
  FmiMirrorColumns2.Free;
  FmiMirrorColumns1.Free;
  FmiMirrorColumns.Free;
  FmiMirrorSettings.Free;
  FmiRemoveMirror.Free;
  FmiMirrorIndex.Clear;
  FmiMirrorIndex.Free;
  FmiAddMirror.Free;

  FPopupMenu.Free;
end;
{$ENDREGION}
{ ****************************************************************************** }

end.
