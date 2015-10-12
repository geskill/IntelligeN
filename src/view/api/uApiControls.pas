unit uApiControls;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, StdCtrls, ExtCtrls, ShellAPI, Clipbrd, Dialogs, ActiveX, AxCtrls,
  jpeg, Generics.Collections,
  // PopupMenu Mod
  uMydxBarPopupMenu, // uMyPopupMenu,
  // Dev Express
  cxLookAndFeelPainters, cxLabel, cxEdit, cxTextEdit, cxDropDownEdit, cxCheckComboBox, cxCalendar, dxBar,
  // Dev Express Mod
  uMycxRichEdit, uMyTMonospaceHint,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl, OtlSync, OtlTask,
  // OLEDrop
  OLEDrop,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiControlsBase,
  // Utils
  uImageUtils, uStringUtils;

const
  WM_CONTROL_VALUE_CHANGE = WM_USER + 175;

type
  TIControlBasic = class(TIControlBase, IControlBasic)
  private
    FHandle: HWND;
    FComponentController: IControlController;
    FControlID: TControlID;
    FBufferedValues: TQueue<string>; // Omni Queue implementieren
    FTitleLabel, FHintLabel, FClearLabel: TcxLabel;
    FOleDrop: TOleDrop;
    procedure WndProc(var Msg: TMessage);
    // procedure WMControlValueChange(var Msg: TMessage); message WM_CONTROL_VALUE_CHANGE;
    function GetControlController: IControlController;
    procedure SetControlController(const AControlController: IControlController);
    function GetTypeID: TTypeID;
    function GetControlID: TControlID;
    function GetName: WideString;
    function GetTitle: WideString;
    function GetLeft: Integer;
    procedure SetLeft(ALeft: Integer);
    function GetTop: Integer;
    procedure SetTop(ATop: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AWidth: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AHeight: Integer);
    function GetHint: WideString;
    procedure SetHint(AHint: WideString);
    function GetValue: WideString;
    procedure SetValue(AValue: WideString); virtual;
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    procedure FPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FPanelResize(Sender: TObject);
    procedure FmiUndoClick(Sender: TObject);
    procedure FmiCutClick(Sender: TObject);
    procedure FmiCopyClick(Sender: TObject);
    procedure FmiPasteClick(Sender: TObject);
    procedure FmiDeleteClick(Sender: TObject);
    procedure FmiSelectAllClick(Sender: TObject);
    procedure FClearLabelClick(Sender: TObject);
  protected
    FValueBufferLock: TOmniCS;
    FValueLock: TOmniMREW;
    FPanel: TPanel;
    FPopupMenu: TMydxBarPopupMenu;
    procedure FPopupMenuPopup(Sender: TObject); virtual;
    function GetControl: TcxCustomTextEdit; virtual; abstract;
    function GetControlValue: WideString; virtual; abstract;
    procedure SetControlValue(AValue: WideString); virtual; abstract;
    procedure SetName(AName: WideString); virtual;
    procedure SetTitle(ATitle: WideString); virtual;
    function GetFocus: Boolean; virtual;
    procedure SetFocus(AFocus: Boolean); virtual;
    procedure ControlOnDrop(AText: PWideChar);
    procedure ControlOnChange(Sender: TObject); virtual;
    procedure ControlOnEnter(Sender: TObject); virtual;
    procedure ControlOnExit(Sender: TObject); virtual;
    procedure DefaultConfiguration; virtual;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); virtual;
    destructor Destroy; override;

    property ControlController: IControlController read GetControlController write SetControlController;

    property TypeID: TTypeID read GetTypeID;
    property ControlID: TControlID read GetControlID;
    property Name: WideString read GetName write SetName;
    property Title: WideString read GetTitle write SetTitle;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: WideString read GetHint write SetHint;
    property Value: WideString read GetValue write SetValue;
    property Focus: Boolean read GetFocus write SetFocus;
    property Visible: Boolean read GetVisible write SetVisible;

  end;

  TIControlEdit = class(TIControlBasic, IControlEdit)
  protected
    FEdit: TcxTextEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIControlComboBox = class(TIControlBasic, IControlComboBox)
  private
    function GetList: WideString;
  protected
    FComboBox: TcxComboBox;
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    procedure SetList(AList: WideString); virtual;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
    procedure DefaultConfiguration; override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIControlComboBoxList = class(TIControlComboBox, IControlComboBoxList)
  protected
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIControlCheckComboBox = class(TIControlBasic, IControlCheckComboBox)
  private
    function InternalIndexOf(AStr: string): Integer;
  protected
    FCheckComboBox: TcxCheckComboBox;
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    function GetList: WideString;
    procedure SetList(AList: WideString); virtual;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
    procedure DefaultConfiguration; override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIDateEdit = class(TIControlBasic, IDateEdit)
  protected
    FDateEdit: TcxDateEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIRichEdit = class(TIControlBasic, IRichEdit)
  protected
    FMycxRichEdit: TMycxRichEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIReleaseName = class(TIControlEdit)
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIReleaseDate = class(TIDateEdit)
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TITags = class(TIControlEdit)

  end;

  TITitle = class(TIControlEdit)

  end;

  TIArtist = class(TIControlEdit)

  end;

  TINotes = class(TIControlCheckComboBox)
  private
    procedure FCheckComboBoxPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIPassword = class(TIControlComboBox)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TPictureMirror = class(TInterfacedObject, IPictureMirror)
  private
    FPicture: IPicture;
    FImage: TImage;
    FName, FValue, FErrorMsg: string;
  protected
    function GetPicture: IPicture;
    procedure SetPicture(APicture: IPicture);
    function GetName: WideString;
    function GetOriginalValue: WideString;
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
    function GetErrorMsg: WideString;
    procedure SetErrorMsg(AErrorMsg: WideString);
    procedure SetHint;
  public
    constructor Create(AOwner: TWinControl; APicture: IPicture; AImageHoster: WideString);
    property Picture: IPicture read GetPicture write SetPicture;
    property Name: WideString read GetName;
    property OriginalValue: WideString read GetOriginalValue;
    property Value: WideString read GetValue write SetValue;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;

    procedure LocalUpload(ALocalPath: WideString);
    procedure RemoteUpload;

    destructor Destroy; override;
  end;

  TIPicture = class(TIControlComboBox, IPictureEx)
  private
    FPictureArrayLock: TOmniMREW;
    FPictureArray: array of TPictureInfo;
    FPictureMirrorList: TInterfaceList;
    FPictureMirrorPanel: TFlowPanel;
    FmiSe1: TdxBarSeparator;
    FmiRemoteUploadImageMenu, FmiLocalUploadImageMenu: TdxBarSubItem;
    FmiRemoteUploadAll: TdxBarButton;
    FmiSaveAs: TdxBarButton;
    FmiVisitImage: TdxBarButton;
    procedure DownloadImage(AImageLink: string; out AMemoryStream: TMemoryStream);

    procedure SaveImage(AMemoryStream: TMemoryStream);
    function GraphicAsVariant(AGraphic: TGraphic): Variant;
    procedure SetValuePicture(AIndex: Integer; AMemoryStream: TMemoryStream); overload;
    procedure FmiRemoteUploadImageClick(Sender: TObject);
    procedure FmiRemoteUploadAllImagesClick(Sender: TObject);
    procedure FmiLocalUploadImageClick(Sender: TObject);
    procedure FmiSaveAsClick(Sender: TObject);
    procedure FmiVisitImageClick(Sender: TObject);
  protected
    procedure FPopupMenuPopup(Sender: TObject); override;
    procedure SetTitle(ATitle: WideString); override;
    function GetMirror(AIndex: Integer): IPictureMirror;
    procedure SetMirror(AIndex: Integer; APictureMirror: IPictureMirror);
    function GetMirrorCount: Integer;
    function AddMirror(AName: WideString): Integer;
    function RemoveMirror(AIndex: Integer): Boolean;
    function GetValuePicture(AIndex: Integer): TPictureInfo;
    procedure SetValuePicture(AIndex: Integer; APictureInfo: TPictureInfo); overload;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;

    procedure AddProposedValue(const ASender: WideString; AValue: WideString; ATitle: WideString); override; safecall;

    procedure RemoteUpload(const AAfterCrawling: WordBool = False);

    property Mirror[AIndex: Integer]: IPictureMirror read GetMirror write SetMirror;
    property MirrorCount: Integer read GetMirrorCount;

    destructor Destroy; override;
  end;

  TITrailer = class(TIControlComboBox, ITrailer)
  private
    FmiSe1: TdxBarSeparator;
    FmiVisitImage: TdxBarButton;
    procedure FmiVisitImageClick(Sender: TObject);
  protected
    procedure FPopupMenuPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TISample = class(TIControlComboBox)

  end;

  TIAudioBitrate = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioBitrateType = class(TIControlComboBoxList)

  end;

  TIAudioEncoder = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioSamplingRate = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioStream = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIGenre = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TILanguage = class(TIControlCheckComboBox)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIRuntime = class(TIControlEdit)

  end;

  TIVideoCodec = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIVideoStream = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIVideoSystem = class(TIControlComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TINFO = class(TIRichEdit)
  private
    procedure FMycxRichEditMouseEnter(Sender: TObject);
    procedure FMycxRichEditMouseLeave(Sender: TObject);
    procedure FMycxRichEditDropFiles(var message: TMessage);
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;

    destructor Destroy; override;
  end;

  TIDescription = class(TIRichEdit)
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AControlController: IControlController; AComponentID: TControlID); override;
  end;

implementation

uses
  uSettings,
  // Api
  uApiSettings, uApiPlugins;
{ . }
{$REGION 'TIControlBasic'}

procedure TIControlBasic.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_CONTROL_VALUE_CHANGE then
  begin
    FValueBufferLock.Acquire;
    try
      SetControlValue(FBufferedValues.Dequeue);
    finally
      FValueBufferLock.Release;
    end;
  end
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{
  procedure TIBasic.WMControlValueChange(var Msg: TMessage);
  var
  Value: string;
  begin
  Value := string(Msg.WParam);

  SetControlValue(Value);
  end;
}

function TIControlBasic.GetControlController;
begin
  Result := FComponentController;
end;

procedure TIControlBasic.SetControlController(const AControlController: IControlController);
begin
  FComponentController := AControlController;
end;

function TIControlBasic.GetTypeID;
begin
  Result := ControlController.TypeID;
end;

function TIControlBasic.GetControlID;
begin
  Result := FControlID;
end;

function TIControlBasic.GetName;
begin
  Result := copy(FPanel.name, 2);
end;

function TIControlBasic.GetTitle;
begin
  Result := copy(FTitleLabel.Caption, 1, length(FTitleLabel.Caption) - 1);
end;

function TIControlBasic.GetLeft;
begin
  Result := FPanel.Left;
end;

procedure TIControlBasic.SetLeft(ALeft: Integer);
begin
  FPanel.Left := ALeft;
end;

function TIControlBasic.GetTop;
begin
  Result := FPanel.Top;
end;

procedure TIControlBasic.SetTop(ATop: Integer);
begin
  FPanel.Top := ATop;
end;

function TIControlBasic.GetWidth;
begin
  Result := FPanel.Width;
end;

procedure TIControlBasic.SetWidth(AWidth: Integer);
begin
  FPanel.Width := AWidth;
end;

function TIControlBasic.GetHeight;
begin
  Result := FPanel.Height;
end;

procedure TIControlBasic.SetHeight(AHeight: Integer);
begin
  FPanel.Height := AHeight;
end;

function TIControlBasic.GetHint;
begin
  Result := FHintLabel.Hint;
end;

procedure TIControlBasic.SetHint(AHint: WideString);
begin
  FHintLabel.Hint := AHint;
end;

function TIControlBasic.GetValue;
begin
  FValueLock.EnterReadLock;
  try
    // TODO: Fix this
    // Result := FValue;
  finally
    FValueLock.ExitReadLock;
  end;
end;

procedure TIControlBasic.SetValue(AValue: WideString);
begin
  FValueBufferLock.Acquire;
  try
    FBufferedValues.Enqueue(AValue);
  finally
    FValueBufferLock.Release;
  end;
  PostMessage(FHandle, WM_CONTROL_VALUE_CHANGE, 0, 0);
end;

function TIControlBasic.GetVisible;
begin
  Result := FPanel.Visible;
end;

procedure TIControlBasic.SetVisible(AVisible: Boolean);
begin
  FPanel.Visible := AVisible;
end;

procedure TIControlBasic.FPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  if Assigned(ControlController.OnSpaceMouseDown) then
    ControlController.OnSpaceMouseDown.Invoke(Self);
end;

procedure TIControlBasic.FPanelResize(Sender: TObject);
begin
  if Assigned(FClearLabel) then
    with FClearLabel do
    begin
      Left := FPanel.Width - FClearLabel.Width - 3;
    end;
end;

procedure TIControlBasic.FmiUndoClick(Sender: TObject);
begin
  GetControl.Undo;
end;

procedure TIControlBasic.FmiCutClick(Sender: TObject);
begin
  GetControl.CutToClipboard;
end;

procedure TIControlBasic.FmiCopyClick(Sender: TObject);
begin
  GetControl.CopyToClipboard;
end;

procedure TIControlBasic.FmiPasteClick(Sender: TObject);
begin
  GetControl.PasteFromClipboard;
end;

procedure TIControlBasic.FmiDeleteClick(Sender: TObject);
begin
  GetControl.ClearSelection;
end;

procedure TIControlBasic.FmiSelectAllClick(Sender: TObject);
begin
  GetControl.SelectAll;
end;

procedure TIControlBasic.FClearLabelClick(Sender: TObject);
begin
  GetControl.Clear;
end;

procedure TIControlBasic.FPopupMenuPopup(Sender: TObject);
begin
  with FPopupMenu do
  begin
    FmiUndo.Enabled := True;
    FmiCut.Enabled := (GetControl.SelLength > 0);
    FmiCopy.Enabled := (GetControl.SelLength > 0);
    FmiPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
    FmiDelete.Enabled := (GetControl.SelLength > 0);
    FmiSelectAll.Enabled := True;
  end;
end;

procedure TIControlBasic.SetName(AName: WideString);
begin
  with FPanel do
  begin
    name := 'p' + AName;
    Caption := '';
  end;
  GetControl.Name := AName;
end;

procedure TIControlBasic.SetTitle(ATitle: WideString);
begin
  with FTitleLabel do
  begin
    Caption := ATitle + ':';

    Width := Canvas.TextWidth(Caption);

    FHintLabel.Left := FTitleLabel.Width + 6;
  end;
end;

function TIControlBasic.GetFocus;
begin
  Result := GetControl.Focused;
end;

procedure TIControlBasic.SetFocus(AFocus: Boolean);
begin
  if AFocus and GetControl.CanFocusEx then
    GetControl.SetFocus;
end;

procedure TIControlBasic.ControlOnDrop(AText: PWideChar);
begin
  Value := AText;
end;

procedure TIControlBasic.ControlOnChange(Sender: TObject);
begin
  FValueLock.EnterWriteLock;
  try
    // TODO: Fix thi
    // FValue := GetControlValue;
  finally
    FValueLock.ExitWriteLock;
  end;

  if Assigned(ControlController.OnControlChange) then
    ControlController.OnControlChange.Invoke(Self);
end;

procedure TIControlBasic.ControlOnEnter(Sender: TObject);
begin
  if Assigned(ControlController.OnControlEnter) then
    ControlController.OnControlEnter.Invoke(Self);
  if Assigned(ControlController.OnPopupMenuChange) then
    ControlController.OnPopupMenuChange.Invoke(Integer(FPopupMenu.ItemLinks));
end;

procedure TIControlBasic.ControlOnExit(Sender: TObject);
begin
  if Assigned(ControlController.OnPopupMenuChange) then
    ControlController.OnPopupMenuChange.Invoke(Integer(nil));
  if Assigned(ControlController.OnControlExit) then
    ControlController.OnControlExit.Invoke(Self);
end;

procedure TIControlBasic.DefaultConfiguration;
begin
  with SettingsManager.Settings.Controls.Controls[TypeID, ControlID] do
  begin
    SetTitle(Title);
    SetHint(HelpText);
    SetValue(Value);
  end;
end;

constructor TIControlBasic.Create;
begin
  FHandle := AllocateHWnd(WndProc);

  FBufferedValues := TQueue<string>.Create;

  FComponentController := AControlController;
  FControlID := AComponentID;

  SetLength(FValueArray, 0);

  FPanel := TPanel.Create(AOwner);
  with FPanel do
  begin
    ParentBackground := False;

    Parent := AOwner;

    BevelOuter := bvNone;

    with Constraints do
    begin
      MinHeight := 13;
      MinWidth := 10;
    end;

    Caption := '';
    Color := clWhite;

    OnMouseDown := FPanelMouseDown;
    OnResize := FPanelResize;
  end;

  FTitleLabel := TcxLabel.Create(FPanel);
  with FTitleLabel do
  begin
    Parent := FPanel;

    Top := 0;
    Left := 0;
  end;

  FHintLabel := TcxLabel.Create(FPanel);
  with FHintLabel do
  begin
    Parent := FPanel;
    Top := 0;
    Width := Canvas.TextWidth('?');
    Caption := '?';
    { TODO 3 : In den Einstellungen Farbe wählbar }
    // Font.Color := clBlue;
    Style.Font.Style := [fsBold, fsUnderline];
    Cursor := crHandPoint;
    ShowHint := True;
  end;

  FClearLabel := TcxLabel.Create(FPanel);
  with FClearLabel do
  begin
    Parent := FPanel;
    Top := 0;
    Width := Canvas.TextWidth('X');
    Left := FPanel.Width - FClearLabel.Width - 3;
    Caption := 'X';
    { TODO 3 : In den Einstellungen Farbe wählbar }
    // Font.Color := clBlue;
    Style.Font.Style := [fsBold, fsUnderline];
    Cursor := crHandPoint;
    ShowHint := True;
    Hint := StrClear;

    OnClick := FClearLabelClick;
  end;

  FPopupMenu := TMydxBarPopupMenu.Create(FPanel);
  with FPopupMenu do
  begin
    FmiUndo.OnClick := FmiUndoClick;
    FmiCut.OnClick := FmiCutClick;
    FmiCopy.OnClick := FmiCopyClick;
    FmiPaste.OnClick := FmiPasteClick;
    FmiDelete.OnClick := FmiDeleteClick;
    FmiSelectAll.OnClick := FmiSelectAllClick;

    OnPopup := FPopupMenuPopup;
  end;

  FOleDrop := TOleDrop.Create(FPanel.Handle);
  with FOleDrop do
    OnTextDropped := ControlOnDrop;
end;

destructor TIControlBasic.Destroy;

  procedure DeallocateHWnd(Wnd: HWND);
  var
    Instance: Pointer;
  begin
    Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
    if Instance <> @DefWindowProc then
      // make sure we restore the old, original windows procedure before leaving
      SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
    DestroyWindow(Wnd);
  end;

begin
  FOleDrop.Free;

  FPopupMenu.Free;

  FClearLabel.Free;
  FHintLabel.Free;
  FTitleLabel.Free;

  FPanel.Free;

  FComponentController := nil;

  FBufferedValues.Free;

  DeallocateHWnd(FHandle);
  FHandle := 0;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlEdit'}

function TIControlEdit.GetControl;
begin
  Result := FEdit;
end;

function TIControlEdit.GetControlValue;
begin
  Result := FEdit.Text;
end;

procedure TIControlEdit.SetControlValue(AValue: WideString);
begin
  FEdit.Text := AValue;
end;

constructor TIControlEdit.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 50;
  end;

  FEdit := TcxTextEdit.Create(AOwner);
  with FEdit do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  DefaultConfiguration;
end;

destructor TIControlEdit.Destroy;
begin
  FEdit.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlComboBox'}

function TIControlComboBox.GetList;
begin
  Result := FComboBox.Properties.Items.Text;
end;

function TIControlComboBox.GetDropDownRows;
begin
  Result := FComboBox.Properties.DropDownRows;
end;

procedure TIControlComboBox.SetDropDownRows(ADropDownRows: Integer);
begin
  FComboBox.Properties.DropDownRows := ADropDownRows;
end;

procedure TIControlComboBox.SetList(AList: WideString);
begin
  FComboBox.Properties.Items.Text := AList;
end;

function TIControlComboBox.GetControl;
begin
  Result := FComboBox;
end;

function TIControlComboBox.GetControlValue;
begin
  Result := FComboBox.Text;
end;

procedure TIControlComboBox.SetControlValue(AValue: WideString);
begin
  FComboBox.Text := AValue;
end;

procedure TIControlComboBox.DefaultConfiguration;
begin
  inherited DefaultConfiguration;
  with SettingsManager.Settings.Controls.Controls[TypeID, ControlID] do
    SetList(GetItems);
end;

constructor TIControlComboBox.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 50;
  end;

  FComboBox := TcxComboBox.Create(AOwner);
  with FComboBox do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := FPopupMenu;

    with Properties do
    begin
      DropDownRows := SettingsManager.Settings.Controls.DropDownRows;
      OnChange := ControlOnChange;
    end;

    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  DefaultConfiguration;
end;

destructor TIControlComboBox.Destroy;
begin
  FComboBox.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlComboBoxList'}

procedure TIControlComboBoxList.SetControlValue(AValue: WideString);
begin
  inherited SetControlValue(SettingsManager.Settings.Controls.GetCustomisedComponentValue(ControlID, TypeID, AValue));
end;

constructor TIControlComboBoxList.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  FPopupMenu.ItemLinks.Clear;

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  with FComboBox do
  begin
    PopupMenu := nil;

    with Properties do
      DropDownListStyle := lsFixedList;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlCheckComboBox'}

function TIControlCheckComboBox.InternalIndexOf(AStr: string): Integer;
var
  _Index, _Count: Integer;
  _Found: Boolean;
begin
  Result := -1;

  _Index := 0;
  _Found := False;
  _Count := FCheckComboBox.Properties.Items.Count;

  while (_Index < _Count) and not _Found do
  begin
    _Found := SameStr(FCheckComboBox.Properties.Items.Items[_Index].Description, AStr);
    if not _Found then
      Inc(_Index);
  end;

  if _Found then
    Result := _Index;
end;

function TIControlCheckComboBox.GetDropDownRows;
begin
  Result := FCheckComboBox.Properties.DropDownRows;
end;

procedure TIControlCheckComboBox.SetDropDownRows(ADropDownRows: Integer);
begin
  FCheckComboBox.Properties.DropDownRows := ADropDownRows;
end;

function TIControlCheckComboBox.GetList;
var
  I: Integer;
begin
  with TStringList.Create do
    try
      for I := 0 to FCheckComboBox.Properties.Items.Count - 1 do
        Add(FCheckComboBox.Properties.Items.Items[I].Description);
      Result := Text;
    finally
      Free;
    end;
end;

procedure TIControlCheckComboBox.SetList(AList: WideString);
var
  I: Integer;
begin
  FCheckComboBox.Properties.Items.Clear;

  with TStringList.Create do
    try
      Text := AList;

      for I := 0 to Count - 1 do
      begin
        with FCheckComboBox.Properties.Items.Add do
          Description := Strings[I];
      end;
    finally
      Free;
    end;
end;

function TIControlCheckComboBox.GetControl;
begin
  Result := FCheckComboBox;
end;

function TIControlCheckComboBox.GetControlValue;
begin
  Result := FCheckComboBox.Text;
end;

procedure TIControlCheckComboBox.SetControlValue(AValue: WideString);
var
  I, findex: Integer;
begin
  with SplittString(';', AValue) do
    try
      for I := 0 to Count - 1 do
      begin
        findex := InternalIndexOf(SettingsManager.Settings.Controls.GetCustomisedComponentValue(ControlID, TypeID, PChar(Strings[I])));

        if findex <> -1 then
          FCheckComboBox.States[findex] := cbsChecked;
      end;
    finally
      Free;
    end;
end;

procedure TIControlCheckComboBox.DefaultConfiguration;
begin
  inherited DefaultConfiguration;
  with SettingsManager.Settings.Controls.Controls[TypeID, ControlID] do
    SetList(GetItems);
end;

constructor TIControlCheckComboBox.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  FPopupMenu.ItemLinks.Clear;

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  FCheckComboBox := TcxCheckComboBox.Create(AOwner);
  with FCheckComboBox do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := nil;

    with Properties do
    begin
      EmptySelectionText := '';
      DropDownRows := SettingsManager.Settings.Controls.DropDownRows;
      OnChange := ControlOnChange;
    end;

    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  DefaultConfiguration;
end;

destructor TIControlCheckComboBox.Destroy;
begin
  FCheckComboBox.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIDateEdit'}

function TIDateEdit.GetControl;
begin
  Result := FDateEdit;
end;

function TIDateEdit.GetControlValue;
begin
  Result := FDateEdit.Text;
end;

procedure TIDateEdit.SetControlValue(AValue: WideString);
var
  FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);

  FDateEdit.Date := StrToDateDef(AValue, Date, FormatSettings);
end;

constructor TIDateEdit.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  FDateEdit := TcxDateEdit.Create(AOwner);
  with FDateEdit do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  DefaultConfiguration;
end;

destructor TIDateEdit.Destroy;
begin
  FDateEdit.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIRichEdit'}

function TIRichEdit.GetControl;
begin
  Result := FMycxRichEdit;
end;

function TIRichEdit.GetControlValue;
begin
  if SettingsManager.Settings.Controls.IRichEditWrapText then
    Result := FMycxRichEdit.Lines.Text
  else
    Result := FMycxRichEdit.Text;
end;

procedure TIRichEdit.SetControlValue(AValue: WideString);
begin
  FMycxRichEdit.Lines.Text := AValue;
end;

constructor TIRichEdit.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 96;
    MinWidth := 150;
  end;

  FMycxRichEdit := TMycxRichEdit.Create(AOwner);
  with FMycxRichEdit do
  begin
    name := Self.name;
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Height := FPanel.Height - Top;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight, akBottom];

    StyleFocused.BorderStyle := ebsThick;

    with Properties do
      ScrollBars := ssBoth;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  DefaultConfiguration;
end;

destructor TIRichEdit.Destroy;
begin
  FMycxRichEdit.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIReleaseName'}

procedure TIReleaseName.ControlOnChange(Sender: TObject);
begin
  inherited ControlOnChange(Sender);

  if Assigned(ControlController.OnReleaseNameChange) then
    ControlController.OnReleaseNameChange.Invoke(FEdit.Text);
end;

constructor TIReleaseName.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 150;
  end;

  with FEdit do
  begin
    // Name := Settings.Name;
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    with Properties do
      OnChange := ControlOnChange;

    {
      // Vordere Button erstmal unsichtbar
      Button.Visible := FALSE;

      // DropDown Menu
      AltBtnKind := bkDropDown;
      AltBtnVisible := TRUE;
      AltBtn.OnClick := eReleaseNameAltBtnClick;
      }

    // Text := Value;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIReleaseDate'}

constructor TIReleaseDate.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  FDateEdit.Date := Date;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TINotes'}

procedure TINotes.FCheckComboBoxPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = 1 then
  begin
    with Settings do
    begin
      cxPCMain.ActivePage := cxTSControls;
      cxPCControls.ActivePage := cxTSControls_;

      cxTCControls.TabIndex := Integer(cNotes);

      Show;
    end;
  end;
end;

constructor TINotes.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
    MinWidth := 75;

  with TcxComboBoxProperties(FCheckComboBox.Properties) do
  begin
    OnButtonClick := FCheckComboBoxPropertiesButtonClick;

    with TcxEditButton(Buttons.Add) do
      Kind := bkEllipsis;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIPassword'}

constructor TIPassword.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
    MinWidth := 75;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TPictureMirror'}

function TPictureMirror.GetPicture: IPicture;
begin
  Result := FPicture;
end;

procedure TPictureMirror.SetPicture(APicture: IPicture);
begin
  FPicture := APicture;
end;

function TPictureMirror.GetName: WideString;
begin
  Result := FName;
end;

function TPictureMirror.GetOriginalValue: WideString;
begin
  Result := FPicture.Value;
end;

function TPictureMirror.GetValue: WideString;
begin
  Result := FValue;
end;

procedure TPictureMirror.SetValue(AValue: WideString);
begin
  FValue := AValue;
  SetHint;
end;

function TPictureMirror.GetErrorMsg: WideString;
begin
  Result := FErrorMsg;
end;

procedure TPictureMirror.SetErrorMsg(AErrorMsg: WideString);
begin
  FErrorMsg := AErrorMsg;
  SetHint;
end;

procedure TPictureMirror.SetHint;
begin
  if not SameStr('', ErrorMsg) then
    FImage.Hint := Name + ' Error:' + sLineBreak + ErrorMsg
  else if SameStr('', Value) then
    FImage.Hint := Name + ': (no value)'
  else
    FImage.Hint := Name + ': ' + Value;
end;

constructor TPictureMirror.Create(AOwner: TWinControl; APicture: IPicture; AImageHoster: WideString);
begin
  FImage := TImage.Create(AOwner);
  with FImage do
  begin
    Parent := AOwner;

    Center := True;

    ShowHint := True;
    Transparent := True;

    Height := 16;
    Width := 16;

    with SettingsManager.Settings.Plugins do
      Picture.Icon.Assign(TImageHosterCollectionItem(FindPlugInCollectionItemFromCollection(AImageHoster, ImageHoster)).Icon);
  end;
  FPicture := APicture;
  FName := AImageHoster;
  FValue := '';
end;

procedure TPictureMirror.LocalUpload(ALocalPath: WideString);
begin
  FPicture.ControlController.TabSheetController.PageController.ImageHosterManager.AddLocalUploadJob(Self, ALocalPath);
end;

procedure TPictureMirror.RemoteUpload;
begin
  FPicture.ControlController.TabSheetController.PageController.ImageHosterManager.AddRemoteUploadJob(Self);
end;

destructor TPictureMirror.Destroy;
begin
  FPicture := nil;
  FImage.Free;
  inherited Destroy;
end;
{$ENDREGION}
{$REGION 'TIPicture'}

procedure TIPicture.DownloadImage(AImageLink: string; out AMemoryStream: TMemoryStream);
var
  HTTPManager: IHTTPManager;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;
  HTTPProcess: IHTTPProcess;
  OleStream: TOleStream;
  Dummy: Int64;
begin
  HTTPManager := THTTPManager.Instance();

  AMemoryStream := TMemoryStream.Create;

  HTTPOptions := THTTPOptions.Create(SettingsManager.Settings.HTTP.GetProxy(psaCrawler));

  HTTPOptions.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
  HTTPOptions.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;

  RequestID := HTTPManager.Get(THTTPRequest.Create(AImageLink), HTTPOptions);

  repeat
    sleep(75);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  OleStream := TOleStream.Create(HTTPProcess.HTTPResult.HTTPResponse.ContentStream);
  try
    HTTPProcess.HTTPResult.HTTPResponse.ContentStream.Seek(0, STREAM_SEEK_SET, Dummy);
    OleStream.Seek(0, STREAM_SEEK_SET);
    AMemoryStream.CopyFrom(OleStream, OleStream.Size);
  finally
    OleStream.Free;
  end;

  HTTPProcess := nil;
  HTTPOptions := nil;
  HTTPManager := nil;
end;

procedure TIPicture.SaveImage(AMemoryStream: TMemoryStream);

  function GetFileName(AMemoryStream: TMemoryStream): string;
  var
    FileName, FileExt: string;
  begin
    FileName := '';
    with ControlController do
      if not SameStr('', FindControl(cReleaseName).Value) then
        FileName := FindControl(cReleaseName).Value
      else
        FileName := FindControl(cTitle).Value;

    FileExt := GetTGraphicFileExt(AMemoryStream);
    if SameStr('', FileExt) then
      FileExt := ExtractFileExt(Value);

    Result := FileName + FileExt;
  end;

begin
  with TSaveDialog.Create(nil) do
    try
      FileName := ExtractFilePath(ParamStr(0)) + GetFileName(AMemoryStream);
      if Execute then
        AMemoryStream.SaveToFile(FileName);
    finally
      Free;
    end;
end;

function TIPicture.GraphicAsVariant(AGraphic: TGraphic): Variant;

  function GetBitmap(const Graphic: TGraphic): TBitmap;
  begin
    Result := TBitmap.Create;
    if Assigned(Graphic) and not Graphic.Empty then
    begin
      Result.SetSize(Graphic.Width, Graphic.Height);
      with Result.Canvas do
        try
          Draw(0, 0, Graphic);
        except

        end;
    end;
  end;

var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    with GetBitmap(AGraphic) do
      try
        SaveToStream(StringStream);
      finally
        Free;
      end;
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

procedure TIPicture.SetValuePicture(AIndex: Integer; AMemoryStream: TMemoryStream);
var
  PictureInfo: TPictureInfo;
  FGraphic: TGraphic;
begin
  with PictureInfo do
  begin
    Picture := '';
    Downloaded := True;
    Size := 0;
    Width := 0;
    Height := 0;
  end;

  PictureInfo.Size := AMemoryStream.Size;

  FGraphic := GetTGraphicType(AMemoryStream).Create;
  try
    AMemoryStream.Position := 0;

    FGraphic.LoadFromStream(AMemoryStream);

    if FGraphic.InheritsFrom(TJPEGImage) then
      with TJPEGImage(FGraphic) do
        DIBNeeded;

    with PictureInfo do
    begin
      Picture := GraphicAsVariant(FGraphic);
      Width := FGraphic.Width;
      Height := FGraphic.Height;
    end;
  finally
    FGraphic.Free;
  end;

  SetValuePicture(AIndex, PictureInfo);
end;

procedure TIPicture.FmiRemoteUploadImageClick(Sender: TObject);
begin
  Mirror[(Sender as TdxBarItem).ClickItemLink.CanVisibleIndex].RemoteUpload;
end;

procedure TIPicture.FmiRemoteUploadAllImagesClick(Sender: TObject);
begin
  RemoteUpload;
end;

procedure TIPicture.FmiLocalUploadImageClick(Sender: TObject);
const
  IMAGE_FILES = '*.bmp;*.gif;*.jpg;*.png;*.tga;*.tif';
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := StrImageFiles + ' (' + IMAGE_FILES + ')|' + IMAGE_FILES;
      Filter := Filter + '|' + StrAllFiles + ' (*.*)|*.*';
      if Execute then
        Mirror[(Sender as TdxBarItem).ClickItemLink.CanVisibleIndex].LocalUpload(FileName);
    finally
      Free;
    end;
end;

procedure TIPicture.FmiSaveAsClick(Sender: TObject);
var
  MemoryStream: TMemoryStream;
begin
  Parallel.Async(
    { } procedure
    { } begin
    { . } DownloadImage(Value, MemoryStream);
    { } end,
    { } Parallel.TaskConfig.OnTerminated(
      { } procedure(const task: IOmniTaskControl)
      { } begin
      { . } SaveImage(MemoryStream);
      { . } MemoryStream.Free;
      { } end
      { } ));
end;

procedure TIPicture.FmiVisitImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(Value), nil, nil, SW_SHOW);
end;

procedure TIPicture.FPopupMenuPopup(Sender: TObject);
var
  IsEmpty: Boolean;
begin
  inherited FPopupMenuPopup(Sender);

  IsEmpty := (FComboBox.Text = '');

  FmiRemoteUploadImageMenu.Enabled := not IsEmpty and (MirrorCount > 0);
  FmiRemoteUploadAll.Enabled := not IsEmpty and (MirrorCount > 0);
  FmiLocalUploadImageMenu.Enabled := (MirrorCount > 0);
  FmiSaveAs.Enabled := not IsEmpty;
  FmiVisitImage.Enabled := not IsEmpty;
end;

procedure TIPicture.SetTitle(ATitle: WideString);
begin
  inherited SetTitle(ATitle);

  if Assigned(FPictureMirrorPanel) then
    FPictureMirrorPanel.Left := FHintLabel.Left + FHintLabel.Width + 6;
end;

function TIPicture.GetMirror(AIndex: Integer): IPictureMirror;
begin
  Result := (FPictureMirrorList[AIndex] as IPictureMirror);
end;

procedure TIPicture.SetMirror(AIndex: Integer; APictureMirror: IPictureMirror);
begin
  FPictureMirrorList[AIndex] := APictureMirror;
end;

function TIPicture.GetMirrorCount: Integer;
begin
  Result := FPictureMirrorList.Count;
end;

function TIPicture.AddMirror(AName: WideString): Integer;
var
  I: Integer;
  PictureMirror: IPictureMirror;
  NewMenuItem: TdxBarButton;
begin
  Result := -1;

  for I := 0 to MirrorCount - 1 do
    if (AName = Mirror[I].Name) then
    begin
      Result := I;
      Exit;
    end;

  // licence check, only 1 imagehoster allowed

  PictureMirror := TPictureMirror.Create(FPictureMirrorPanel, Self as IPicture, AName);

  Result := FPictureMirrorList.Add(PictureMirror);

  NewMenuItem := TdxBarButton.Create(FPopupMenu);
  with NewMenuItem do
  begin
    Caption := AName;

    OnClick := FmiRemoteUploadImageClick;
  end;
  with FmiRemoteUploadImageMenu.ItemLinks.Add do
    Item := NewMenuItem;
  NewMenuItem := TdxBarButton.Create(FPopupMenu);
  with NewMenuItem do
  begin
    Caption := AName;

    OnClick := FmiLocalUploadImageClick;
  end;
  with FmiLocalUploadImageMenu.ItemLinks.Add do
    Item := NewMenuItem;
end;

function TIPicture.RemoveMirror(AIndex: Integer): Boolean;
begin
  Result := True;
  try
    FPictureMirrorList.Delete(AIndex);
    FmiLocalUploadImageMenu.ItemLinks[AIndex].Free;
    FmiRemoteUploadImageMenu.ItemLinks[AIndex].Free;
  except
    Result := False;
  end;
end;

function TIPicture.GetValuePicture(AIndex: Integer): TPictureInfo;
begin
  FPictureArrayLock.EnterReadLock;
  try
    Result := FPictureArray[AIndex];
  finally
    FPictureArrayLock.ExitReadLock;
  end;
end;

procedure TIPicture.SetValuePicture(AIndex: Integer; APictureInfo: TPictureInfo);
begin
  FPictureArrayLock.EnterReadLock;
  try
    FPictureArray[AIndex] := APictureInfo;
  finally
    FPictureArrayLock.ExitReadLock;
  end;
end;

constructor TIPicture.Create;
var
  I: Integer;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  SetLength(FPictureArray, 0);

  FPictureMirrorList := TInterfaceList.Create;

  FPictureMirrorPanel := TFlowPanel.Create(FPanel);
  with FPictureMirrorPanel do
  begin
    ParentBackground := False;

    Parent := FPanel;

    Anchors := [akLeft, akTop, akRight];

    BevelOuter := bvNone;
    Caption := '';
    Color := clWhite;

    Top := 0;
    Left := FHintLabel.Left + FHintLabel.Width + 6;
    Height := 16;
    Width := FClearLabel.Left - Left - 6;
  end;

  with FPopupMenu do
  begin
    FmiSe1 := TdxBarSeparator.Create(FPopupMenu);
    with FmiSe1 do
    begin
      ShowCaption := False;
    end;
    with ItemLinks.Add do
    begin
      Item := FmiSe1;
    end;

    FmiRemoteUploadImageMenu := TdxBarSubItem.Create(FPopupMenu);
    with FmiRemoteUploadImageMenu do
    begin
      Caption := 'Remote upload to ...';
    end;
    with ItemLinks.Add do
    begin
      Item := FmiRemoteUploadImageMenu;
    end;

    FmiRemoteUploadAll := TdxBarButton.Create(FPopupMenu);
    with FmiRemoteUploadAll do
    begin
      Caption := 'Remote upload all';
      // ShortCut := Menus.ShortCut($41,[ssCtrl]);
      OnClick := FmiRemoteUploadAllImagesClick;
    end;
    with ItemLinks.Add do
    begin
      Item := FmiRemoteUploadAll;
    end;

    FmiLocalUploadImageMenu := TdxBarSubItem.Create(FPopupMenu);
    with FmiLocalUploadImageMenu do
    begin
      Caption := 'Local upload to ...';
    end;
    with ItemLinks.Add do
    begin
      Item := FmiLocalUploadImageMenu;
    end;

    FmiSaveAs := TdxBarButton.Create(FPopupMenu);
    with FmiSaveAs do
    begin
      Caption := 'Save as ...';
      OnClick := FmiSaveAsClick;
    end;
    with ItemLinks.Add do
    begin
      Item := FmiSaveAs;
    end;

    FmiVisitImage := TdxBarButton.Create(FPopupMenu);
    with FmiVisitImage do
    begin
      Caption := 'Visit';
      // ShortCut := Menus.ShortCut($41,[ssCtrl]);
      OnClick := FmiVisitImageClick;
    end;
    with ItemLinks.Add do
    begin
      Item := FmiVisitImage;
    end;
  end;

  with SettingsManager.Settings do
    with Plugins.ImageHoster do
      for I := 0 to Count - 1 do
        if TPlugInCollectionItem(Items[I]).Enabled then
          AddMirror(TPlugInCollectionItem(Items[I]).name);

  DefaultConfiguration;
end;

procedure TIPicture.AddProposedValue(const ASender: WideString; AValue: WideString; ATitle: WideString);
var
  ValueCount: Integer;
begin
  inherited AddProposedValue(ASender, AValue, ATitle);

  FPictureArrayLock.EnterWriteLock;
  try
    ValueCount := GetProposedValuesCount;
    SetLength(FPictureArray, ValueCount);
    with FPictureArray[ValueCount - 1] do
    begin
      Picture := GraphicAsVariant(nil);
      Downloaded := False;
    end;
  finally
    FPictureArrayLock.ExitWriteLock;
  end;

  CreateTask(
    { } procedure(const task: IOmniTask)
    { } begin

    { . } task.Invoke(
      { ... } procedure
      { ... } var
      { ..... } MemoryStream: TMemoryStream;
      { ... } begin
      { ..... } DownloadImage(AValue, MemoryStream);
      { ..... } sleep(100);
      { ..... } SetValuePicture(ValueCount - 1, MemoryStream);
      { ..... } MemoryStream.Free;
      { ... } end);

    { } end, 'TIPicture Image Download: ' + AValue).Run;
end;

procedure TIPicture.RemoteUpload(const AAfterCrawling: WordBool = False);

  function UploadAfterCrawling(AName: string): Boolean;
  var
    ImageHosterCollectionItem: TImageHosterCollectionItem;
  begin
    with SettingsManager.Settings.Plugins do
      ImageHosterCollectionItem := TImageHosterCollectionItem(FindPlugInCollectionItemFromCollection(AName, ImageHoster));
    Result := Assigned(ImageHosterCollectionItem) and ImageHosterCollectionItem.UploadAfterCrawling;
  end;

var
  I: Integer;
begin
  for I := 0 to MirrorCount - 1 do
    if not AAfterCrawling or (AAfterCrawling and UploadAfterCrawling(Mirror[I].Name)) then
      ControlController.TabSheetController.PageController.ImageHosterManager.AddRemoteUploadJob(Mirror[I]);
end;

destructor TIPicture.Destroy;
var
  I: Integer;
begin
  for I := MirrorCount - 1 downto 0 do
    RemoveMirror(I);

  for I := ProposedValuesCount - 1 downto 0 do
    GetValuePicture(I).Clear;

  FmiVisitImage.Free;
  FmiSaveAs.Free;
  FmiLocalUploadImageMenu.Free;
  FmiRemoteUploadAll.Free;
  FmiRemoteUploadImageMenu.Free;
  FmiSe1.Free;

  FPictureMirrorPanel.Free;

  FPictureArrayLock.EnterWriteLock;
  try
    SetLength(FPictureArray, 0);
  finally
    FPictureArrayLock.ExitWriteLock;
  end;

  FPictureMirrorList.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TITrailer'}

procedure TITrailer.FmiVisitImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(Value), nil, nil, SW_SHOW);
end;

procedure TITrailer.FPopupMenuPopup(Sender: TObject);
begin
  inherited FPopupMenuPopup(Sender);

  FmiVisitImage.Enabled := not(FComboBox.Text = '');
end;

constructor TITrailer.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 50;
  end;

  with FPopupMenu do
  begin
    FmiSe1 := TdxBarSeparator.Create(FPopupMenu);
    with FmiSe1 do
    begin
      ShowCaption := False;
    end;
    with ItemLinks.Add do
    begin
      // index := FmiSe2.index;
      Item := FmiSe1;
    end;

    FmiVisitImage := TdxBarButton.Create(FPopupMenu);
    with FmiVisitImage do
    begin
      Caption := 'Visit';
      // ShortCut := Menus.ShortCut($41,[ssCtrl]);
      OnClick := FmiVisitImageClick;
    end;
    with ItemLinks.Add do
    begin
      // index := FmiVisitImage.index;
      Item := FmiVisitImage;
    end;
  end;

  DefaultConfiguration;
end;

destructor TITrailer.Destroy;
begin
  FmiSe1.Free;
  FmiVisitImage.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIAudioBitrate'}

constructor TIAudioBitrate.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIAudioEncoder'}

constructor TIAudioEncoder.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIAudioSamplingRate'}

constructor TIAudioSamplingRate.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIAudioStream'}

constructor TIAudioStream.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIGenre'}

constructor TIGenre.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TILanguage'}

constructor TILanguage.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIVideoCodec'}

constructor TIVideoCodec.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIVideoStream'}

constructor TIVideoStream.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIVideoSystem'}

constructor TIVideoSystem.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TINFO'}

procedure TINFO.FMycxRichEditMouseEnter(Sender: TObject);
begin
  HintWindowClass := TMyMonospaceHint;
end;

procedure TINFO.FMycxRichEditMouseLeave(Sender: TObject);
begin
  HintWindowClass := THintWindow;
end;

procedure TINFO.FMycxRichEditDropFiles(var message: TMessage);
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
      with TStringList.Create do
        try
          LoadFromFile(FileName);
          FMycxRichEdit.Lines.Text := UnicodeString(Text);
        finally
          Free;
        end;

    StrDispose(FileName);
  end;

  DragFinish(message.WParam);
end;

procedure TINFO.ControlOnChange(Sender: TObject);
begin
  inherited ControlOnChange(Sender);
  FMycxRichEdit.Hint := TrimRight(FMycxRichEdit.Lines.Text);
end;

constructor TINFO.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  DragAcceptFiles(FMycxRichEdit.Handle, True);

  with FPanel.Constraints do
  begin
    MinHeight := 96;
    MinWidth := 150;
  end;

  with FMycxRichEdit do
  begin
    ShowHint := True;

    with Style do
    begin
      Font.name := 'Courier New';
      Font.Size := 8;
    end;

    PopupMenu := FPopupMenu;

    with Properties do
    begin
      MemoMode := True;
      Properties.OnChange := ControlOnChange;
    end;

    OnMouseEnter := FMycxRichEditMouseEnter;
    OnMouseLeave := FMycxRichEditMouseLeave;
    OnDropFiles := FMycxRichEditDropFiles;
  end;

  DefaultConfiguration;
end;

destructor TINFO.Destroy;
begin
  DragAcceptFiles(FMycxRichEdit.Handle, False);

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIDescription'}

procedure TIDescription.ControlOnChange(Sender: TObject);
begin
  inherited ControlOnChange(Sender);
  FMycxRichEdit.Hint := TrimRight(FMycxRichEdit.Lines.Text);
end;

constructor TIDescription.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 96;
    MinWidth := 150;
  end;

  with FMycxRichEdit do
  begin
    ShowHint := True;

    with Properties do
    begin
      ScrollBars := ssVertical;

      OnChange := ControlOnChange;
    end;
  end;

  DefaultConfiguration;
end;
{$ENDREGION}

end.
