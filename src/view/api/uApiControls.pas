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
  uConst, uAppInterface,
  // Api
  uApiConst,
  // Utils
  uImageUtils, uStringUtils;

const
  WM_CONTROL_VALUE_CHANGE = WM_USER + 175;

type
  TIBasic = class(TInterfacedObject, IBasic)
  private
    FHandle: HWND;
    FComponentController: IComponentController;
    FComponentID: TComponentID;
    FValue: string;
    FBufferedValues: TQueue<string>; // Omni Queue implementieren
    FValueArray: array of array of string;
    FTitleLabel, FHintLabel, FClearLabel: TcxLabel;
    FOleDrop: TOleDrop;
    procedure WndProc(var Msg: TMessage);
    // procedure WMControlValueChange(var Msg: TMessage); message WM_CONTROL_VALUE_CHANGE;
    function GetComponentController: IComponentController;
    procedure SetComponentController(const AComponentController: IComponentController);
    function GetTemplateTypeID: TTemplateTypeID;
    function GetComponentID: TComponentID;
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
    FValueLock, FValueArrayLock: TOmniMREW;
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
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); virtual;
    property ComponentController: IComponentController read GetComponentController write SetComponentController;
    procedure AddValue(AValue, ASenderName: WideString); overload; virtual;
    function GetValueName(AIndex: Integer): WideString;
    function GetValueContent(AIndex: Integer): WideString;
    function GetValueCount: Integer;
    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID;
    property ComponentID: TComponentID read GetComponentID;
    property Name: WideString read GetName write SetName;
    property Title: WideString read GetTitle write SetTitle;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: WideString read GetHint write SetHint;
    property Value: WideString read GetValue write SetValue;

    destructor Destroy; override;
  end;

  TIEdit = class(TIBasic, IEdit)
  protected
    FEdit: TcxTextEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    destructor Destroy; override;
  end;

  TIComboBox = class(TIBasic, IComboBox)
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
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIComboBoxList = class(TIComboBox, IComboBoxList)
  protected
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    procedure AddValue(AValue, ASenderName: WideString); override;
  end;

  TICheckComboBox = class(TIBasic, ICheckComboBox)
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
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIDateEdit = class(TIBasic, IDateEdit)
  protected
    FDateEdit: TcxDateEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    destructor Destroy; override;
  end;

  TIRichEdit = class(TIBasic, IRichEdit)
  protected
    FMycxRichEdit: TMycxRichEdit;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(AValue: WideString); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    destructor Destroy; override;
  end;

  TIReleaseName = class(TIEdit)
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIReleaseDate = class(TIDateEdit)
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TITags = class(TIEdit)

  end;

  TITitle = class(TIEdit)

  end;

  TIArtist = class(TIEdit)

  end;

  TINotes = class(TICheckComboBox)
  private
    procedure FCheckComboBoxPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIPassword = class(TIComboBox)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
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

  TIPicture = class(TIComboBox, IPictureEx)
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
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    procedure AddValue(AValue, ASenderName: WideString); override;
    property Mirror[AIndex: Integer]: IPictureMirror read GetMirror write SetMirror;
    property MirrorCount: Integer read GetMirrorCount;
    procedure RemoteUpload(const AAfterCrawling: WordBool = False);
    destructor Destroy; override;
  end;

  TITrailer = class(TIComboBox, ITrailer)
  private
    FTitleArrayLock: TOmniMREW;
    FTitleArray: array of string;
    FmiSe1: TdxBarSeparator;
    FmiVisitImage: TdxBarButton;
    procedure FmiVisitImageClick(Sender: TObject);
  protected
    procedure FPopupMenuPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
    procedure AddValue(AValue: WideString; ATitle: WideString; ASender: WideString); overload;
    function GetValueTitle(AIndex: Integer): WideString;
    procedure SetValueTitle(AIndex: Integer; ATitle: WideString);
    destructor Destroy; override;
  end;

  TISample = class(TIComboBox)

  end;

  TIAudioBitrate = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIAudioBitrateType = class(TIComboBoxList)

  end;

  TIAudioEncoder = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIAudioSamplingRate = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIAudioStream = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIGenre = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TILanguage = class(TICheckComboBox)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIRuntime = class(TIEdit)

  end;

  TIVideoCodec = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIVideoStream = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TIVideoSystem = class(TIComboBoxList)
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

  TINFO = class(TIRichEdit)
  private
    procedure FMycxRichEditMouseEnter(Sender: TObject);
    procedure FMycxRichEditMouseLeave(Sender: TObject);
    procedure FMycxRichEditDropFiles(var message: TMessage);
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;

    destructor Destroy; override;
  end;

  TIDescription = class(TIRichEdit)
  protected
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AComponentController: IComponentController; AComponentID: TComponentID); override;
  end;

implementation

uses
  uSettings,
  // Api
  uApiSettings, uApiPlugins;

resourcestring
  StrClear = 'Clear';
{$REGION 'TIBasic'}

procedure TIBasic.WndProc(var Msg: TMessage);
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

function TIBasic.GetComponentController;
begin
  Result := FComponentController;
end;

procedure TIBasic.SetComponentController(const AComponentController: IComponentController);
begin
  FComponentController := AComponentController;
end;

function TIBasic.GetTemplateTypeID;
begin
  Result := ComponentController.TemplateTypeID;
end;

function TIBasic.GetComponentID;
begin
  Result := FComponentID;
end;

function TIBasic.GetName;
begin
  Result := copy(FPanel.name, 2);
end;

function TIBasic.GetTitle;
begin
  Result := copy(FTitleLabel.Caption, 1, length(FTitleLabel.Caption) - 1);
end;

function TIBasic.GetLeft;
begin
  Result := FPanel.Left;
end;

procedure TIBasic.SetLeft(ALeft: Integer);
begin
  FPanel.Left := ALeft;
end;

function TIBasic.GetTop;
begin
  Result := FPanel.Top;
end;

procedure TIBasic.SetTop(ATop: Integer);
begin
  FPanel.Top := ATop;
end;

function TIBasic.GetWidth;
begin
  Result := FPanel.Width;
end;

procedure TIBasic.SetWidth(AWidth: Integer);
begin
  FPanel.Width := AWidth;
end;

function TIBasic.GetHeight;
begin
  Result := FPanel.Height;
end;

procedure TIBasic.SetHeight(AHeight: Integer);
begin
  FPanel.Height := AHeight;
end;

function TIBasic.GetHint;
begin
  Result := FHintLabel.Hint;
end;

procedure TIBasic.SetHint(AHint: WideString);
begin
  FHintLabel.Hint := AHint;
end;

function TIBasic.GetValue;
begin
  FValueLock.EnterReadLock;
  try
    Result := FValue;
  finally
    FValueLock.ExitReadLock;
  end;
end;

procedure TIBasic.SetValue(AValue: WideString);
begin
  FValueBufferLock.Acquire;
  try
    FBufferedValues.Enqueue(AValue);
  finally
    FValueBufferLock.Release;
  end;
  PostMessage(FHandle, WM_CONTROL_VALUE_CHANGE, 0, 0);
end;

procedure TIBasic.FPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  if Assigned(ComponentController.OnSpaceMouseDown) then
    ComponentController.OnSpaceMouseDown.Invoke(Self);
end;

procedure TIBasic.FPanelResize(Sender: TObject);
begin
  if Assigned(FClearLabel) then
    with FClearLabel do
    begin
      Left := FPanel.Width - FClearLabel.Width - 3;
    end;
end;

procedure TIBasic.FmiUndoClick(Sender: TObject);
begin
  GetControl.Undo;
end;

procedure TIBasic.FmiCutClick(Sender: TObject);
begin
  GetControl.CutToClipboard;
end;

procedure TIBasic.FmiCopyClick(Sender: TObject);
begin
  GetControl.CopyToClipboard;
end;

procedure TIBasic.FmiPasteClick(Sender: TObject);
begin
  GetControl.PasteFromClipboard;
end;

procedure TIBasic.FmiDeleteClick(Sender: TObject);
begin
  GetControl.ClearSelection;
end;

procedure TIBasic.FmiSelectAllClick(Sender: TObject);
begin
  GetControl.SelectAll;
end;

procedure TIBasic.FClearLabelClick(Sender: TObject);
begin
  GetControl.Clear;
end;

procedure TIBasic.FPopupMenuPopup(Sender: TObject);
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

procedure TIBasic.SetName(AName: WideString);
begin
  with FPanel do
  begin
    name := 'p' + AName;
    Caption := '';
  end;
  GetControl.Name := AName;
end;

procedure TIBasic.SetTitle(ATitle: WideString);
begin
  with FTitleLabel do
  begin
    Caption := ATitle + ':';

    Width := Canvas.TextWidth(Caption);

    FHintLabel.Left := FTitleLabel.Width + 6;
  end;
end;

function TIBasic.GetFocus;
begin
  Result := GetControl.Focused;
end;

procedure TIBasic.SetFocus(AFocus: Boolean);
begin
  if AFocus and GetControl.CanFocusEx then
    GetControl.SetFocus;
end;

procedure TIBasic.ControlOnDrop(AText: PWideChar);
begin
  Value := AText;
end;

procedure TIBasic.ControlOnChange(Sender: TObject);
begin
  FValueLock.EnterWriteLock;
  try
    FValue := GetControlValue;
  finally
    FValueLock.ExitWriteLock;
  end;

  if Assigned(ComponentController.OnControlChange) then
    ComponentController.OnControlChange.Invoke(Self);
end;

procedure TIBasic.ControlOnEnter(Sender: TObject);
begin
  if Assigned(ComponentController.OnControlEnter) then
    ComponentController.OnControlEnter.Invoke(Self);
  if Assigned(ComponentController.OnPopupMenuChange) then
    ComponentController.OnPopupMenuChange.Invoke(Integer(FPopupMenu.ItemLinks));
end;

procedure TIBasic.ControlOnExit(Sender: TObject);
begin
  if Assigned(ComponentController.OnPopupMenuChange) then
    ComponentController.OnPopupMenuChange.Invoke(Integer(nil));
  if Assigned(ComponentController.OnControlExit) then
    ComponentController.OnControlExit.Invoke(Self);
end;

procedure TIBasic.DefaultConfiguration;
begin
  with SettingsManager.Settings.Controls.Controls[TemplateTypeID, ComponentID] do
  begin
    SetTitle(Title);
    SetHint(HelpText);
    SetValue(Value);
  end;
end;

constructor TIBasic.Create;
begin
  FHandle := AllocateHWnd(WndProc);

  FBufferedValues := TQueue<string>.Create;

  FComponentController := AComponentController;
  FComponentID := AComponentID;

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

    Show;
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

    Show;
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

    Show;
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

procedure TIBasic.AddValue(AValue, ASenderName: WideString);
var
  _NewValueIndex: Integer;
begin
  FValueArrayLock.EnterWriteLock;
  try
    SetLength(FValueArray, length(FValueArray) + 1, 2);
  finally
    FValueArrayLock.ExitWriteLock;
  end;

  _NewValueIndex := GetValueCount - 1;

  FValueArrayLock.EnterWriteLock;
  try
    FValueArray[_NewValueIndex][0] := ASenderName;
    FValueArray[_NewValueIndex][1] := AValue;
  finally
    FValueArrayLock.ExitWriteLock;
  end;
end;

function TIBasic.GetValueName(AIndex: Integer): WideString;
begin
  FValueArrayLock.EnterReadLock;
  try
    Result := FValueArray[AIndex][0];
  finally
    FValueArrayLock.ExitReadLock;
  end;
end;

function TIBasic.GetValueContent(AIndex: Integer): WideString;
begin
  FValueArrayLock.EnterReadLock;
  try
    Result := FValueArray[AIndex][1];
  finally
    FValueArrayLock.ExitReadLock;
  end;
end;

function TIBasic.GetValueCount: Integer;
begin
  FValueArrayLock.EnterReadLock;
  try
    Result := length(FValueArray);
  finally
    FValueArrayLock.ExitReadLock;
  end;
end;

destructor TIBasic.Destroy;

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

var
  I: Integer;
begin
  FOleDrop.Free;

  FPopupMenu.Free;

  FClearLabel.Free;
  FHintLabel.Free;
  FTitleLabel.Free;

  FPanel.Free;

  for I := GetValueCount - 1 downto 0 do
    SetLength(FValueArray[I], 0);
  SetLength(FValueArray, 0);

  FComponentController := nil;

  FBufferedValues.Free;

  DeallocateHWnd(FHandle);
  FHandle := 0;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIEdit'}

function TIEdit.GetControl;
begin
  Result := FEdit;
end;

function TIEdit.GetControlValue;
begin
  Result := FEdit.Text;
end;

procedure TIEdit.SetControlValue(AValue: WideString);
begin
  FEdit.Text := AValue;
end;

constructor TIEdit.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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

destructor TIEdit.Destroy;
begin
  FEdit.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIComboBox'}

function TIComboBox.GetList;
begin
  Result := FComboBox.Properties.Items.Text;
end;

function TIComboBox.GetDropDownRows;
begin
  Result := FComboBox.Properties.DropDownRows;
end;

procedure TIComboBox.SetDropDownRows(ADropDownRows: Integer);
begin
  FComboBox.Properties.DropDownRows := ADropDownRows;
end;

procedure TIComboBox.SetList(AList: WideString);
begin
  FComboBox.Properties.Items.Text := AList;
end;

function TIComboBox.GetControl;
begin
  Result := FComboBox;
end;

function TIComboBox.GetControlValue;
begin
  Result := FComboBox.Text;
end;

procedure TIComboBox.SetControlValue(AValue: WideString);
begin
  FComboBox.Text := AValue;
end;

procedure TIComboBox.DefaultConfiguration;
begin
  inherited DefaultConfiguration;
  with SettingsManager.Settings.Controls.Controls[TemplateTypeID, ComponentID] do
    SetList(GetItems);
end;

constructor TIComboBox.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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

destructor TIComboBox.Destroy;
begin
  FComboBox.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIComboBoxList'}

procedure TIComboBoxList.SetControlValue(AValue: WideString);
begin
  inherited SetControlValue(SettingsManager.Settings.Controls.GetCustomisedComponentValue(ComponentID, TemplateTypeID, AValue));
end;

constructor TIComboBoxList.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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

procedure TIComboBoxList.AddValue(AValue, ASenderName: WideString);
begin
  inherited AddValue(SettingsManager.Settings.Controls.GetCustomisedComponentValue(ComponentID, TemplateTypeID, AValue), ASenderName);
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TICheckComboBox'}

function TICheckComboBox.InternalIndexOf(AStr: string): Integer;
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

function TICheckComboBox.GetDropDownRows;
begin
  Result := FCheckComboBox.Properties.DropDownRows;
end;

procedure TICheckComboBox.SetDropDownRows(ADropDownRows: Integer);
begin
  FCheckComboBox.Properties.DropDownRows := ADropDownRows;
end;

function TICheckComboBox.GetList;
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

procedure TICheckComboBox.SetList(AList: WideString);
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

function TICheckComboBox.GetControl;
begin
  Result := FCheckComboBox;
end;

function TICheckComboBox.GetControlValue;
begin
  Result := FCheckComboBox.Text;
end;

procedure TICheckComboBox.SetControlValue(AValue: WideString);
var
  I, findex: Integer;
begin
  with SplittString(';', AValue) do
    try
      for I := 0 to Count - 1 do
      begin
        findex := InternalIndexOf(SettingsManager.Settings.Controls.GetCustomisedComponentValue(ComponentID, TemplateTypeID, PChar(Strings[I])));

        if findex <> -1 then
          FCheckComboBox.States[findex] := cbsChecked;
      end;
    finally
      Free;
    end;
end;

procedure TICheckComboBox.DefaultConfiguration;
begin
  inherited DefaultConfiguration;
  with SettingsManager.Settings.Controls.Controls[TemplateTypeID, ComponentID] do
    SetList(GetItems);
end;

constructor TICheckComboBox.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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

destructor TICheckComboBox.Destroy;
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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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

  if Assigned(ComponentController.OnReleaseNameChange) then
    ComponentController.OnReleaseNameChange.Invoke(FEdit.Text);
end;

constructor TIReleaseName.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  FPicture.ComponentController.TabSheetController.PageController.ImageHosterManager.AddLocalUploadJob(Self, ALocalPath);
end;

procedure TPictureMirror.RemoteUpload;
begin
  FPicture.ComponentController.TabSheetController.PageController.ImageHosterManager.AddRemoteUploadJob(Self);
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
    with ComponentController do
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
  inherited Create(AOwner, AComponentController, AComponentID);

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

procedure TIPicture.AddValue(AValue, ASenderName: WideString);
var
  ValueCount: Integer;
begin
  inherited AddValue(AValue, ASenderName);

  FPictureArrayLock.EnterWriteLock;
  try
    ValueCount := GetValueCount;
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
      ComponentController.TabSheetController.PageController.ImageHosterManager.AddRemoteUploadJob(Mirror[I]);
end;

destructor TIPicture.Destroy;
var
  I: Integer;
begin
  for I := MirrorCount - 1 downto 0 do
    RemoveMirror(I);

  for I := GetValueCount - 1 downto 0 do
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
  inherited Create(AOwner, AComponentController, AComponentID);

  SetLength(FTitleArray, 0);

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

procedure TITrailer.AddValue(AValue: WideString; ATitle: WideString; ASender: WideString);
begin
  inherited AddValue(AValue, ASender);

  FTitleArrayLock.EnterWriteLock;
  try
    // vorher GetValueCount +1 ???
    SetLength(FTitleArray, GetValueCount);
  finally
    FTitleArrayLock.ExitWriteLock;
  end;

  SetValueTitle(GetValueCount - 1, ATitle);
end;

function TITrailer.GetValueTitle(AIndex: Integer): WideString;
begin
  FTitleArrayLock.EnterReadLock;
  try
    Result := FTitleArray[AIndex];
  finally
    FTitleArrayLock.ExitReadLock;
  end;
end;

procedure TITrailer.SetValueTitle(AIndex: Integer; ATitle: WideString);
begin
  FTitleArrayLock.EnterReadLock;
  try
    FTitleArray[AIndex] := ATitle;
  finally
    FTitleArrayLock.ExitReadLock;
  end;
end;

destructor TITrailer.Destroy;
begin
  FmiSe1.Free;
  FmiVisitImage.Free;

  FTitleArrayLock.EnterWriteLock;
  try
    SetLength(FTitleArray, 0);
  finally
    FTitleArrayLock.ExitWriteLock;
  end;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIAudioBitrate'}

constructor TIAudioBitrate.Create;
begin
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
  inherited Create(AOwner, AComponentController, AComponentID);

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
