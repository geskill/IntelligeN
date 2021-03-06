unit uApiControls;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, StdCtrls, ExtCtrls, Variants,
  ShellAPI, Clipbrd, Dialogs, jpeg,
  // Spring Framework
  Spring.Collections.Lists,
  // PopupMenu Mod
  uMydxBarPopupMenu, // uMyPopupMenu,
  // Dev Express
  cxLookAndFeelPainters, cxLabel, cxEdit, cxTextEdit, cxDropDownEdit, cxCheckBox, cxCheckComboBox, cxCalendar, dxBar,
  // Dev Express Mod
  uMycxRichEdit, uMyMonospaceHint,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl, OtlSync, OtlTask,
  // OLEDrop
  OLEDrop,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiControlsBase, uApiHTTP,
  // Utils
  uImageUtils, uStringUtils;

type
  TIControlBasic = class(TIControlBase, IControlBasic)
  private
    // GUI
    FTitleLabel, FClearLabel: TcxLabel;
    FOleDrop: TOleDrop;
    // GUI
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
    // GUI
    FPanel: TPanel;
    FPopupMenu: TMydxBarPopupMenu;
    // GUI
    procedure FPopupMenuPopup(Sender: TObject); virtual;
    // Internal
    function GetControl: TcxCustomTextEdit; virtual; abstract;
    function GetControlValue: WideString; virtual; abstract;
    procedure SetControlValue(const AValue: WideString); virtual; abstract;
    function GetControlName: WideString;
    procedure SetControlName(const AName: WideString); virtual;
    function GetControlTitle: WideString;
    procedure SetControlTitle(const ATitle: WideString); virtual;
    function GetControlLeft: Integer;
    procedure SetControlLeft(const ALeft: Integer);
    function GetControlTop: Integer;
    procedure SetControlTop(const ATop: Integer);
    function GetControlWidth: Integer;
    procedure SetControlWidth(const AWidth: Integer);
    function GetControlHeight: Integer;
    procedure SetControlHeight(const AHeight: Integer);
    function GetControlHint: WideString;
    procedure SetControlHint(const AHint: WideString);
    function GetControlFocus: WordBool; virtual;
    procedure SetControlFocus(const AFocus: WordBool); virtual;
    procedure ControlOnDrop(AText: PWideChar);
    procedure ControlOnChange(ASender: TObject); virtual;
    procedure ControlOnEnter(ASender: TObject); virtual;
    procedure ControlOnExit(ASender: TObject); virtual;
  private
    // Logic
    FControlController: IControlController;
  protected
    // Base
    function GetValue: WideString; override;

    // Additional
    function GetControlController: IControlController;
    procedure SetControlController(const AControlController: IControlController);

    function GetTypeID: TTypeID;

    procedure SetValue(const AValue: WideString); reintroduce; safecall;

    procedure LoadDefaultConfiguration; virtual;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AControlID: TControlID); virtual;
    destructor Destroy; override;

    // Internal
    property Name: WideString read GetControlName write SetControlName;
    property Title: WideString read GetControlTitle write SetControlTitle;
    property Left: Integer read GetControlLeft write SetControlLeft;
    property Top: Integer read GetControlTop write SetControlTop;
    property Width: Integer read GetControlWidth write SetControlWidth;
    property Height: Integer read GetControlHeight write SetControlHeight;
    property Hint: WideString read GetControlHint write SetControlHint;
    property Focus: WordBool read GetControlFocus write SetControlFocus;

    // Base
    property Value: WideString read GetValue { . } write SetValue;

    // Additional
    property ControlController: IControlController read GetControlController write SetControlController;

    property TypeID: TTypeID read GetTypeID;

    // Cloning
    function CloneInstance(): IControlBase;
  end;

  TIControlEdit = class(TIControlBasic, IControlEdit)
  protected
    // GUI
    FEdit: TcxTextEdit;
    // Internal
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(const AValue: WideString); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIControlComboBox = class(TIControlBasic, IControlComboBox)
  private
    function GetList: WideString;
  protected
    // GUI
    FComboBox: TcxComboBox;
    // Internal
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    procedure SetList(const AList: WideString); virtual;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(const AValue: WideString); override;
    procedure LoadDefaultConfiguration; override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIControlComboBoxList = class(TIControlComboBox, IControlComboBoxList)
  private
    function GetCustomisedComponentValue(const AValue: string): string;
  protected
    // Internal
    procedure SetControlValue(const AValue: WideString); override;
    function HasControlValue(const AValue: WideString): WordBool;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIControlCheckComboBox = class(TIControlBasic, IControlCheckComboBox)
  private
    function GetCustomisedComponentValue(const AValue: string): string;
    function InternalIndexOf(const AStr: string): Integer;
    procedure CheckAll;
    procedure UncheckAll;
  protected
    // GUI
    FCheckComboBox: TcxCheckComboBox;
    // Internal
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    function GetList: WideString;
    procedure SetList(const AList: WideString); virtual;
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(const AValue: WideString); override;
    function HasControlValue(const AValue: WideString): WordBool;
    procedure LoadDefaultConfiguration; override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
    destructor Destroy; override;
  end;

  TIControlDateEdit = class(TIControlBasic, IControlDateEdit)
  protected
    // GUI
    FDateEdit: TcxDateEdit;
    // Internal
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(const AValue: WideString); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIControlRichEdit = class(TIControlBasic, IControlRichEdit)
  protected
    // GUI
    FMycxRichEdit: TMycxRichEdit;
    // Internal
    function GetControl: TcxCustomTextEdit; override;
    function GetControlValue: WideString; override;
    procedure SetControlValue(const AValue: WideString); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TIReleaseName = class(TIControlEdit)
  protected
    // Internal
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIReleaseDate = class(TIControlDateEdit)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TITags = class(TIControlEdit)

  end;

  TITitle = class(TIControlEdit)

  end;

  TICreator = class(TIControlComboBox)

  end;

  TIDirector = class(TIControlComboBox)

  end;

  TIPublisher = class(TIControlComboBox)

  end;

  TINotes = class(TIControlCheckComboBox)
  private
    procedure FCheckComboBoxPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIPassword = class(TIControlComboBox)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TPictureMirrorData = class(TIControlData, IPictureMirrorData)
  private
    FName: WideString;
    FOriginalValue: WideString;
    FErrorMsg: WideString;
  protected
    function GetName: WideString; virtual; safecall;
    function GetOriginalValue: WideString; virtual; safecall;
    function GetErrorMsg: WideString; virtual; safecall;
    procedure SetErrorMsg(const AErrorMsg: WideString); virtual; safecall;
  public
    constructor Create(const AName, AOriginalValue: WideString; const AValue: WideString = ''); reintroduce;
    constructor Clone(const APictureMirrorData: IPictureMirrorData);
    destructor Destroy; override;

    property Name: WideString read GetName;
    property OriginalValue: WideString read GetOriginalValue;
    property Value: WideString read GetValue write SetValue;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;

    // Cloning
    function CloneInstance(): IControlData;
  end;

  TPictureMirror = class(TPictureMirrorData, IPictureMirror)
  private
    FPicture: IPicture;
    FImage: TImage;
  protected
    procedure FOnClickIconClick(Sender: TObject);
    function GetPicture: IPicture;
    procedure SetPicture(APicture: IPicture);
    function GetOriginalValue: WideString; override; safecall;
    procedure SetValue(const AValue: WideString); override; safecall;
    procedure SetErrorMsg(const AErrorMsg: WideString); override; safecall;
    procedure SetHint;
    procedure ResetErrorMsgAndValue();
    procedure DrawIconRectangle();
    procedure DrawStatusRectangle();
  public
    constructor Create(const AOwner: TWinControl; APicture: IPicture; const AImageHosterName: WideString); reintroduce;
    destructor Destroy; override;

    property Picture: IPicture read GetPicture write SetPicture;
    property Name: WideString read GetName;
    property OriginalValue: WideString read GetOriginalValue;
    property Value: WideString read GetValue write SetValue;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;

    procedure LocalUpload(const ALocalPath: WideString);
    procedure RemoteUpload;
  end;

  TIPicture = class(TIControlComboBox, IPicture)
  private
    // GUI
    FPictureMirrorPanel: TFlowPanel;
    FmiSe1: TdxBarSeparator;
    FmiRemoteUploadImageMenu, FmiLocalUploadImageMenu: TdxBarSubItem;
    FmiRemoteUploadAll: TdxBarButton;
    FmiSaveAs: TdxBarButton;
    FmiVisitImage: TdxBarButton;
    // GUI
    procedure FmiRemoteUploadImageClick(Sender: TObject);
    procedure FmiRemoteUploadAllImagesClick(Sender: TObject);
    procedure FmiLocalUploadImageClick(Sender: TObject);
    procedure FmiSaveAsClick(Sender: TObject);
    procedure FmiVisitImageClick(Sender: TObject);
  protected
    // GUI
    procedure FPopupMenuPopup(Sender: TObject); override;
    // Internal
    procedure SetControlTitle(const ATitle: WideString); override;
  private
    // Logic
    FPictureArrayLock: TOmniMREW;
    FPictureArray: array of TPictureInfo;
    FMirrorList: TInterfaceList<IPictureMirror>;
    // Logic
    function BitmapAsThumbnail(const Graphic: TGraphic): TBitmap;
    function GraphicAsVariant(AGraphic: TGraphic): Variant;
    procedure SaveImage; overload;
    procedure SaveImage(AMemoryStream: TMemoryStream); overload;

    procedure HandleProposedValue(AIndex: Integer; const AValue: WideString);
    procedure SetValuePictureFromDownload(AIndex: Integer; AMemoryStream: TMemoryStream); overload;
  protected
    function GetValuePicture(AIndex: Integer): TPictureInfo; safecall;
    procedure SetValuePicture(AIndex: Integer; APictureInfo: TPictureInfo); safecall;

    function GetMirror(const IndexOrName: OleVariant): IPictureMirror; safecall;
    function GetMirrorCount: Integer; safecall;

    function AddMirror(const AName: WideString): Integer; safecall;
    function RemoveMirror(AIndex: Integer): WordBool; safecall;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;

    // Base
    procedure AddProposedValue(const ASender: WideString; const AValue: WideString; const ATitle: WideString = ''); override; safecall;

    // Additional
    procedure RemoteUpload(const AAfterCrawling: WordBool = False); safecall;

    function FindMirrorIndex(const AHoster: WideString): Integer;
    function FindMirror(const AHoster: WideString): IPictureMirror; safecall;

    property Mirror[const IndexOrName: OleVariant]: IPictureMirror read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
  end;

  TITrailer = class(TIControlComboBox, ITrailer)
  private
    // GUI
    FmiSe1: TdxBarSeparator;
    FmiVisitImage: TdxBarButton;
    procedure FmiVisitImageClick(Sender: TObject);
  protected
    // Internal
    procedure FPopupMenuPopup(Sender: TObject); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
    destructor Destroy; override;
  end;

  TISample = class(TIControlComboBox)

  end;

  TIAudioBitrate = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioBitrateType = class(TIControlComboBoxList)

  end;

  TIAudioEncoder = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioSamplingRate = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIAudioStream = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIGenre = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TILanguage = class(TIControlCheckComboBox)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TISubtitle = class(TIControlCheckComboBox)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIRuntime = class(TIControlEdit)

  end;

  TIVersion = class(TIControlEdit)

  end;

  TIVideoCodec = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIVideoStream = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIVideoSystem = class(TIControlComboBoxList)
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

  TIWebsite = class(TIControlEdit)

  end;

  TINFO = class(TIControlRichEdit)
  private
    // GUI
    procedure FMycxRichEditMouseEnter(Sender: TObject);
    procedure FMycxRichEditMouseLeave(Sender: TObject);
    procedure FMycxRichEditDropFiles(var message: TMessage);
  protected
    // Internal
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;

    destructor Destroy; override;
  end;

  TIDescription = class(TIControlRichEdit)
  protected
    // Internal
    procedure ControlOnChange(Sender: TObject); override;
  public
    constructor Create(const AOwner: TWinControl; const AControlController: IControlController; AComponentID: TControlID); override;
  end;

implementation

uses
  uSettings,
  // Api
  uApiSettings;
{ . }
{$REGION 'TIControlBasic'}

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

function TIControlBasic.GetControlName;
begin
  Result := copy(FPanel.Name, 2);
end;

procedure TIControlBasic.SetControlName(const AName: WideString);
begin
  with FPanel do
  begin
    Name := 'p' + AName;
    Caption := '';
  end;
  GetControl.Name := AName;
end;

function TIControlBasic.GetControlTitle;
begin
  Result := copy(FTitleLabel.Caption, 1, length(FTitleLabel.Caption) - 1);
end;

procedure TIControlBasic.SetControlTitle(const ATitle: WideString);
begin
  with FTitleLabel do
  begin
    Caption := ATitle + ':';

    Width := Canvas.TextWidth(Caption);
  end;
end;

function TIControlBasic.GetControlLeft;
begin
  Result := FPanel.Left;
end;

procedure TIControlBasic.SetControlLeft(const ALeft: Integer);
begin
  FPanel.Left := ALeft;
end;

function TIControlBasic.GetControlTop;
begin
  Result := FPanel.Top;
end;

procedure TIControlBasic.SetControlTop(const ATop: Integer);
begin
  FPanel.Top := ATop;
end;

function TIControlBasic.GetControlWidth;
begin
  Result := FPanel.Width;
end;

procedure TIControlBasic.SetControlWidth(const AWidth: Integer);
begin
  FPanel.Width := AWidth;
end;

function TIControlBasic.GetControlHeight;
begin
  Result := FPanel.Height;
end;

procedure TIControlBasic.SetControlHeight(const AHeight: Integer);
begin
  FPanel.Height := AHeight;
end;

function TIControlBasic.GetControlHint;
begin
  Result := FTitleLabel.Hint;
end;

procedure TIControlBasic.SetControlHint(const AHint: WideString);
begin
  FTitleLabel.Hint := AHint;
end;

function TIControlBasic.GetControlFocus;
begin
  Result := GetControl.Focused;
end;

procedure TIControlBasic.SetControlFocus(const AFocus: WordBool);
begin
  if AFocus and GetControl.CanFocusEx then
    GetControl.SetFocus;
end;

procedure TIControlBasic.ControlOnDrop(AText: PWideChar);
begin
  SetControlValue(AText);
end;

procedure TIControlBasic.ControlOnChange(ASender: TObject);
begin
  SetControlValue(GetControlValue);

  if Assigned(ControlController.OnControlChange) then
    ControlController.OnControlChange.Invoke(Self);
end;

procedure TIControlBasic.ControlOnEnter(ASender: TObject);
begin
  if Assigned(ControlController.OnControlEnter) then
    ControlController.OnControlEnter.Invoke(Self);
  if Assigned(ControlController.OnPopupMenuChange) then
    ControlController.OnPopupMenuChange.Invoke(Integer(FPopupMenu.ItemLinks));
end;

procedure TIControlBasic.ControlOnExit(ASender: TObject);
begin
  if Assigned(ControlController.OnPopupMenuChange) then
    ControlController.OnPopupMenuChange.Invoke(Integer(nil));
  if Assigned(ControlController.OnControlExit) then
    ControlController.OnControlExit.Invoke(Self);
end;

function TIControlBasic.GetControlController;
begin
  Result := FControlController;
end;

procedure TIControlBasic.SetControlController(const AControlController: IControlController);
begin
  FControlController := AControlController;
end;

function TIControlBasic.GetTypeID;
begin
  Result := ControlController.TypeID;
end;

function TIControlBasic.GetValue;
begin
  Result := GetControlValue;
end;

procedure TIControlBasic.SetValue(const AValue: WideString);
begin
  SetControlValue(AValue);
end;

procedure TIControlBasic.LoadDefaultConfiguration;
begin
  with SettingsManager.Settings.Controls.Controls[TypeID, ControlID] do
  begin
    SetControlTitle(Title);
    SetControlHint(HelpText);
    SetControlValue(Value);
  end;
end;

constructor TIControlBasic.Create;
begin
  inherited Create(AControlID);

  FControlController := AControlController;

  FPanel := TPanel.Create(AOwner);
  with FPanel do
  begin
    ParentBackground := False;

    Parent := AOwner;

    BevelOuter := bvNone;
    Caption := '';
    Color := clWhite;

    with Constraints do
    begin
      MinHeight := 13;
      MinWidth := 10;
    end;

    OnMouseDown := FPanelMouseDown;
    OnResize := FPanelResize;
  end;

  FTitleLabel := TcxLabel.Create(FPanel);
  with FTitleLabel do
  begin
    Parent := FPanel;

    Top := 0;
    Left := 0;

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
    { TODO 3 : In den Einstellungen Farbe w�hlbar }
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
begin
  FOleDrop.Free;

  FPopupMenu.Free;

  FClearLabel.Free;
  FTitleLabel.Free;

  FPanel.Free;

  FControlController := nil;

  inherited Destroy;
end;

function TIControlBasic.CloneInstance(): IControlBase;
var
  LControlBase: IControlBase;
begin
  LControlBase := TIControlBase.Clone(Self);

  Result := LControlBase;
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

procedure TIControlEdit.SetControlValue(const AValue: WideString);
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

  FEdit := TcxTextEdit.Create(FPanel);
  with FEdit do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    ControlStyle := ControlStyle - [csSetCaption];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  LoadDefaultConfiguration;
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

procedure TIControlComboBox.SetList(const AList: WideString);
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

procedure TIControlComboBox.SetControlValue(const AValue: WideString);
begin
  FComboBox.Text := AValue;
end;

procedure TIControlComboBox.LoadDefaultConfiguration;
begin
  inherited LoadDefaultConfiguration;
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

  FComboBox := TcxComboBox.Create(FPanel);
  with FComboBox do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    ControlStyle := ControlStyle - [csSetCaption];

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

  LoadDefaultConfiguration;
end;

destructor TIControlComboBox.Destroy;
begin
  FComboBox.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlComboBoxList'}

function TIControlComboBoxList.GetCustomisedComponentValue(const AValue: string): string;
begin
  Result := SettingsManager.Settings.Controls.GetCustomisedComponentValue(ControlID, TypeID, AValue);
end;

procedure TIControlComboBoxList.SetControlValue(const AValue: WideString);
begin
  inherited SetControlValue(GetCustomisedComponentValue(AValue));
end;

function TIControlComboBoxList.HasControlValue(const AValue: WideString): WordBool;
begin
  Result := not SameStr('', GetCustomisedComponentValue(AValue));
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

  LoadDefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlCheckComboBox'}

function TIControlCheckComboBox.GetCustomisedComponentValue(const AValue: string): string;
begin
  Result := SettingsManager.Settings.Controls.GetCustomisedComponentValue(ControlID, TypeID, AValue);
end;

function TIControlCheckComboBox.InternalIndexOf(const AStr: string): Integer;
var
  LIndex, LCount: Integer;
  LFound: Boolean;
begin
  Result := -1;

  LIndex := 0;
  LFound := False;
  LCount := FCheckComboBox.Properties.Items.Count;

  while (LIndex < LCount) and not LFound do
  begin
    LFound := SameStr(FCheckComboBox.Properties.Items.Items[LIndex].Description, AStr);
    if not LFound then
      Inc(LIndex);
  end;

  if LFound then
    Result := LIndex;
end;

procedure TIControlCheckComboBox.CheckAll;
var
  LItemIndex: Integer;
begin
  with FCheckComboBox do
    for LItemIndex := 0 to Properties.Items.Count - 1 do
      States[LItemIndex] := cbsChecked;
end;

procedure TIControlCheckComboBox.UncheckAll;
var
  LItemIndex: Integer;
begin
  with FCheckComboBox do
    for LItemIndex := 0 to Properties.Items.Count - 1 do
      States[LItemIndex] := cbsUnchecked;
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

procedure TIControlCheckComboBox.SetList(const AList: WideString);
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

procedure TIControlCheckComboBox.SetControlValue(const AValue: WideString);
var
  LIndex, LValueIndex: Integer;
begin
  FCheckComboBox.Properties.OnChange := nil;
  UncheckAll;

  with SplittString(';', AValue) do
    try
      for LIndex := 0 to Count - 1 do
      begin
        LValueIndex := InternalIndexOf(GetCustomisedComponentValue(Strings[LIndex]));

        if LValueIndex <> -1 then
          FCheckComboBox.States[LValueIndex] := cbsChecked;
      end;
    finally
      Free;
    end;
  FCheckComboBox.Properties.OnChange := ControlOnChange;
end;

function TIControlCheckComboBox.HasControlValue(const AValue: WideString): WordBool;
var
  LIndex, LValueIndex: Integer;
begin
  Result := False;

  with SplittString(';', AValue) do
    try
      for LIndex := 0 to Count - 1 do
      begin
        LValueIndex := InternalIndexOf(GetCustomisedComponentValue(Strings[LIndex]));

        if LValueIndex <> -1 then
          Exit(True);
      end;
    finally
      Free;
    end;
end;

procedure TIControlCheckComboBox.LoadDefaultConfiguration;
begin
  inherited LoadDefaultConfiguration;
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

  FCheckComboBox := TcxCheckComboBox.Create(FPanel);
  with FCheckComboBox do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    ControlStyle := ControlStyle - [csSetCaption];

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

  LoadDefaultConfiguration;
end;

destructor TIControlCheckComboBox.Destroy;
begin
  FCheckComboBox.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlDateEdit'}

function TIControlDateEdit.GetControl;
begin
  Result := FDateEdit;
end;

function TIControlDateEdit.GetControlValue;
begin
  Result := FDateEdit.Text;
end;

procedure TIControlDateEdit.SetControlValue(const AValue: WideString);
var
  FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FormatSettings);

  FDateEdit.Date := StrToDateDef(AValue, Date, FormatSettings);
end;

constructor TIControlDateEdit.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 75;
  end;

  FDateEdit := TcxDateEdit.Create(FPanel);
  with FDateEdit do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight];

    ControlStyle := ControlStyle - [csSetCaption];

    StyleFocused.BorderStyle := ebsThick;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  LoadDefaultConfiguration;
end;

destructor TIControlDateEdit.Destroy;
begin
  FDateEdit.Free;

  inherited Destroy;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIControlRichEdit'}

function TIControlRichEdit.GetControl;
begin
  Result := FMycxRichEdit;
end;

function TIControlRichEdit.GetControlValue;
begin
  if SettingsManager.Settings.Controls.IRichEditWrapText then
    Result := FMycxRichEdit.Lines.Text
  else
    Result := FMycxRichEdit.Text;
end;

procedure TIControlRichEdit.SetControlValue(const AValue: WideString);
begin
  FMycxRichEdit.Lines.Text := AValue;
end;

constructor TIControlRichEdit.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 96;
    MinWidth := 150;
  end;

  FMycxRichEdit := TMycxRichEdit.Create(FPanel);
  with FMycxRichEdit do
  begin
    Parent := FPanel;
    Top := 16;
    Left := 0;
    Height := FPanel.Height - Top;
    Width := FPanel.Width;

    Anchors := [akLeft, akTop, akRight, akBottom];

    ControlStyle := ControlStyle - [csSetCaption];

    StyleFocused.BorderStyle := ebsThick;

    with Properties do
      ScrollBars := ssBoth;

    PopupMenu := FPopupMenu;

    Properties.OnChange := ControlOnChange;
    OnEnter := ControlOnEnter;
    OnExit := ControlOnExit;
  end;

  LoadDefaultConfiguration;
end;

destructor TIControlRichEdit.Destroy;
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

  LoadDefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIReleaseDate'}

constructor TIReleaseDate.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  FDateEdit.Date := Date;

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIPassword'}

constructor TIPassword.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
    MinWidth := 75;

  LoadDefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TPictureMirrorData'}

function TPictureMirrorData.GetName: WideString;
begin
  Result := FName;
end;

function TPictureMirrorData.GetOriginalValue: WideString;
begin
  Result := FOriginalValue;
end;

function TPictureMirrorData.GetErrorMsg: WideString;
begin
  Result := FErrorMsg;
end;

procedure TPictureMirrorData.SetErrorMsg(const AErrorMsg: WideString);
begin
  FErrorMsg := AErrorMsg;
end;

constructor TPictureMirrorData.Create(const AName, AOriginalValue: WideString; const AValue: WideString = '');
begin
  inherited Create(cPicture, AValue);
  FName := AName;
  FOriginalValue := AOriginalValue;
  FErrorMsg := '';
end;

constructor TPictureMirrorData.Clone(const APictureMirrorData: IPictureMirrorData);
begin
  Create(APictureMirrorData.Name, APictureMirrorData.OriginalValue, APictureMirrorData.Value);
  FErrorMsg := APictureMirrorData.ErrorMsg;
end;

destructor TPictureMirrorData.Destroy;
begin
  inherited Destroy;
end;

function TPictureMirrorData.CloneInstance;
begin
  Result := TPictureMirrorData.Clone(Self);
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TPictureMirror'}

procedure TPictureMirror.FOnClickIconClick(Sender: TObject);
begin
  if not SameStr('', Value) then
    ShellExecute(0, 'open', PChar(Value), nil, nil, SW_SHOW);
end;

function TPictureMirror.GetPicture: IPicture;
begin
  Result := FPicture;
end;

procedure TPictureMirror.SetPicture(APicture: IPicture);
begin
  FPicture := APicture;
end;

function TPictureMirror.GetOriginalValue: WideString;
begin
  Result := FPicture.Value;
end;

procedure TPictureMirror.SetValue(const AValue: WideString);
begin
  inherited SetValue(AValue);

  with Picture.ControlController do
    if Assigned(OnControlChange) then
      OnControlChange.Invoke(Picture);

  SetHint;
end;

procedure TPictureMirror.SetErrorMsg(const AErrorMsg: WideString);
begin
  inherited SetErrorMsg(AErrorMsg);
  SetHint;
end;

procedure TPictureMirror.SetHint;
begin
  if SameStr('', ErrorMsg) and SameStr('', Value) then
    FImage.Hint := ''
  else if not SameStr('', ErrorMsg) then
    FImage.Hint := Name + ' Error:' + sLineBreak + ErrorMsg
  else
    FImage.Hint := Name + ': ' + Value;
  DrawStatusRectangle();
end;

procedure TPictureMirror.ResetErrorMsgAndValue;
begin
  ErrorMsg := '';
  Value := '';
  DrawStatusRectangle();
end;

procedure TPictureMirror.DrawIconRectangle;
begin
  with SettingsManager.Settings.Plugins do
    FImage.Picture.Bitmap.Assign(TImageHosterCollectionItem(FindPlugInCollectionItemFromCollection(Name, ImageHoster)).Icon);
end;

procedure TPictureMirror.DrawStatusRectangle;
begin
  if SameStr('', ErrorMsg) and SameStr('', Value) then
    DrawIconRectangle()
  else
  begin
    with FImage.Canvas do
    begin
      if not SameStr('', ErrorMsg) then
        Brush.Color := clRed
      else
        Brush.Color := clLime;

      FillRect(Rect(10, 0, 16, 6));
    end;
  end;
end;

constructor TPictureMirror.Create(const AOwner: TWinControl; APicture: IPicture; const AImageHosterName: WideString);
begin
  inherited Create(AImageHosterName, APicture.Value);

  FImage := TImage.Create(AOwner);
  with FImage do
  begin
    Parent := AOwner;

    Center := True;

    ShowHint := True;
    Transparent := True;

    Height := 16;
    Width := 16;

    DrawIconRectangle();

    OnDblClick := FOnClickIconClick; // Double click required
  end;
  FPicture := APicture;
end;

destructor TPictureMirror.Destroy;
begin
  FPicture := nil;
  FImage.Free;
  inherited Destroy;
end;

procedure TPictureMirror.LocalUpload(const ALocalPath: WideString);
begin
  FPicture.ControlController.TabSheetController.PageController.ImageHosterManager.AddLocalUploadJob(Self, ALocalPath);
end;

procedure TPictureMirror.RemoteUpload;
begin
  FPicture.ControlController.TabSheetController.PageController.ImageHosterManager.AddRemoteUploadJob(Self, OriginalValue);
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TIPicture'}

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
begin
  SaveImage;
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

procedure TIPicture.SetControlTitle(const ATitle: WideString);
begin
  inherited SetControlTitle(ATitle);

  if Assigned(FPictureMirrorPanel) then
    FPictureMirrorPanel.Left := FTitleLabel.Left + FTitleLabel.Width + 6;
end;

function TIPicture.BitmapAsThumbnail(const Graphic: TGraphic): TBitmap;
const
  MAX_WIDTH = 300;
  MAX_HEIGHT = 300;
var
  LThumbnail: TBitmap;
  LThumbnailRect: TRect;
begin
  LThumbnail := TBitmap.Create;
  if Assigned(Graphic) and not Graphic.Empty then
  begin
    LThumbnail.Canvas.Lock;
    try
      LThumbnail.Assign(Graphic);
      LThumbnailRect.Left := 0;
      LThumbnailRect.Top := 0;
      if Graphic.Width > Graphic.Height then
      begin
        LThumbnailRect.Right := MAX_WIDTH;
        LThumbnailRect.Bottom := (MAX_WIDTH * Graphic.Height) div Graphic.Width;
      end
      else
      begin
        LThumbnailRect.Bottom := MAX_HEIGHT;
        LThumbnailRect.Right := (MAX_HEIGHT * Graphic.Width) div Graphic.Height;
      end;

      LThumbnail.Canvas.StretchDraw(LThumbnailRect, LThumbnail);
      LThumbnail.SetSize(LThumbnailRect.Right, LThumbnailRect.Bottom);
    finally
      LThumbnail.Canvas.Unlock;
    end;
  end;
  Result := LThumbnail;
end;

function TIPicture.GraphicAsVariant(AGraphic: TGraphic): Variant;
var
  LStringStream: TStringStream;
  LThumbnail: TBitmap;
begin
  LStringStream := TStringStream.Create('');
  try
    LThumbnail := BitmapAsThumbnail(AGraphic);
    with LThumbnail do
      try
        Canvas.Lock;
        try
          SaveToStream(LStringStream);
        finally
          Canvas.Unlock;
        end;
      finally
        Free;
      end;
    Result := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
end;

procedure TIPicture.SaveImage;
var
  LMemoryStream: TMemoryStream;
begin
  Async(
    { } procedure
    { } var
    { . } LCookies: WideString;
    { } begin
    { . } TApiHTTP.DownloadData(Value, LMemoryStream, LCookies,
      // TODO: Read settings at a thread-safe position
      { . } SettingsManager.Settings.HTTP.GetProxy(psaCrawler), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
    { } end). { }
  Await(
    { } procedure
    { } begin
    { . } SaveImage(LMemoryStream);
    { . } LMemoryStream.Free;
    { } end);
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

procedure TIPicture.HandleProposedValue(AIndex: Integer; const AValue: WideString);
var
  LMemoryStream: TMemoryStream;
  LCookies: WideString;
begin
  TApiHTTP.DownloadData(AValue, LMemoryStream, LCookies,
    // TODO: Read settings at a thread-safe position
    { . } SettingsManager.Settings.HTTP.GetProxy(psaCrawler), SettingsManager.Settings.HTTP.ConnectTimeout, SettingsManager.Settings.HTTP.ReadTimeout);
  try
    SetValuePictureFromDownload(AIndex - 1, LMemoryStream);
  finally
    LMemoryStream.Free;
  end;
end;

procedure TIPicture.SetValuePictureFromDownload(AIndex: Integer; AMemoryStream: TMemoryStream);
var
  LPictureInfo: TPictureInfo;
  LGraphic: TGraphic;
begin
  with LPictureInfo do
  begin
    Picture := '';
    Downloaded := True;
    Size := 0;
    Width := 0;
    Height := 0;
  end;

  LPictureInfo.Size := AMemoryStream.Size;

  try
    LGraphic := GetTGraphicType(AMemoryStream).Create;
    try
      LGraphic.LoadFromStream(AMemoryStream);

      if LGraphic.InheritsFrom(TJPEGImage) then
        with TJPEGImage(LGraphic) do
          DIBNeeded;

      with LPictureInfo do
      begin
        Picture := GraphicAsVariant(LGraphic);
        Width := LGraphic.Width;
        Height := LGraphic.Height;
      end;
    finally
      LGraphic.Free;
    end;

  except
    // This is not a image file. i.e. 404 error page not found
  end;

  SetValuePicture(AIndex, LPictureInfo);
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
  FPictureArrayLock.EnterWriteLock;
  try
    FPictureArray[AIndex] := APictureInfo;
  finally
    FPictureArrayLock.ExitWriteLock;
  end;
end;

function TIPicture.GetMirror(const IndexOrName: OleVariant): IPictureMirror;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    if VarIsNumeric(IndexOrName) then
      Result := FMirrorList[IndexOrName]
    else
      Result := FindMirror(IndexOrName);
  end;
end;

function TIPicture.GetMirrorCount: Integer;
begin
  Result := FMirrorList.Count;
end;

function TIPicture.AddMirror(const AName: WideString): Integer;
var
  LPictureMirror: IPictureMirror;
  LNewMenuItem: TdxBarButton;
begin
  Result := FindMirrorIndex(AName);

  if not(Result = -1) then
    Exit;

  LPictureMirror := TPictureMirror.Create(FPictureMirrorPanel, Self as IPicture, AName);

  Result := FMirrorList.Add(LPictureMirror);

  LNewMenuItem := TdxBarButton.Create(FPopupMenu);
  with LNewMenuItem do
  begin
    Caption := AName;

    OnClick := FmiRemoteUploadImageClick;
  end;
  with FmiRemoteUploadImageMenu.ItemLinks.Add do
    Item := LNewMenuItem;
  LNewMenuItem := TdxBarButton.Create(FPopupMenu);
  with LNewMenuItem do
  begin
    Caption := AName;

    OnClick := FmiLocalUploadImageClick;
  end;
  with FmiLocalUploadImageMenu.ItemLinks.Add do
    Item := LNewMenuItem;

  if Assigned(ControlController) and Assigned(ControlController.OnControlChange) then
    ControlController.OnControlChange.Invoke(Self);
end;

function TIPicture.RemoveMirror(AIndex: Integer): WordBool;
begin
  Result := True;
  try
    FMirrorList.Delete(AIndex);
    FmiLocalUploadImageMenu.ItemLinks[AIndex].Free;
    FmiRemoteUploadImageMenu.ItemLinks[AIndex].Free;
  except
    Result := False;
  end;

  if Assigned(ControlController) and Assigned(ControlController.OnControlChange) then
    ControlController.OnControlChange.Invoke(Self);
end;

constructor TIPicture.Create;
var
  I: Integer;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  SetLength(FPictureArray, 0);

  FMirrorList := TInterfaceList<IPictureMirror>.Create;

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
    Left := FTitleLabel.Left + FTitleLabel.Width + 6;
    Height := 16;
    Width := FClearLabel.Left - Left - 6;

    OnMouseDown := FPanelMouseDown;
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

  LoadDefaultConfiguration;
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

  FMirrorList.Free;

  inherited Destroy;
end;

procedure TIPicture.AddProposedValue(const ASender: WideString; const AValue: WideString; const ATitle: WideString = '');
var
  LProposedValueCount: Integer;
begin
  inherited AddProposedValue(ASender, AValue, ATitle);

  LProposedValueCount := GetProposedValuesCount;

  // Must be done in main thread, because user can access control editor
  // with images that are not downloaded so far, but the count has already
  // been set by inherited AddProposedValue(). Therefore do not do this
  // in the background task!
  FPictureArrayLock.EnterWriteLock;
  try
    SetLength(FPictureArray, LProposedValueCount);
    with FPictureArray[LProposedValueCount - 1] do
    begin
      Picture := GraphicAsVariant(nil);
      Downloaded := False;
    end;
  finally
    FPictureArrayLock.ExitWriteLock;
  end;

  // Do everything else in the background task. Access of TPictureInfo is
  // secured by TOmniMREW.
  Parallel.Async(
    { } procedure
    { } begin
    { . } HandleProposedValue(LProposedValueCount, AValue);
    { } end);
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
  LMirrorIndex: Integer;
begin
  for LMirrorIndex := 0 to MirrorCount - 1 do
    if not AAfterCrawling or (AAfterCrawling and UploadAfterCrawling(Mirror[LMirrorIndex].Name)) then
      Mirror[LMirrorIndex].RemoteUpload;
end;

function TIPicture.FindMirrorIndex(const AHoster: WideString): Integer;
var
  LIndex: Integer;
  LMirror: IPictureMirror;
begin
  Result := -1;

  for LIndex := 0 to FMirrorList.Count - 1 do
  begin
    LMirror := FMirrorList[LIndex];

    if SameText(AHoster, LMirror.Name) then
    begin
      Result := LIndex;
      break;
    end;
  end;
end;

function TIPicture.FindMirror(const AHoster: WideString): IPictureMirror;
var
  LFoundIndex: Integer;
begin
  Result := nil;

  LFoundIndex := FindMirrorIndex(AHoster);
  if not(LFoundIndex = -1) then
    Result := FMirrorList[LFoundIndex]
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
end;
{$ENDREGION}
{ ****************************************************************************** }
{$REGION 'TISubtitle'}

constructor TISubtitle.Create;
begin
  inherited Create(AOwner, AControlController, AComponentID);

  with FPanel.Constraints do
  begin
    MinHeight := 37;
    MinWidth := 125;
  end;

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
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

  LoadDefaultConfiguration;
end;
{$ENDREGION}

end.
