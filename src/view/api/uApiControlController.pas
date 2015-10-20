unit uApiControlController;

interface

uses
  // Delphi
  SysUtils, Controls, Classes, Types, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // MultiEvent
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiControls, uApiControlControllerBase, uApiControlAligner, uApiMultiCastEvent;

type
  TControlController = class(TIControlControllerBase, IControlController)
  type
    TIControlBasicMeta = class of TIControlBasic;
  private
    FControlList: TInterfaceList<IControlBasic>;

    FWorkPanel: TControl;
    FTabSheetController: ITabSheetController;
    FTypeID: TTypeID;

    FSpaceMouseDown: INotifyEventHandler;
    FControlChange: IControlChangeEvent;
    FControlEnter, FControlExit: IControlEventHandler;
    FReleaseNameChange: IReleaseNameChange;
    FPopupMenuChange: IPopupMenuChange;

    function GetClassType(AType: TControlID): TIControlBasicMeta;
  protected
    // Base (see: http://stackoverflow.com/questions/27929314/how-do-i-implement-two-interfaces-that-have-methods-with-the-same-name)
    function GetControl(const IndexOrName: OleVariant): IControlBase; override; safecall;
    function GetBasicControl(const IndexOrName: OleVariant): IControlBasic; safecall;
    function IControlController.GetControl = GetBasicControl;
    function GetControlCount: Integer; override; safecall;

    // Additional
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetTypeID: TTypeID;
    procedure SetTypeID(ATypeID: TTypeID);

    // Events
    function GetSpaceMouseDown: INotifyEventHandler;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
    function GetControlChange: IControlChangeEvent;
    function GetControlEnter: IControlEventHandler;
    procedure SetControlEnter(AControlEnter: IControlEventHandler);
    function GetControlExit: IControlEventHandler;
    procedure SetControlExit(AControlExit: IControlEventHandler);
    function GetReleaseNameChange: IReleaseNameChange;
    procedure SetReleaseNameChange(AReleaseNameChange: IReleaseNameChange);
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
  public
    constructor Create(AWorkPanel: TControl);
    destructor Destroy; override;

    // Base (see above on stackoverflow.com)
    function FindControl(const AControlID: TControlID): IControlBase; safecall;
    function FindBasicControl(const AControlID: TControlID): IControlBasic; safecall;
    function IControlController.FindControl = FindBasicControl;

    property Control[const IndexOrName: OleVariant]: IControlBasic read GetBasicControl; default;
    property ControlCount: Integer read GetControlCount;

    // Additional
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property TypeID: TTypeID read GetTypeID write SetTypeID;

    procedure NewControl(AType: TControlID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer); overload;
    procedure NewControl(AClass: TIControlBasicMeta; AType: TControlID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer); overload;

    // Cloning
    function CloneInstance(): IControlControllerBase;

    // Events
    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnControlChange: IControlChangeEvent read GetControlChange;
    property OnControlEnter: IControlEventHandler read GetControlEnter write SetControlEnter;
    property OnControlExit: IControlEventHandler read GetControlExit write SetControlExit;
    property OnReleaseNameChange: IReleaseNameChange read GetReleaseNameChange write SetReleaseNameChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;
  end;

implementation

{ TComponentController }

function TControlController.GetClassType(AType: TControlID): TIControlBasicMeta;
begin
  case AType of
    cReleaseName:
      Result := TIReleaseName;
    cReleaseDate:
      Result := TIReleaseDate;
    cTags:
      Result := TITags;
    cTitle:
      Result := TITitle;
    cArtist:
      Result := TIArtist;
    cPicture:
      Result := TIPicture;
    cTrailer:
      Result := TITrailer;
    cSample:
      Result := TISample;
    cNotes:
      Result := TINotes;
    cPassword:
      Result := TIPassword;
    cAudioBitrate:
      Result := TIAudioBitrate;
    cAudioBitrateType:
      Result := TIAudioBitrateType;
    cAudioEncoder:
      Result := TIAudioEncoder;
    cAudioSamplingRate:
      Result := TIAudioSamplingRate;
    cAudioStream:
      Result := TIAudioStream;
    cGenre:
      Result := TIGenre;
    cLanguage:
      Result := TILanguage;
    cRuntime:
      Result := TIRuntime;
    cVideoCodec:
      Result := TIVideoCodec;
    cVideoStream:
      Result := TIVideoStream;
    cVideoSystem:
      Result := TIVideoSystem;
    cNFO:
      Result := TINFO;
    cDescription:
      Result := TIDescription;
  else
    raise Exception.Create('Unknown component');
  end;
end;

function TControlController.GetControl(const IndexOrName: OleVariant): IControlBase;
begin
  Result := GetBasicControl(IndexOrName);
end;

function TControlController.GetBasicControl(const IndexOrName: OleVariant): IControlBasic;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    if VarIsNumeric(IndexOrName) then
      Result := FControlList[IndexOrName]
    else
      Result := FindBasicControl(StringToControlID(IndexOrName));
  end;
end;

function TControlController.GetControlCount: Integer;
begin
  Result := FControlList.Count;
end;

function TControlController.GetTabSheetController: ITabSheetController;
begin
  Result := FTabSheetController;
end;

procedure TControlController.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabSheetController := ATabSheetController;
end;

function TControlController.GetTypeID;
begin
  Result := FTypeID;
end;

procedure TControlController.SetTypeID(ATypeID: TTypeID);
begin
  FTypeID := ATypeID;
end;

function TControlController.GetSpaceMouseDown;
begin
  Result := FSpaceMouseDown;
end;

procedure TControlController.SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
begin
  FSpaceMouseDown := ASpaceMouseDown;
end;

function TControlController.GetControlChange;
begin
  Result := FControlChange;
end;

function TControlController.GetControlEnter;
begin
  Result := FControlEnter;
end;

procedure TControlController.SetControlEnter(AControlEnter: IControlEventHandler);
begin
  FControlEnter := AControlEnter;
end;

function TControlController.GetControlExit;
begin
  Result := FControlExit;
end;

procedure TControlController.SetControlExit(AControlExit: IControlEventHandler);
begin
  FControlExit := AControlExit;
end;

function TControlController.GetReleaseNameChange;
begin
  Result := FReleaseNameChange;
end;

procedure TControlController.SetReleaseNameChange(AReleaseNameChange: IReleaseNameChange);
begin
  FReleaseNameChange := AReleaseNameChange;
end;

function TControlController.GetPopupMenuChange;
begin
  Result := FPopupMenuChange;
end;

procedure TControlController.SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
begin
  FPopupMenuChange := APopupMenuChange;
end;

constructor TControlController.Create;
begin
  inherited Create;

  FWorkPanel := AWorkPanel;
  FControlList := TInterfaceList<IControlBasic>.Create;

  FControlChange := TIControlChangeEvent.Create;
end;

destructor TControlController.Destroy;
begin
  FSpaceMouseDown := nil;
  FControlChange := nil;
  FControlEnter := nil;
  FControlExit := nil;
  FReleaseNameChange := nil;
  FPopupMenuChange := nil;

  FTabSheetController := nil;

  FControlList.Free;
  FWorkPanel := nil;

  inherited Destroy;
end;

function TControlController.FindControl(const AControlID: TControlID): IControlBase;
begin
  Result := FindBasicControl(AControlID);
end;

function TControlController.FindBasicControl(const AControlID: TControlID): IControlBasic;
var
  LIndex: Integer;
  LControl: IControlBasic;
begin
  Result := nil;

  for LIndex := 0 to FControlList.Count - 1 do
  begin
    LControl := FControlList[LIndex];

    if (AControlID = LControl.ControlID) then
    begin
      Result := LControl;
      break;
    end;
  end;
end;

procedure TControlController.NewControl(AType: TControlID; ATitle, AValue, AHint, AList: WideString; ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
begin
  NewControl(GetClassType(AType), AType, ATitle, AValue, AHint, AList, ALeft, ATop, AWidth, AHeight);
end;

procedure TControlController.NewControl(AClass: TIControlBasicMeta; AType: TControlID; ATitle, AValue, AHint, AList: WideString; ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
var
  LBasicControl: TIControlBasic;
  LBasicControlInterface: IControlBasic;
  LComboBoxInterface: IControlComboBox;
  LCheckComboBoxInterface: IControlCheckComboBox;
  // LPoint: TPoint;
begin
  LBasicControl := AClass.Create(TWinControl(FWorkPanel), Self, AType);

  (*
  with TControlAligner.Create do
  try
    WorkPanelWidth := FWorkPanel.Width;
    ControlController := Self;
    MirrorController := TabSheetController.MirrorController;
    LPoint := NextControlPosition(AHeight, AWidth);
    ControlController := nil;
    MirrorController := nil;
  finally
    Free;
  end;
  *)

  LBasicControlInterface := LBasicControl;
  with LBasicControlInterface do
  begin
    Name := ControlIDToString(AType);
    if not(ATitle = '#') then
      Title := ATitle;
    if not(AHint = '#') then
      Hint := AHint;
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
    if not(AValue = '#') then
      Value := AValue; // abstract
  end;

  if not(AList = '#') then
  begin
    if LBasicControlInterface.QueryInterface(IControlComboBox, LComboBoxInterface) = 0 then
      LComboBoxInterface.List := AList;

    if LBasicControlInterface.QueryInterface(IControlCheckComboBox, LCheckComboBoxInterface) = 0 then
      LCheckComboBoxInterface.List := AList;
  end;

  FControlList.Add(LBasicControlInterface);
end;

function TControlController.CloneInstance: IControlControllerBase;
var
  LControlControllerBase: IControlControllerBase;
begin
  LControlControllerBase := TIControlControllerBase.Clone(Self);

  Result := LControlControllerBase;
end;

end.
