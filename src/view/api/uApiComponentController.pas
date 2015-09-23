unit uApiComponentController;

interface

uses
  // Delphi
  SysUtils, Controls, Classes, ExtCtrls, Generics.Collections,
  // MultiEvent
  Generics.MultiEvents.NotifyInterface,
  // Interface
  uAppInterface,
  // Common
  uConst,
  // Api
  uApiControls, uApiMultiCastEvent;

type
  TComponentController = class(TInterfacedObject, IComponentController)
  type
    TIBasicMeta = class of TIBasic;
  private
    FWorkPanel: TComponent;
    FControlList: TInterfaceList;
    FTabSheetController: ITabSheetController;
    FTemplateTypeID: TTemplateTypeID;

    FSpaceMouseDown: INotifyEventHandler;
    FControlChange: IControlChangeEvent;
    FControlEnter, FControlExit: IControlEventHandler;
    FReleaseNameChange: IReleaseNameChange;
    FPopupMenuChange: IPopupMenuChange;

    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetTemplateTypeID: TTemplateTypeID;
    procedure SetTemplateTypeID(ATemplateTypeID: TTemplateTypeID);
    function GetControl(index: Integer): IBasic;
    procedure SetControl(index: Integer; AControl: IBasic);

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

    function GetClassType(AType: TComponentID): TIBasicMeta;
  public
    constructor Create(AWorkPanel: TComponent);
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID write SetTemplateTypeID;
    procedure NewControl(AType: TComponentID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer); overload;
    procedure NewControl(AClass: TIBasicMeta; AType: TComponentID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer); overload;
    function FindControl(ComponentID: TComponentID): IBasic;
    property Control[index: Integer]: IBasic read GetControl write SetControl;
    function ControlCount: Integer;

    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnControlChange: IControlChangeEvent read GetControlChange;
    property OnControlEnter: IControlEventHandler read GetControlEnter write SetControlEnter;
    property OnControlExit: IControlEventHandler read GetControlExit write SetControlExit;
    property OnReleaseNameChange: IReleaseNameChange read GetReleaseNameChange write SetReleaseNameChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;

    destructor Destroy; override;
  end;

implementation

{ TComponentController }

function TComponentController.GetTabSheetController: ITabSheetController;
begin
  Result := FTabSheetController;
end;

procedure TComponentController.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabSheetController := ATabSheetController;
end;

function TComponentController.GetTemplateTypeID;
begin
  Result := FTemplateTypeID;
end;

procedure TComponentController.SetTemplateTypeID(ATemplateTypeID: TTemplateTypeID);
begin
  FTemplateTypeID := ATemplateTypeID;
end;

function TComponentController.GetControl(index: Integer): IBasic;
begin
  Result := (FControlList[index] as IBasic);
end;

procedure TComponentController.SetControl(index: Integer; AControl: IBasic);
begin
  FControlList[index] := AControl;
end;

function TComponentController.GetSpaceMouseDown;
begin
  Result := FSpaceMouseDown;
end;

procedure TComponentController.SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
begin
  FSpaceMouseDown := ASpaceMouseDown;
end;

function TComponentController.GetControlChange;
begin
  Result := FControlChange;
end;

function TComponentController.GetControlEnter;
begin
  Result := FControlEnter;
end;

procedure TComponentController.SetControlEnter(AControlEnter: IControlEventHandler);
begin
  FControlEnter := AControlEnter;
end;

function TComponentController.GetControlExit;
begin
  Result := FControlExit;
end;

procedure TComponentController.SetControlExit(AControlExit: IControlEventHandler);
begin
  FControlExit := AControlExit;
end;

function TComponentController.GetReleaseNameChange;
begin
  Result := FReleaseNameChange;
end;

procedure TComponentController.SetReleaseNameChange(AReleaseNameChange: IReleaseNameChange);
begin
  FReleaseNameChange := AReleaseNameChange;
end;

function TComponentController.GetPopupMenuChange;
begin
  Result := FPopupMenuChange;
end;

procedure TComponentController.SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
begin
  FPopupMenuChange := APopupMenuChange;
end;

function TComponentController.GetClassType(AType: TComponentID): TIBasicMeta;
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

constructor TComponentController.Create;
begin
  FWorkPanel := AWorkPanel;

  FControlList := TInterfaceList.Create;

  FControlChange := TIControlChangeEvent.Create;
end;

procedure TComponentController.NewControl(AType: TComponentID; ATitle, AValue, AHint, AList: WideString; ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  NewControl(GetClassType(AType), AType, ATitle, AValue, AHint, AList, ALeft, ATop, AWidth, AHeight);
end;

procedure TComponentController.NewControl(AClass: TIBasicMeta; AType: TComponentID; ATitle, AValue, AHint, AList: WideString; ALeft: Integer; ATop: Integer;
  AWidth: Integer; AHeight: Integer);
var
  lBasic: TIBasic;
  lBasicIntf: IBasic;
  lComboBoxIntf: IComboBox;
  lCheckComboBoxIntf: ICheckComboBox;
begin
  lBasic := AClass.Create(TWinControl(FWorkPanel), Self, AType);

  lBasicIntf := lBasic;
  with lBasicIntf do
  begin
    name := TComponentIDToString(AType);
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
    if lBasicIntf.QueryInterface(IComboBox, lComboBoxIntf) = 0 then
      lComboBoxIntf.List := AList;

    if lBasicIntf.QueryInterface(ICheckComboBox, lCheckComboBoxIntf) = 0 then
      lCheckComboBoxIntf.List := AList;
  end;

  FControlList.Add(lBasicIntf);
end;

function TComponentController.FindControl(ComponentID: TComponentID): IBasic;
var
  found: Boolean;
  I, Count: Integer;
begin
  found := false;

  I := 0;
  Count := ControlCount;

  Result := nil;

  while not found and (I < Count) do
  begin
    if GetControl(I).ComponentID = ComponentID then
    begin
      found := true;
      Result := GetControl(I);
    end;

    Inc(I);
  end;
end;

function TComponentController.ControlCount: Integer;
begin
  Result := FControlList.Count;
end;

destructor TComponentController.Destroy;
begin
  FSpaceMouseDown := nil;
  FControlChange := nil;
  FControlEnter := nil;
  FControlExit := nil;
  FReleaseNameChange := nil;
  FPopupMenuChange := nil;

  FControlList.Free;

  FTabSheetController := nil;

  FWorkPanel := nil;
end;

end.
