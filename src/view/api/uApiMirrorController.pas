unit uApiMirrorController;

interface

uses
  // Delphi
  Classes, Controls, Types, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent, Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiMirrorControllerBase, uApiControlAligner, uApiMultiCastEvent;

type
  TMirrorController = class(TIMirrorControllerBase, IMirrorController)
  private
    FMirrorList: TInterfaceList<IMirrorControl>;

    FWorkPanel: TControl;
    FTabSheetController: ITabSheetController;

    FSpaceMouseDown: INotifyEventHandler;
    FChange: INotifyEvent;
    FPopupMenuChange: IPopupMenuChange;
  protected
    // Base
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorControl(const IndexOrName: OleVariant): IMirrorControl; safecall;
    function IMirrorController.GetMirror = GetMirrorControl;
    function GetMirrorCount: Integer; safecall;

    // Additional
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);

    // Events
    function GetSpaceMouseDown: INotifyEventHandler;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
    function GetChange: INotifyEvent;
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
  public
    constructor Create(AWorkPanel: TControl);
    destructor Destroy; override;

    // Base
    function FindMirror(const AHoster: WideString): IMirrorContainer; safecall;
    function FindMirrorControl(const AHoster: WideString): IMirrorControl; safecall;
    function IMirrorController.FindMirror = FindMirrorControl;

    property Mirror[const IndexOrName: OleVariant]: IMirrorControl read GetMirrorControl; default;
    property MirrorCount: Integer read GetMirrorCount;

    // Additional
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;

    // TODO: Re-work this needed?
    function IndexOf(const Item: IMirrorControl): Integer;
    function Add: Integer;
    procedure Insert(index: Integer; const Item: IMirrorControl); overload;
    function Insert(index: Integer): IMirrorControl; overload;
    function Remove(index: Integer): Boolean;

    // Cloning
    function CloneInstance(): IMirrorControllerBase;

    // Events
    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnChange: INotifyEvent read GetChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;
  end;

implementation

uses
  // Api
  uApiMirrorControl;

{ TMirrorController }

function TMirrorController.GetMirror(const IndexOrName: OleVariant): IMirrorContainer;
begin
  Result := GetMirrorControl(IndexOrName);
end;

function TMirrorController.GetMirrorControl(const IndexOrName: OleVariant): IMirrorControl;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    if VarIsNumeric(IndexOrName) then
      Result := FMirrorList[IndexOrName]
    else
      Result := FindMirrorControl(IndexOrName);
  end;
end;

function TMirrorController.GetMirrorCount;
begin
  Result := FMirrorList.Count;
end;

function TMirrorController.GetTabSheetController: ITabSheetController;
begin
  Result := FTabSheetController;
end;

procedure TMirrorController.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabSheetController := ATabSheetController;
end;

function TMirrorController.GetSpaceMouseDown;
begin
  Result := FSpaceMouseDown;
end;

procedure TMirrorController.SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
begin
  FSpaceMouseDown := ASpaceMouseDown;
end;

function TMirrorController.GetChange;
begin
  Result := FChange;
end;

function TMirrorController.GetPopupMenuChange;
begin
  Result := FPopupMenuChange;
end;

procedure TMirrorController.SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
begin
  FPopupMenuChange := APopupMenuChange;
end;

constructor TMirrorController.Create;
begin
  inherited Create;

  FWorkPanel := AWorkPanel;
  FMirrorList := TInterfaceList<IMirrorControl>.Create;

  FChange := TINotifyEvent.Create;
end;

destructor TMirrorController.Destroy;
begin
  FPopupMenuChange := nil;
  FChange := nil;
  FSpaceMouseDown := nil;

  TabSheetController := nil;

  FMirrorList.Free;
  FWorkPanel := nil;

  inherited Destroy;
end;

function TMirrorController.FindMirror(const AHoster: WideString): IMirrorContainer;
begin
  Result := FindMirrorControl(AHoster);
end;

function TMirrorController.FindMirrorControl(const AHoster: WideString): IMirrorControl;
var
  LIndex: Integer;
  LMirror: IMirrorContainer;
begin
  Result := nil;

  for LIndex := 0 to FMirrorList.Count - 1 do
  begin
    LMirror := FMirrorList[LIndex];

    if SameText(AHoster, LMirror.Hoster) then
    begin
      Result := LMirror;
      break;
    end;
  end;
end;

function TMirrorController.IndexOf(const Item: IMirrorControl): Integer;
begin
  Result := FMirrorList.IndexOf(Item as IMirrorControl);
end;

function TMirrorController.Add: Integer;
var
  MirrorControl: IMirrorControl;
  Point: TPoint;
begin
  with TControlAligner.Create do
  try
    WorkPanelWidth := FWorkPanel.Width;
    ControlController := TabSheetController.ControlController;
    MirrorController := Self;
    Point := NextMirrorPosition;
    ControlController := nil;
    MirrorController := nil;
  finally
    Free;
  end;

  MirrorControl := TMirrorControl.Create(FWorkPanel, Point.X, Point.Y);
  MirrorControl.MirrorController := Self;

  Result := FMirrorList.Add(MirrorControl);
end;

procedure TMirrorController.Insert(index: Integer; const Item: IMirrorControl);
begin
  FMirrorList.Insert(index, Item as IMirrorControl);
end;

function TMirrorController.Insert(index: Integer): IMirrorControl;
begin
  Result := TMirrorControl.Create(FWorkPanel);
  Result.MirrorController := Self;
  FMirrorList.Insert(index, Result);
end;

function TMirrorController.Remove(index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  try
    for I := 0 to Mirror[index].DirectlinksMirrorCount - 1 do
    begin
      TabSheetController.PageController.HosterManager.RemoveHosterCheckJob(Mirror[index].Directlink.Mirror[I]);
      Mirror[index].Directlink.Mirror[I].DirectlinksPanel := nil;
    end;
    for I := 0 to Mirror[index].CrypterCount - 1 do
      Mirror[index].Crypter[I].MirrorControl := nil;
    Mirror[index].Directlink.MirrorControl := nil;
    Mirror[index].MirrorController := nil;

    FMirrorList.Delete(index);
  except
    Result := False;
  end;
end;

function TMirrorController.CloneInstance: IMirrorControllerBase;
var
  LMirrorControllerBase: IMirrorControllerBase;
begin
  LMirrorControllerBase := TIMirrorControllerBase.Clone(Self);

  Result := LMirrorControllerBase;
end;

end.
