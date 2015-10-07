unit uApiMirrorController;

interface

uses
  // Delphi
  Classes, Controls, Types,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent, Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiComponentParser, uApiMultiCastEvent;

type
  TMirrorController = class(TInterfacedObject, IMirrorController)
  private
    FMirrorList: TInterfaceList;
    FWorkPanel: TControl;
    FTabSheetController: ITabSheetController;
    FSpaceMouseDown: INotifyEventHandler;
    FChange: INotifyEvent;
    FPopupMenuChange: IPopupMenuChange;
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetMirror(index: Integer): IMirrorControl;
    function GetMirrorCount: Integer;

    function GetSpaceMouseDown: INotifyEventHandler;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
    function GetChange: INotifyEvent;
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);
  public
    constructor Create(AWorkPanel: TControl);
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    function IndexOf(const Item: IMirrorControl): Integer;
    function Add: Integer;
    procedure Insert(index: Integer; const Item: IMirrorControl); overload;
    function Insert(index: Integer): IMirrorControl; overload;
    function Remove(index: Integer): Boolean;
    property Mirror[index: Integer]: IMirrorControl read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;

    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnChange: INotifyEvent read GetChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;

    destructor Destroy; override;
  end;

implementation

uses
  // Api
  uApiMirrorControl;

{ TMirrorController }

function TMirrorController.GetTabSheetController: ITabSheetController;
begin
  Result := FTabSheetController;
end;

procedure TMirrorController.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabSheetController := ATabSheetController;
end;

function TMirrorController.GetMirror(index: Integer): IMirrorControl;
begin
  Result := (FMirrorList[index] as IMirrorControl);
end;

function TMirrorController.GetMirrorCount;
begin
  Result := FMirrorList.Count;
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
  FMirrorList := TInterfaceList.Create;
  FWorkPanel := AWorkPanel;
  FChange := TINotifyEvent.Create;
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
  with TComponentParser.Create do
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

destructor TMirrorController.Destroy;
begin
  FPopupMenuChange := nil;
  FChange := nil;
  FSpaceMouseDown := nil;
  TabSheetController := nil;
  FWorkPanel := nil;
  FMirrorList.Free;
end;

end.
