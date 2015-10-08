unit uApiControlControllerBase;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Api
  uApiControlsBase;

type
  TIControlControllerBase = class(TInterfacedObject, IControlControllerBase)
  private
    FControlList: TInterfaceList<IControlBase>;
  protected
    function GetControl(const IndexOrName: OleVariant): IControlBase; safecall;
    function GetControlCount: Integer; safecall;
  public
    constructor Create;
    constructor Clone(const AControlControllerBase: IControlControllerBase);
    destructor Destroy; override;

    function FindControl(const AControlID: TControlID): IControlBase; safecall;

    property Control[const IndexOrName: OleVariant]: IControlBase read GetControl;
    property ControlCount: Integer read GetControlCount;
  end;

implementation

function TIControlControllerBase.GetControl(const IndexOrName: OleVariant): IControlBase;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    if VarIsNumeric(IndexOrName) then
      Result := FControlList[IndexOrName]
    else
      Result := FindControl(StringToControlID(IndexOrName));
  end;
end;

function TIControlControllerBase.GetControlCount: Integer;
begin
  Result := FControlList.Count;
end;

constructor TIControlControllerBase.Create;
begin
  inherited Create;
  FControlList := TInterfaceList<IControlBase>.Create();
end;

constructor TIControlControllerBase.Clone(const AControlControllerBase: IControlControllerBase);
var
  LIndex: Integer;
  LControl: IControlBase;
begin
  Create;

  for LIndex := 0 to AControlControllerBase.ControlCount - 1 do
  begin
    LControl := TIControlBase.Clone(AControlControllerBase.Control[LIndex]);
    FControlList.Add(LControl);
  end;
end;

destructor TIControlControllerBase.Destroy;
begin
  FControlList.Free;
  inherited Destroy;
end;

function TIControlControllerBase.FindControl(const AControlID: TControlID): IControlBase;
var
  LIndex: Integer;
  LControl: IControlBase;
begin
  Result := nil;

  for LIndex := 0 to FControlList.Count - 1 do
  begin
    LControl := FControlList[LIndex];

    if (LControl.ControlID = AControlID) then
    begin
      Result := LControl;
      break;
    end;
  end;
end;

end.
