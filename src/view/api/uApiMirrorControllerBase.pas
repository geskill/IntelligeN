unit uApiMirrorControllerBase;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Api
  uApiMirrorControlBase;

type
  TMirrorContainerList = TInterfaceList<IMirrorContainer>;

  TIMirrorControllerBase = class(TInterfacedObject, IMirrorControllerBase)
  private
    FMirrorList: TMirrorContainerList;
  protected
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; virtual; safecall;
    function GetMirrorCount: Integer; virtual; safecall;
  public
    constructor Create;
    constructor Clone(const AMirrorControllerBase: IMirrorControllerBase);
    destructor Destroy; override;

    function FindMirror(const AHoster: WideString): IMirrorContainer; safecall;

    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
  end;

implementation

function TIMirrorControllerBase.GetMirror(const IndexOrName: OleVariant): uBaseInterface.IMirrorContainer;
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

function TIMirrorControllerBase.GetMirrorCount: Integer;
begin
  Result := FMirrorList.Count;
end;

constructor TIMirrorControllerBase.Create;
begin
  inherited Create;
  FMirrorList := TMirrorContainerList.Create();
end;

constructor TIMirrorControllerBase.Clone(const AMirrorControllerBase: IMirrorControllerBase);
var
  LIndex: Integer;
  LMirror: IMirrorContainer;
begin
  Create;

  for LIndex := 0 to AMirrorControllerBase.MirrorCount - 1 do
  begin
    LMirror := TIMirrorContainer.Clone(AMirrorControllerBase.Mirror[LIndex]);
    FMirrorList.Add(LMirror);
  end;
end;

destructor TIMirrorControllerBase.Destroy;
begin
  FMirrorList.Free;
  inherited Destroy;
end;

function TIMirrorControllerBase.FindMirror(const AHoster: WideString): IMirrorContainer;
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

end.
