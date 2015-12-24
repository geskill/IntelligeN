unit uApiTabSheetData;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiControlsBase, uApiMirrorControlBase, uApiMirrorControllerBase;

type
  TControlDataList = TInterfaceList<IControlData>;

  TITabSheetData = class(TInterfacedObject, ITabSheetData)
  private
    FTypeID: TTypeID;
    FControlList: TControlDataList;
    FMirrorList: TMirrorContainerList;
  protected
    function GetTypeID: TTypeID; safecall;

    function GetControl(const IndexOrName: OleVariant): IControlData; safecall;
    function GetControlCount: Integer; safecall;
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorCount: Integer; safecall;

    function GetCustomField(const IndexOrName: OleVariant): INameValueItem; safecall;
    function GetCustomFieldCount: Integer; safecall;
  public
    constructor Create(ATypeID: TTypeID); overload;
    constructor Create(ATypeID: TTypeID; AControlList: TControlDataList; AMirrorList: TMirrorContainerList); overload;
    constructor Clone(const ATabSheetData: ITabSheetData);
    destructor Destroy; override;

    property TypeID: TTypeID read GetTypeID;

    function FindControl(const AControlID: TControlID): IControlData; safecall;
    function FindMirror(const AHoster: WideString): IMirrorContainer; safecall;

    property Control[const IndexOrName: OleVariant]: IControlData read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;

    property CustomField[const IndexOrName: OleVariant]: INameValueItem read GetCustomField;
    property CustomFieldCount: Integer read GetCustomFieldCount;
  end;

implementation

{ TITabSheetData }

function TITabSheetData.GetTypeID: TTypeID;
begin
  Result := FTypeID;
end;

function TITabSheetData.GetControl(const IndexOrName: OleVariant): IControlData;
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

function TITabSheetData.GetControlCount: Integer;
begin
  Result := FControlList.Count;
end;

function TITabSheetData.GetMirror(const IndexOrName: OleVariant): IMirrorContainer;
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

function TITabSheetData.GetMirrorCount: Integer;
begin
  Result := FMirrorList.Count;
end;

function TITabSheetData.GetCustomField(const IndexOrName: OleVariant): INameValueItem;
begin
  // TODO: Implement this
  Result := nil;
end;

function TITabSheetData.GetCustomFieldCount: Integer;
begin
  // TODO: Implement this
  Result := 0;
end;

constructor TITabSheetData.Create(ATypeID: TTypeID);
begin
  Create(ATypeID, nil, nil);
end;

constructor TITabSheetData.Create(ATypeID: TTypeID; AControlList: TControlDataList; AMirrorList: TMirrorContainerList);
begin
  inherited Create;

  FTypeID := ATypeID;

  if not Assigned(AControlList) then
    FControlList := TControlDataList.Create
  else
    FControlList := AControlList;

  if not Assigned(AMirrorList) then
    FMirrorList := TMirrorContainerList.Create
  else
    FMirrorList := AMirrorList;
end;

constructor TITabSheetData.Clone(const ATabSheetData: ITabSheetData);
var
  LIndex: Integer;
  LControl: IControlData;
  LMirror: IMirrorContainer;
begin
  Create;

  for LIndex := 0 to ATabSheetData.ControlCount - 1 do
  begin
    LControl := TIControlData.Clone(ATabSheetData.Control[LIndex]);
    FControlList.Add(LControl);
  end;

  for LIndex := 0 to ATabSheetData.MirrorCount - 1 do
  begin
    LMirror := TIMirrorContainer.Clone(ATabSheetData.Mirror[LIndex]);
    FMirrorList.Add(LMirror);
  end;
end;

function TITabSheetData.FindControl(const AControlID: TControlID): IControlData;
var
  LIndex: Integer;
  LControl: IControlData;
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

function TITabSheetData.FindMirror(const AHoster: WideString): IMirrorContainer;
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

destructor TITabSheetData.Destroy;
begin
  FMirrorList.Free;
  FControlList.Free;

  inherited Destroy;
end;

end.
