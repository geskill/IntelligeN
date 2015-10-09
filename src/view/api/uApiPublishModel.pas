unit uApiPublishModel;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiControlsBase, uApiMirrorControlBase;

type
  TICMSWebsiteData = class(TInterfacedObject, ITabSheetData)
  private
    FTypeID: TTypeID;
    FControlList: TList<IControlData>;
    FMirrorList: TList<IMirrorContainer>;
  protected
    function GetTypeID: TTypeID; safecall;

    function GetControl(const IndexOrName: OleVariant): IControlData; safecall;
    function GetControlCount: Integer; safecall;
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorCount: Integer; safecall;
  public
    constructor Create(ATypeID: TTypeID);
    destructor Destroy; override;

    property TypeID: TTypeID read GetTypeID;

    property ControlList: TList<IControlData>read FControlList;
    property MirrorList: TList<IMirrorContainer>read FMirrorList;

    function FindControl(const AControlID: TControlID): IControlData; safecall;
    function FindMirror(const AHoster: WideString): IMirrorContainer; safecall;

    property Control[const IndexOrName: OleVariant]: IControlData read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;

  end;

implementation

{ TITabSheetData }

function TICMSWebsiteData.GetTypeID: TTypeID;
begin
  Result := FTypeID;
end;

function TICMSWebsiteData.GetControl(const IndexOrName: OleVariant): IControlData;
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

function TICMSWebsiteData.GetControlCount: Integer;
begin
  Result := FControlList.Count;
end;

function TICMSWebsiteData.GetMirror(const IndexOrName: OleVariant): IMirrorContainer;
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

function TICMSWebsiteData.GetMirrorCount: Integer;
begin
  Result := FMirrorList.Count;
end;

constructor TICMSWebsiteData.Create(ATypeID: TTypeID);
begin
  FTypeID := ATypeID;
  FControlList := TList<IControlData>.Create;
  FMirrorList := TList<IMirrorContainer>.Create;
end;

function TICMSWebsiteData.FindControl(const AControlID: TControlID): IControlData;
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

function TICMSWebsiteData.FindMirror(const AHoster: WideString): IMirrorContainer;
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

destructor TICMSWebsiteData.Destroy;
begin
  FMirrorList.Free;
  FControlList.Free;

  inherited Destroy;
end;

end.
