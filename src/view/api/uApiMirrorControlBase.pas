unit uApiMirrorControlBase;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists, Spring.SystemUtils,
  // Common
  uBaseConst, uBaseInterface,
  // Api
  uApiControlsBase,
  // Utils
  uVariantUtils;

type
  TAbstractFunc = reference to function(): Variant;
  TAbstractIndexFunc = reference to function(AIndex: Integer): Variant;

  TIMirrorData = class(TIValueItem, IMirrorData)
  private
    FStatus: TContentStatus;
    FSize, FPartSize: Double;
    FHoster, FHosterShort: WideString;
    FParts: Integer;
  protected
    function GetStatus: TContentStatus; virtual; safecall;
    function GetSize: Double; virtual; safecall;
    function GetPartSize: Double; virtual; safecall;
    function GetHoster: WideString; virtual; safecall;
    function GetHosterShort: WideString; virtual; safecall;
    function GetParts: Integer; virtual; safecall;
  public
    constructor Create(AStatus: TContentStatus; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = ''); reintroduce;
    constructor Clone(const AMirrorData: IMirrorData);
    destructor Destroy; override;

    property Status: TContentStatus read GetStatus;
    property Size: Double read GetSize;
    property PartSize: Double read GetPartSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
  end;

  TIDirectlink = class(TIMirrorData, IDirectlink)
  private
    FFileName: WideString;
  protected
    function GetFileName: WideString; virtual; safecall;
  public
    constructor Create(AFileName: WideString; AStatus: TContentStatus; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = '');
    constructor Clone(const ADirectlink: IDirectlink);
    destructor Destroy; override;

    property FileName: WideString read GetFileName;
  end;

  TICrypter = class(TIMirrorData, ICrypter)
  private
    FName, FStatusImage, FStatusImageText: WideString;
  protected
    function GetName: WideString; safecall;
    function GetStatusImage: WideString; safecall;
    function GetStatusImageText: WideString; safecall;
  public
    constructor Create(AName, AStatusImage, AStatusImageText: WideString; AStatus: TContentStatus; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = '');
    constructor Clone(const ACrypter: ICrypter);
    destructor Destroy; override;

    property Name: WideString read GetName;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
  end;

  TIDirectlinkContainer = class(TIDirectlink, IDirectlinkContainer)
  private
    FDirectlinkList: TList<IDirectlink>;
  protected
    function GetValue: WideString; override; safecall;
    function GetStatus: TContentStatus; override; safecall;
    function GetSize: Double; override; safecall;
    function GetPartSize: Double; override; safecall;
    function GetHoster: WideString; override; safecall;
    function GetHosterShort: WideString; override; safecall;
    function GetParts: Integer; override; safecall;
    function GetFileName: WideString; override; safecall;

    function GetDirectlink(const Index: Integer): IDirectlink; virtual; safecall;
    function GetDirectlinkCount: Integer; virtual; safecall;
  protected
    function GetAbstractBestValue(AValueAtIndex: TAbstractIndexFunc): Variant; virtual;
  public
    constructor Create(); reintroduce;
    constructor Clone(const ADirectlinkContainer: IDirectlinkContainer);
    destructor Destroy; override;

    property Directlink[const Index: Integer]: IDirectlink read GetDirectlink;
    property DirectlinkCount: Integer read GetDirectlinkCount;
  end;

  TIMirrorContainer = class(TIDirectlinkContainer, IMirrorContainer)
  private
    FCrypterList: TList<ICrypter>;
  protected
    function GetValue: WideString; override; safecall;
    function GetStatus: TContentStatus; override; safecall;
    function GetSize: Double; override; safecall;
    function GetPartSize: Double; override; safecall;
    function GetHoster: WideString; override; safecall;
    function GetHosterShort: WideString; override; safecall;
    function GetParts: Integer; override; safecall;
    function GetFileName: WideString; override; safecall;

    function GetCrypter(const IndexOrName: OleVariant): ICrypter; virtual; safecall;
    function GetCrypterCount: Integer; virtual; safecall;
  protected
    function GetAbstractBestValue(AValue: TAbstractFunc; AValueAtIndex: TAbstractIndexFunc): Variant; reintroduce;
  public
    constructor Create(); reintroduce;
    constructor Clone(const AMirrorContainer: IMirrorContainer);
    destructor Destroy; override;

    function FindCrypter(const AName: WideString): ICrypter; safecall;

    property Crypter[const IndexOrName: OleVariant]: ICrypter read GetCrypter;
    property CrypterCount: Integer read GetCrypterCount;
  end;

implementation

{$REGION 'TIMirrorData'}
{ TIMirrorData }

function TIMirrorData.GetStatus: TContentStatus;
begin
  Result := FStatus;
end;

function TIMirrorData.GetSize: Double;
begin
  Result := FSize;
end;

function TIMirrorData.GetPartSize: Double;
begin
  Result := FPartSize;
end;

function TIMirrorData.GetHoster: WideString;
begin
  Result := FHoster;
end;

function TIMirrorData.GetHosterShort: WideString;
begin
  Result := FHosterShort;
end;

function TIMirrorData.GetParts: Integer;
begin
  Result := FParts;
end;

constructor TIMirrorData.Create;
begin
  inherited Create(AValue);

  FStatus := AStatus;
  FSize := ASize;
  FPartSize := APartSize;
  FHoster := AHoster;
  FHosterShort := AHosterShort;
  FParts := AParts;
end;

constructor TIMirrorData.Clone(const AMirrorData: IMirrorData);
begin
  Create(AMirrorData.Status, AMirrorData.Size, AMirrorData.PartSize, AMirrorData.Hoster, AMirrorData.HosterShort, AMirrorData.Parts, AMirrorData.Value);
end;

destructor TIMirrorData.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{ ... }
{$REGION 'TIDirectlink'}
{ TIDirectlink }

function TIDirectlink.GetFileName: WideString;
begin
  Result := FFileName;
end;

constructor TIDirectlink.Create;
begin
  inherited Create(AStatus, ASize, APartSize, AHoster, AHosterShort, AParts, AValue);

  FFileName := AFileName;
end;

constructor TIDirectlink.Clone(const ADirectlink: IDirectlink);
begin
  Create(ADirectlink.FileName, ADirectlink.Status, ADirectlink.Size, ADirectlink.PartSize, ADirectlink.Hoster, ADirectlink.HosterShort, ADirectlink.Parts, ADirectlink.Value);
end;

destructor TIDirectlink.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{ ... }
{$REGION 'TICrypter'}
{ TICrypter }

function TICrypter.GetName: WideString;
begin
  Result := FName;
end;

function TICrypter.GetStatusImage: WideString;
begin
  Result := FStatusImage;
end;

function TICrypter.GetStatusImageText: WideString;
begin
  Result := FStatusImageText;
end;

constructor TICrypter.Create;
begin
  inherited Create(AStatus, ASize, APartSize, AHoster, AHosterShort, AParts, AValue);

  FName := AName;
  FStatusImage := AStatusImage;
  FStatusImageText := AStatusImageText;
end;

constructor TICrypter.Clone(const ACrypter: ICrypter);
begin
  Create(ACrypter.Name, ACrypter.StatusImage, ACrypter.StatusImageText, ACrypter.Status, ACrypter.Size, ACrypter.PartSize, ACrypter.Hoster, ACrypter.HosterShort, ACrypter.Parts, ACrypter.Value);
end;

destructor TICrypter.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{ ... }

{ TIDirectlinkContainer }

function TIDirectlinkContainer.GetValue: WideString;
begin
  Result := ''; // never used // do nothing
end;

function TIDirectlinkContainer.GetStatus: TContentStatus;
var
  LValue: Variant;
begin
  LValue := GetAbstractBestValue(
    { } function(AIndex: Integer): Variant
    { } begin
    { . } Result := FDirectlinkList[AIndex].Status;
    { } end);
  if VarIsNull(LValue) or not VarIsOrdinal(LValue) or not TEnum.IsValid<TContentStatus>(Integer(LValue)) then
    Result := csNotChecked
  else
    Result := LValue;
end;

function TIDirectlinkContainer.GetSize: Double;
begin
  Result := VarToFloatDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].Size;
      { } end), 0);
end;

function TIDirectlinkContainer.GetPartSize: Double;
begin
  Result := VarToFloatDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].PartSize;
      { } end), 0);
end;

function TIDirectlinkContainer.GetHoster: WideString;
begin
  Result := VarToStrDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].Hoster;
      { } end), '');
end;

function TIDirectlinkContainer.GetHosterShort: WideString;
begin
  Result := VarToStrDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].HosterShort;
      { } end), '');
end;

function TIDirectlinkContainer.GetParts: Integer;
begin
  Result := VarToIntDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].Parts;
      { } end), 0);
end;

function TIDirectlinkContainer.GetFileName: WideString;
begin
  Result := VarToStrDef(GetAbstractBestValue(
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FDirectlinkList[AIndex].FileName;
      { } end), '');
end;

function TIDirectlinkContainer.GetDirectlink(const Index: Integer): IDirectlink;
begin
  Result := FDirectlinkList[Index];
end;

function TIDirectlinkContainer.GetDirectlinkCount: Integer;
begin
  Result := FDirectlinkList.Count;
end;

function TIDirectlinkContainer.GetAbstractBestValue(AValueAtIndex: TAbstractIndexFunc): Variant;
var
  LIndex, LCount: Integer;
  FFound: Boolean;
begin
  Result := Null;

  LIndex := 0;
  LCount := DirectlinkCount;
  FFound := False;

  while (LIndex < LCount) and not FFound do
  begin
    FFound := HasUsefulValue(AValueAtIndex(LIndex));

    if not FFound then
    begin
      Inc(LIndex);
    end;
  end;

  if FFound then
  begin
    Result := AValueAtIndex(LIndex);
  end;
end;

constructor TIDirectlinkContainer.Create;
begin
  // DO NOT INHERIT
  FDirectlinkList := TList<IDirectlink>.Create;
end;

constructor TIDirectlinkContainer.Clone(const ADirectlinkContainer: IDirectlinkContainer);
var
  LIndex: Integer;
  LDirectlink: IDirectlink;
begin
  Create;

  for LIndex := 0 to ADirectlinkContainer.DirectlinkCount - 1 do
  begin
    LDirectlink := TIDirectlink.Clone(ADirectlinkContainer.Directlink[LIndex]);
    FDirectlinkList.Add(LDirectlink);
  end;
end;

destructor TIDirectlinkContainer.Destroy;
begin
  FDirectlinkList.Free;
  // DO NOT INHERIT
end;

{ TIMirrorContainer }

function TIMirrorContainer.GetValue: WideString;
begin
  Result := inherited GetValue;
end;

function TIMirrorContainer.GetStatus: TContentStatus;
var
  LValue: Variant;
begin
  LValue := GetAbstractBestValue(
    { } function: Variant
    { } begin
    { . } Result := inherited GetStatus;
    { } end,
    { } function(AIndex: Integer): Variant
    { } begin
    { . } Result := FCrypterList[AIndex].Status;
    { } end);
  if VarIsNull(LValue) or not VarIsOrdinal(LValue) or not TEnum.IsValid<TContentStatus>(Integer(LValue)) then
    Result := csNotChecked
  else
    Result := LValue;
end;

function TIMirrorContainer.GetSize: Double;
begin
  Result := VarToFloatDef(GetAbstractBestValue(
      { } function: Variant
      { } begin
      { . } Result := inherited GetSize;
      { } end,
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FCrypterList[AIndex].Size;
      { } end), 0);
end;

function TIMirrorContainer.GetPartSize: Double;
begin
  Result := VarToFloatDef(GetAbstractBestValue(
      { } function: Variant
      { } begin
      { . } Result := inherited GetPartSize;
      { } end,
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FCrypterList[AIndex].PartSize;
      { } end), 0);
end;

function TIMirrorContainer.GetHoster: WideString;
begin
  Result := VarToStrDef(GetAbstractBestValue(
      { } function: Variant
      { } begin
      { . } Result := inherited GetHoster;
      { } end,
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FCrypterList[AIndex].Hoster;
      { } end), '');
end;

function TIMirrorContainer.GetHosterShort: WideString;
begin
  Result := VarToStrDef(GetAbstractBestValue(
      { } function: Variant
      { } begin
      { . } Result := inherited GetHosterShort;
      { } end,
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FCrypterList[AIndex].HosterShort;
      { } end), '');
end;

function TIMirrorContainer.GetParts: Integer;
begin
  Result := VarToIntDef(GetAbstractBestValue(
      { } function: Variant
      { } begin
      { . } Result := inherited GetParts;
      { } end,
      { } function(AIndex: Integer): Variant
      { } begin
      { . } Result := FCrypterList[AIndex].Parts;
      { } end), 0);
end;

function TIMirrorContainer.GetFileName: WideString;
begin
  Result := inherited GetFileName;
end;

function TIMirrorContainer.GetCrypter(const IndexOrName: OleVariant): ICrypter;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    if VarIsNumeric(IndexOrName) then
      Result := FCrypterList[IndexOrName]
    else
      Result := FindCrypter(IndexOrName);
  end;
end;

function TIMirrorContainer.GetCrypterCount: Integer;
begin
  Result := FCrypterList.Count;
end;

function TIMirrorContainer.GetAbstractBestValue(AValue: TAbstractFunc; AValueAtIndex: TAbstractIndexFunc): Variant;
var
  LIndex, LCount: Integer;
  FFound: Boolean;
begin
  Result := AValue; // Best value from directlinks

  if not HasUsefulValue(Result) then
  begin
    LIndex := 0;
    LCount := CrypterCount;
    FFound := False;

    while (LIndex < LCount) and not FFound do
    begin
      FFound := HasUsefulValue(AValueAtIndex(LIndex));

      if not FFound then
      begin
        Inc(LIndex);
      end;
    end;

    if FFound then
    begin
      Result := AValueAtIndex(LIndex);
    end;
  end;
end;

constructor TIMirrorContainer.Create;
begin
  inherited Create;
  FCrypterList := TList<ICrypter>.Create;
end;

constructor TIMirrorContainer.Clone(const AMirrorContainer: IMirrorContainer);
var
  LIndex: Integer;
  LDirectlink: IDirectlink;
  LCrypter: ICrypter;
begin
  Create;

  // Elements of the IDirectlinkContainer have to be cloned, too.
  for LIndex := 0 to AMirrorContainer.DirectlinkCount - 1 do
  begin
    LDirectlink := TIDirectlink.Clone(AMirrorContainer.Directlink[LIndex]);
    // Access to FDirectlinkList of inherited object possible, because definition in same file.
    FDirectlinkList.Add(LDirectlink);
  end;

  for LIndex := 0 to AMirrorContainer.CrypterCount - 1 do
  begin
    LCrypter := TICrypter.Clone(AMirrorContainer.Crypter[LIndex]);
    FCrypterList.Add(LCrypter);
  end;
end;

destructor TIMirrorContainer.Destroy;
begin
  FCrypterList.Free;
  inherited Destroy;
end;

function TIMirrorContainer.FindCrypter(const AName: WideString): ICrypter;
var
  LIndex: Integer;
  LCrypter: ICrypter;
begin
  Result := nil;

  for LIndex := 0 to FCrypterList.Count - 1 do
  begin
    LCrypter := FCrypterList[LIndex];

    if SameText(AName, LCrypter.Name) then
    begin
      Result := LCrypter;
      break;
    end;
  end;
end;

end.
