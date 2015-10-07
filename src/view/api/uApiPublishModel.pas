unit uApiPublishModel;

interface

uses
  // Delphi
  SysUtils, Variants, Generics.Collections,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

type
  TIMirrorData = class(TInterfacedObject, IMirrorData)
  private
    FSize, FPartSize: Extended;
    FHoster, FHosterShort: WideString;
    FParts: Integer;
  protected
    function GetSize: Extended; safecall;
    function GetPartSize: Extended; safecall;
    function GetHoster: WideString; safecall;
    function GetHosterShort: WideString; safecall;
    function GetParts: Integer; safecall;
  public
    constructor Create(ASize, APartSize: Extended; AHoster, AHosterShort: WideString; AParts: Integer);
    property Size: Extended read GetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
  end;

  TIControlContainer = class(TInterfacedObject, IControlContainer)
  private
    FComponentID: TControlID;
    FValue: WideString;
  protected
    function GetControlID: TControlID; safecall;
    function GetValue: WideString; safecall;
  public
    constructor Create(AComponentID: TControlID; AValue: WideString);
    property AControlID: TControlID read GetControlID;
    property Value: WideString read GetValue;
  end;

  TIMirrorItem = class(TIMirrorData, IMirrorData)
  private
    FValue: WideString;
  protected
    function GetValue: WideString; safecall;
  public
    constructor Create(ASize, APartSize: Extended; AHoster, AHosterShort, AValue: WideString; AParts: Integer);

    property Value: WideString read GetValue;
  end;

  TICrypter = class(TIMirrorItem, ICrypter)
  private
    FName, FStatusImage, FStatusImageText: WideString;
  protected
    function GetName: WideString; safecall;
    function GetStatusImage: WideString; safecall;
    function GetStatusImageText: WideString; safecall;
  public
    constructor Create(ASize, APartSize: Extended; AHoster, AHosterShort, AValue, AName, AStatusImage, AStatusImageText: WideString; AParts: Integer);

    property Name: WideString read GetName;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
  end;

  TIDirectlink = class(TIMirrorItem, IDirectlink)

  end;

  TIMirrorContainer = class(TIMirrorData, IMirrorContainer)
  private
    FCrypterList: TList<ICrypter>;
    FDirectlinkList: TList<IDirectlink>;
  protected
    function GetCrypter(const IndexOrName: OleVariant): ICrypter; safecall;
    function GetCrypterCount: Integer; safecall;
    function GetDirectlink(const Index: Integer): IDirectlink; safecall;
    function GetDirectlinkCount: Integer; safecall;
  public
    constructor Create(AMirrorControl: IMirrorControl);
    property Crypter[const IndexOrName: OleVariant]: ICrypter read GetCrypter;
    property CrypterCount: Integer read GetCrypterCount;
    property Directlink[const Index: Integer]: IDirectlink read GetDirectlink;
    property DirectlinkCount: Integer read GetDirectlinkCount;
    destructor Destroy; override;
  end;

implementation

{$REGION 'TIMirrorData'}
{ TIMirrorData }

function TIMirrorData.GetSize: Extended;
begin
  Result := FSize;
end;

function TIMirrorData.GetPartSize: Extended;
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
  inherited Create;

  FSize := ASize;
  FPartSize := APartSize;
  FHoster := AHoster;
  FHosterShort := AHosterShort;
  FParts := AParts;
end;
{$ENDREGION}
{$REGION 'TIControlContainer'}

{ TIControlContainer }
function TIControlContainer.GetControlID: TControlID;
begin
  Result := FComponentID;
end;

function TIControlContainer.GetValue: WideString;
begin
  Result :=  FValue;
end;

constructor TIControlContainer.Create;
begin
  inherited Create;

  FComponentID :=  AComponentID;
  FValue :=  AValue;
end;
{$ENDREGION}
{$REGION 'TIMirrorItem'}
{ TIMirrorItem }

function TIMirrorItem.GetValue: WideString;
begin
  Result := FValue;
end;

constructor TIMirrorItem.Create;
begin
  inherited Create(ASize, APartSize, AHoster, AHosterShort, AParts);

  FValue := AValue;
end;
{$ENDREGION}
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
  inherited Create(ASize, APartSize, AHoster, AHosterShort, AValue, AParts);

  FName := AName;
  FStatusImage := AStatusImage;
  FStatusImageText := AStatusImageText;
end;
{$ENDREGION}
{$REGION 'TIMirrorContainer'}
{ TIMirrorContainer }

function TIMirrorContainer.GetCrypter(const IndexOrName: OleVariant): ICrypter;

  function Find(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with FCrypterList do
      for I := 0 to Count - 1 do
        if SameText(AName, Items[I].Name) then
          Exit(I);
  end;

var
  Index: Integer;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    Index := -1;
    if VarIsNumeric(IndexOrName) then
      Index := IndexOrName
    else
      Index := Find(IndexOrName);

    if not((Index < 0) or (Index > FCrypterList.Count)) then
      Result := FCrypterList.Items[Index];
  end;
end;

function TIMirrorContainer.GetCrypterCount: Integer;
begin
  Result := FCrypterList.Count;
end;

function TIMirrorContainer.GetDirectlink(const Index: Integer): IDirectlink;
begin
  Result := FDirectlinkList.Items[Index];
end;

function TIMirrorContainer.GetDirectlinkCount: Integer;
begin
  Result := FDirectlinkList.Count;
end;

constructor TIMirrorContainer.Create;
var
  I: Integer;
begin
  inherited Create(AMirrorControl.Size, AMirrorControl.PartSize, AMirrorControl.Hoster, AMirrorControl.GetHoster(True), AMirrorControl.Parts);

  FCrypterList := TList<ICrypter>.Create;
  FDirectlinkList := TList<IDirectlink>.Create;

  with AMirrorControl do
  begin
    for I := 0 to CrypterCount - 1 do
      with Crypter[I] do
        FCrypterList.Add(TICrypter.Create(Size, PartSize, Hoster, HosterShort, Link, Name, StatusImage, StatusImageText, Parts));

    for I := 0 to DirectlinksMirrorCount - 1 do
      with Directlink.Mirror[I] do
        FDirectlinkList.Add(TIDirectlink.Create(Size, PartSize, Hoster, HosterShort, Value, Parts));
  end;

end;

destructor TIMirrorContainer.Destroy;
begin
  FCrypterList.Free;
  FDirectlinkList.Free;

  inherited Destroy;
end;
{$ENDREGION}

end.
