unit uApiMirrorControlBase;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // Common
  uBaseConst, uBaseInterface,
  // Api
  uApiControlsBase;

type
  TIMirrorData = class(TIValueItem, IMirrorData)
  private
    FSize, FPartSize: Double;
    FHoster, FHosterShort: WideString;
    FParts: Integer;
  protected
    function GetSize: Double; virtual; safecall;
    function GetPartSize: Double; virtual; safecall;
    function GetHoster: WideString; virtual; safecall;
    function GetHosterShort: WideString; virtual; safecall;
    function GetParts: Integer; virtual; safecall;
  public
    constructor Create(ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = ''); reintroduce;
    constructor Clone(const AMirrorData: IMirrorData);
    destructor Destroy; override;

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
    constructor Create(AFileName: WideString; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = '');
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
    constructor Create(AName, AStatusImage, AStatusImageText: WideString; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = '');
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
    function GetSize: Double; override; safecall;
    function GetPartSize: Double; override; safecall;
    function GetHoster: WideString; override; safecall;
    function GetHosterShort: WideString; override; safecall;
    function GetParts: Integer; override; safecall;
    function GetFileName: WideString; override; safecall;

    function GetDirectlink(const Index: Integer): IDirectlink; safecall;
    function GetDirectlinkCount: Integer; safecall;
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
    function GetSize: Double; override; safecall;
    function GetPartSize: Double; override; safecall;
    function GetHoster: WideString; override; safecall;
    function GetHosterShort: WideString; override; safecall;
    function GetParts: Integer; override; safecall;

    function GetCrypter(const IndexOrName: OleVariant): ICrypter; safecall;
    function GetCrypterCount: Integer; safecall;
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

  FSize := ASize;
  FPartSize := APartSize;
  FHoster := AHoster;
  FHosterShort := AHosterShort;
  FParts := AParts;
end;

constructor TIMirrorData.Clone(const AMirrorData: IMirrorData);
begin
  Create(AMirrorData.Size, AMirrorData.PartSize, AMirrorData.Hoster, AMirrorData.HosterShort, AMirrorData.Parts, AMirrorData.Value);
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

constructor TIDirectlink.Create(AFileName: WideString; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString = '');
begin
  inherited Create(ASize, APartSize, AHoster, AHosterShort, AParts, AValue);

  FFileName := AFileName;
end;

constructor TIDirectlink.Clone(const ADirectlink: IDirectlink);
begin
  Create(ADirectlink.FileName, ADirectlink.Size, ADirectlink.PartSize, ADirectlink.Hoster, ADirectlink.HosterShort, ADirectlink.Parts, ADirectlink.Value);
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

constructor TICrypter.Create(AName, AStatusImage, AStatusImageText: WideString; ASize, APartSize: Double; AHoster, AHosterShort: WideString; AParts: Integer; AValue: WideString);
begin
  inherited Create(ASize, APartSize, AHoster, AHosterShort, AParts, AValue);

  FName := AName;
  FStatusImage := AStatusImage;
  FStatusImageText := AStatusImageText;
end;

constructor TICrypter.Clone(const ACrypter: ICrypter);
begin
  Create(ACrypter.Name, ACrypter.StatusImage, ACrypter.StatusImageText, ACrypter.Size, ACrypter.PartSize, ACrypter.Hoster, ACrypter.HosterShort, ACrypter.Parts, ACrypter.Value);
end;

destructor TICrypter.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{ ... }

{ TISubMirrorContainer }

function TIDirectlinkContainer.GetValue: WideString;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetSize: Double;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetPartSize: Double;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetHoster: WideString;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetHosterShort: WideString;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetParts: Integer;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetFileName: WideString;
begin
  // TODO: Implement
end;

function TIDirectlinkContainer.GetDirectlink(const Index: Integer): IDirectlink;
begin
  Result := FDirectlinkList[Index];
end;

function TIDirectlinkContainer.GetDirectlinkCount: Integer;
begin
  Result := FDirectlinkList.Count;
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
  // TODO: Implement
end;

function TIMirrorContainer.GetSize: Double;
begin
  // TODO: Implement
end;

function TIMirrorContainer.GetPartSize: Double;
begin
  // TODO: Implement
end;

function TIMirrorContainer.GetHoster: WideString;
begin
  // TODO: Implement
end;

function TIMirrorContainer.GetHosterShort: WideString;
begin
  // TODO: Implement
end;

function TIMirrorContainer.GetParts: Integer;
begin
  // TODO: Implement
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
