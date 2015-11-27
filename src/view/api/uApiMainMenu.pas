unit uApiMainMenu;

interface

uses
  // Delphi
  SysUtils, Generics.Collections, Dialogs,
  // Dev Express
  dxBar,
  // MultiEvent
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

type
  TStoredMenuItem = class
  private
    FButton: TdxBarItem;
    FOnMenuClick: INotifyEventHandler;
    procedure OnClick(Sender: TObject);
  public
    constructor Create(AButton: TdxBarItem);
    property OnMenuClick: INotifyEventHandler read FOnMenuClick write FOnMenuClick;
  end;

  TIMenuItem = class(TInterfacedObject, IMenuItem)
  private
    FButton: TdxBarItem;
  public
    constructor Create(AButton: TdxBarItem);
    function GetMenuItems: IMenuItems; safecall;
    function InsertMenuItem(AIndex: Integer; const ACaption: WideString; const AHint: WideString; AShortCut: Integer; AImageIndex: Integer; ATag: Integer; const AOnClick: INotifyEventHandler): IMenuItem; safecall;
    function GetIndex: Integer; safecall;
    function GetName: WideString; safecall;
    function GetCaption: WideString; safecall;
    procedure SetCaption(const AValue: WideString); safecall;
    function GetHint: WideString; safecall;
    procedure SetHint(const AValue: WideString); safecall;
    function GetShortCut: Integer; safecall;
    procedure SetShortCut(AValue: Integer); safecall;
    function GetImageIndex: Integer; safecall;
    procedure SetImageIndex(AImageIndex: Integer); safecall;
    function GetTag: Integer; safecall;
    procedure SetTag(AValue: Integer); safecall;
    function GetOnClick: INotifyEventHandler; safecall;
    procedure SetOnClick(const AValue: INotifyEventHandler); safecall;
  end;

  TIMenuItems = class(TInterfacedObject, IMenuItems)
  private
    FMenuItems: TdxBarItemLinks;
  public
    constructor Create(AMenuItems: TdxBarItemLinks);
    function GetCount: Integer; safecall;
    function GetItem(AIndex: Integer): IMenuItem; safecall;
    function RemoveItem(const AMenuItem: IMenuItem): WordBool; safecall;
  end;

  TIMainMenu = class(TInterfacedObject, IMainMenu)
  private
    FMainMenu: TdxBar;
  public
    constructor Create(AMainMenu: TdxBar);
    function GetMenuItems: IMenuItems; safecall;
    function InsertMenuItem(AIndex: Integer; const ACaption: WideString; const AHint: WideString; AShortCut: Integer; AImageIndex: Integer; ATag: Integer; const AOnClick: INotifyEventHandler; const ASubMenuItem: WordBool = True): IMenuItem; safecall;
  end;

implementation

var
  StoredMenuItems: TList<TStoredMenuItem>;

  { TStoredMenuItem }

procedure TStoredMenuItem.OnClick(Sender: TObject);
var
  MenuItem: IMenuItem;
begin
  MenuItem := TIMenuItem.Create(FButton);
  try
    if Assigned(FOnMenuClick) then
      FOnMenuClick.Invoke(MenuItem);
  finally
    MenuItem := nil;
  end;
end;

constructor TStoredMenuItem.Create(AButton: TdxBarItem);
begin
  inherited Create;
  FButton := AButton;

  AButton.OnClick := OnClick;
end;

{ ****************************************************************************** }
{$REGION 'TIMenuItem'}

constructor TIMenuItem.Create;
begin
  FButton := AButton;
end;

function TIMenuItem.GetMenuItems;
begin
  if FButton.InheritsFrom(TdxBarSubItem) then
    Result := TIMenuItems.Create((FButton as TdxBarSubItem).ItemLinks);
end;

function TIMenuItem.InsertMenuItem;

  function GetNewName(APrefix: string): string;

    function search(ANewID: Integer): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      for I := 0 to StoredMenuItems.Count - 1 do
        if APrefix + IntToStr(I) = StoredMenuItems.Items[I].FButton.name then
          Result := True;
    end;

  var
    _NewID: Integer;
  begin
    _NewID := StoredMenuItems.Count + 1;
    while search(_NewID) do
      Inc(_NewID);

    Result := APrefix + IntToStr(_NewID);
  end;

var
  dxBarItem: TdxBarButton;
  StoredMenuItem: TStoredMenuItem;
begin
  if FButton.InheritsFrom(TdxBarSubItem) then
    dxBarItem := TdxBarButton.Create(FButton);
  // else
  // dxBarSubItem := TdxBarSubItem.Create();

  with dxBarItem do
  begin
    Caption := aCaption;
    name := GetNewName(copy(ClassName, 3));
    Hint := aHint;
    index := aIndex;
    ImageIndex := aImageIndex;
    ShortCut := aShortCut;
    Tag := aTag;
  end;
  with (FButton as TdxBarSubItem).ItemLinks.Add do
  begin
    index := dxBarItem.index;
    Item := dxBarItem;
  end;

  StoredMenuItem := TStoredMenuItem.Create(dxBarItem);
  StoredMenuItem.OnMenuClick := aOnClick;
  StoredMenuItems.Add(StoredMenuItem);

  Result := TIMenuItem.Create(dxBarItem);
end;

function TIMenuItem.GetIndex;
begin
  Result := FButton.index;
end;

function TIMenuItem.GetName;
begin
  Result := FButton.name;
end;

function TIMenuItem.GetCaption;
begin
  Result := FButton.Caption;
end;

procedure TIMenuItem.SetCaption;
begin
  FButton.Caption := aValue;
end;

function TIMenuItem.GetHint;
begin
  Result := FButton.Hint;
end;

procedure TIMenuItem.SetHint;
begin
  FButton.Hint := aValue;
end;

function TIMenuItem.GetShortCut;
begin
  Result := FButton.ShortCut;
end;

procedure TIMenuItem.SetShortCut;
begin
  FButton.ShortCut := aValue;
end;

function TIMenuItem.GetImageIndex;
begin
  Result := FButton.ImageIndex;
end;

procedure TIMenuItem.SetImageIndex;
begin
  FButton.ImageIndex := aImageIndex;
end;

function TIMenuItem.GetTag;
begin
  Result := FButton.Tag;
end;

procedure TIMenuItem.SetTag;
begin
  FButton.Tag := aValue;
end;

function TIMenuItem.GetOnClick;
begin
  Result := nil;
end;

procedure TIMenuItem.SetOnClick;
begin
  // FNotifyEvent := aValue;
end;
{$ENDREGION}

{ ****************************************************************************** }
constructor TIMenuItems.Create;
begin
  FMenuItems := AMenuItems;
end;

function TIMenuItems.GetCount;
begin
  Result := FMenuItems.Count;
end;

function TIMenuItems.GetItem;
begin
  Result := TIMenuItem.Create(FMenuItems.Items[AIndex].Item);
end;

function TIMenuItems.RemoveItem;
var
  I: Integer;
begin
  Result := False;

  with StoredMenuItems do
    for I := 0 to Count - 1 do
      if Items[I].FButton.name = aMenuItem.GetName then
      begin
        Items[I].FButton.Free;
        Items[I].Free;
        Delete(I);
        Result := True;
        break;
      end;
end;

{ ****************************************************************************** }
constructor TIMainMenu.Create;
begin
  FMainMenu := AMainMenu;
end;

function TIMainMenu.GetMenuItems;
begin
  Result := TIMenuItems.Create(FMainMenu.ItemLinks);
end;

function TIMainMenu.InsertMenuItem;
var
  dxBarItem: TdxBarItem;
  StoredMenuItem: TStoredMenuItem;
begin
  case ASubMenuItem of
    True:
      dxBarItem := TdxBarSubItem.Create(FMainMenu);
    False:
      dxBarItem := TdxBarButton.Create(FMainMenu);
  end;

  with dxBarItem do
  begin
    Caption := aCaption;
    Hint := aHint;
    index := aIndex;
    ImageIndex := aImageIndex;
    ShortCut := aShortCut;
    Tag := aTag;
  end;
  with FMainMenu.ItemLinks.Add do
  begin
    index := dxBarItem.index;
    Item := dxBarItem;
  end;

  StoredMenuItem := TStoredMenuItem.Create(dxBarItem);
  StoredMenuItem.OnMenuClick := aOnClick;

  Result := TIMenuItem.Create(dxBarItem);
end;

initialization

StoredMenuItems := TList<TStoredMenuItem>.Create;

finalization

StoredMenuItems.Free;

end.
