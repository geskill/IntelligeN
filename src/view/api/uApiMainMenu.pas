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
  uAppInterface;

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
    function GetMenuItems: IMenuItems; stdcall;
    function InsertMenuItem(aIndex: Integer; const aCaption: WideString; const aHint: WideString; aShortCut: Word; aImageIndex: Integer;
      aTag: Integer; const aOnClick: INotifyEventHandler): IMenuItem; stdcall;
    function GetIndex: Integer; stdcall;
    function GetName: WideString; stdcall;
    function GetCaption: WideString; stdcall;
    procedure SetCaption(const aValue: WideString); stdcall;
    function GetHint: WideString; stdcall;
    procedure SetHint(const aValue: WideString); stdcall;
    function GetShortCut: Word; stdcall;
    procedure SetShortCut(aValue: Word); stdcall;
    function GetImageIndex: Integer; stdcall;
    procedure SetImageIndex(aImageIndex: Integer); stdcall;
    function GetTag: Integer; stdcall;
    procedure SetTag(aValue: Integer); stdcall;
    function GetOnClick: INotifyEventHandler; stdcall;
    procedure SetOnClick(const aValue: INotifyEventHandler); stdcall;
  end;

  TIMenuItems = class(TInterfacedObject, IMenuItems)
  private
    FMenuItems: TdxBarItemLinks;
  public
    constructor Create(AMenuItems: TdxBarItemLinks);
    function GetCount: Integer; stdcall;
    function GetItem(index: Integer): IMenuItem; stdcall;
    function RemoveItem(const aMenuItem: IMenuItem): WordBool; stdcall;
  end;

  TIMainMenu = class(TInterfacedObject, IMainMenu)
  private
    FMainMenu: TdxBar;
  public
    constructor Create(AMainMenu: TdxBar);
    function GetMenuItems: IMenuItems; stdcall;
    function InsertMenuItem(aIndex: Integer; const aCaption: WideString; const aHint: WideString; aShortCut: Word; aImageIndex: Integer;
      aTag: Integer; const aOnClick: INotifyEventHandler; const ASubMenuItem: WordBool = True): IMenuItem; stdcall;
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
  Result := TIMenuItem.Create(FMenuItems.Items[index].Item);
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
