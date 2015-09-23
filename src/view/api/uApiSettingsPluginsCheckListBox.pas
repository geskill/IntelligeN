unit uApiSettingsPluginsCheckListBox;

interface

uses
  // Delphi
  Windows, Classes, Controls, ExtCtrls, Menus, Types, ImgList,
  // DevExpress
  cxButtons, cxCheckListBox, dxBar,
  // Api
  uApiConst;

type
  TPluginsCheckListBoxEndDragEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;

  TPluginsCheckListBox = class
  private
    FPanel: TPanel;
    FCheckListBox: TcxCheckListBox;
    FImageList: TImageList;
    FPopupMenu, FAddPopupMenu: TdxBarPopupMenu; // TPopupMenu;
    FUpItem, FDownItem, FAddAllPluginsItem: TdxBarButton; // TMenuItem;
    FAddButton, FRemoveButton: TcxButton;
    FDragDropIndex: Integer;

    FOnClick: TNotifyEvent;
    FOnEndDrag: TPluginsCheckListBoxEndDragEvent;
    FOnRemovePluginClick, FOnRemovedPluginClick: TNotifyEvent;

    procedure DropDownMenuPopup(Sender: TObject; var APopupMenu: TPopupMenu; var AHandled: Boolean);

    function GetParent: TWinControl;
    procedure SetParent(AParent: TWinControl);
    function GetLeft: Integer;
    procedure SetLeft(ALeft: Integer);
    function GetTop: Integer;
    procedure SetTop(ATop: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AWidth: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AHeight: Integer);
    function GetDragDrop: Boolean;
    procedure SetDragDrop(ADragDrop: Boolean);

    function GetClickCheck: TcxClickCheckEvent;
    procedure SetClickCheck(AClickCheck: TcxClickCheckEvent);
    function GetAddPluginClick: TNotifyEvent;
    procedure SetAddPluginClick(AAddPluginClick: TNotifyEvent);
    function GetAddAllPluginClick: TNotifyEvent;
    procedure SetAddAllPluginClick(AAddAllPluginClick: TNotifyEvent);
  protected
    procedure PopupMenuPopUp(Sender: TObject);
    procedure UpItemClick(Sender: TObject);
    procedure DownItemClick(Sender: TObject);
    procedure Click(Sender: TObject);
    procedure RemovePluginClick(Sender: TObject);
    procedure DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure EndDrag(Sender, Target: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;

    property Parent: TWinControl read GetParent write SetParent;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property DragDrop: Boolean read GetDragDrop write SetDragDrop;
    property InnerCheckListBox: TcxCheckListBox read FCheckListBox;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnClickCheck: TcxClickCheckEvent read GetClickCheck write SetClickCheck;
    property OnEndDrag: TPluginsCheckListBoxEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnAddPluginClick: TNotifyEvent read GetAddPluginClick write SetAddPluginClick;
    property OnAddAllPluginClick: TNotifyEvent read GetAddAllPluginClick write SetAddAllPluginClick;
    property OnRemovePluginClick: TNotifyEvent read FOnRemovePluginClick write FOnRemovePluginClick;
    property OnRemovedPluginClick: TNotifyEvent read FOnRemovedPluginClick write FOnRemovedPluginClick;
  end;

implementation

{ TPluginsCheckListBox }

procedure TPluginsCheckListBox.DropDownMenuPopup(Sender: TObject; var APopupMenu: TPopupMenu; var AHandled: Boolean);
var
  APoint: TPoint;
begin
  with TcxButton(Sender) do
  begin
    APoint.X := Left;
    APoint.Y := Top + Height;
    APoint := Parent.ClientToScreen(APoint);
    FAddPopupMenu.Popup(APoint.X, APoint.Y);
    AHandled := True;
  end;
end;

function TPluginsCheckListBox.GetParent: TWinControl;
begin
  result := FPanel.Parent;
end;

procedure TPluginsCheckListBox.SetParent(AParent: TWinControl);
begin
  FPanel.Parent := AParent;
end;

function TPluginsCheckListBox.GetLeft: Integer;
begin
  result := FPanel.Left;
end;

procedure TPluginsCheckListBox.SetLeft(ALeft: Integer);
begin
  FPanel.Left := ALeft;
end;

function TPluginsCheckListBox.GetTop: Integer;
begin
  result := FPanel.Top;
end;

procedure TPluginsCheckListBox.SetTop(ATop: Integer);
begin
  FPanel.Top := ATop;
end;

function TPluginsCheckListBox.GetWidth: Integer;
begin
  result := FPanel.Width;
end;

procedure TPluginsCheckListBox.SetWidth(AWidth: Integer);
begin
  FPanel.Width := AWidth;

  FRemoveButton.Left := FPanel.Width - FRemoveButton.Width;
end;

function TPluginsCheckListBox.GetHeight: Integer;
begin
  result := FPanel.Height;
end;

procedure TPluginsCheckListBox.SetHeight(AHeight: Integer);
begin
  FPanel.Height := AHeight;

  FAddButton.Top := FPanel.Height - FAddButton.Height - 8;
  FRemoveButton.Top := FPanel.Height - FRemoveButton.Height - 8;
end;

function TPluginsCheckListBox.GetDragDrop: Boolean;
begin
  result := FCheckListBox.DragMode = dmAutomatic;
end;

procedure TPluginsCheckListBox.SetDragDrop(ADragDrop: Boolean);
begin
  with FCheckListBox do
    case ADragDrop of
      True:
        DragMode := dmAutomatic;
      False:
        DragMode := dmManual;
    end;
end;

function TPluginsCheckListBox.GetClickCheck: TcxClickCheckEvent;
begin
  result := FCheckListBox.OnClickCheck;
end;

procedure TPluginsCheckListBox.SetClickCheck(AClickCheck: TcxClickCheckEvent);
begin
  FCheckListBox.OnClickCheck := AClickCheck;
end;

function TPluginsCheckListBox.GetAddPluginClick: TNotifyEvent;
begin
  result := FAddButton.OnClick;
end;

procedure TPluginsCheckListBox.SetAddPluginClick(AAddPluginClick: TNotifyEvent);
begin
  FAddButton.OnClick := AAddPluginClick;
end;

function TPluginsCheckListBox.GetAddAllPluginClick: TNotifyEvent;
begin
  result := FAddAllPluginsItem.OnClick;
end;

procedure TPluginsCheckListBox.SetAddAllPluginClick(AAddAllPluginClick: TNotifyEvent);
begin
  FAddAllPluginsItem.OnClick := AAddAllPluginClick;
end;

procedure TPluginsCheckListBox.PopupMenuPopUp(Sender: TObject);
var
  p: TPoint;
  i: Integer;
begin
  p := FCheckListBox.ScreenToClient(Mouse.CursorPos);
  i := FCheckListBox.ItemAtPos(p, True);

  if not(i = -1) then
    FCheckListBox.Selected[i] := True;

  FUpItem.Enabled := not(i = -1) and DragDrop;
  FDownItem.Enabled := not(i = -1) and DragDrop;
end;

procedure TPluginsCheckListBox.UpItemClick(Sender: TObject);
begin
  if DragDrop then
    with FCheckListBox do
      if (ItemIndex > 0) then
      begin
        if Assigned(FOnEndDrag) then
          FOnEndDrag(Sender, ItemIndex, ItemIndex - 1);

        Items[ItemIndex].index := ItemIndex - 1;
        Selected[ItemIndex - 1] := True;
      end;
end;

procedure TPluginsCheckListBox.DownItemClick(Sender: TObject);
begin
  if DragDrop then
    with FCheckListBox do
      if (ItemIndex < Count - 1) then
      begin
        if Assigned(FOnEndDrag) then
          FOnEndDrag(Sender, ItemIndex, ItemIndex + 1);

        Items[ItemIndex].index := ItemIndex + 1;
        Selected[ItemIndex + 1] := True;
      end;
end;

procedure TPluginsCheckListBox.Click(Sender: TObject);
begin
  FRemoveButton.Enabled := not(FCheckListBox.ItemIndex = -1);

  if Assigned(FOnClick) then
    FOnClick(Sender);
end;

procedure TPluginsCheckListBox.RemovePluginClick(Sender: TObject);
begin
  if Assigned(FOnRemovePluginClick) then
    FOnRemovePluginClick(Self);
  with FCheckListBox do
    Items[ItemIndex].Free;
  if Assigned(FOnRemovedPluginClick) then
    FOnRemovedPluginClick(Self);

  FRemoveButton.Enabled := not(FCheckListBox.ItemIndex = -1);
end;

procedure TPluginsCheckListBox.DragOver(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  FDragDropIndex := FCheckListBox.ItemAtPos(Point(X, Y), True);
  if (FDragDropIndex <> -1) and (FDragDropIndex <> FCheckListBox.ItemIndex) and (FCheckListBox.ItemIndex <> -1) then
  begin
    Accept := True;
  end
  else
    FDragDropIndex := -1;
end;

procedure TPluginsCheckListBox.EndDrag(Sender: TObject; Target: TObject; X: Integer; Y: Integer);
begin
  if FDragDropIndex <> -1 then
    with FCheckListBox do
    begin
      if Assigned(FOnEndDrag) then
        FOnEndDrag(Sender, ItemIndex, FDragDropIndex);

      Items[ItemIndex].index := FDragDropIndex;
      Selected[FDragDropIndex] := True;
      FDragDropIndex := -1;
    end;
end;

constructor TPluginsCheckListBox.Create;
begin
  FDragDropIndex := -1;

  FPanel := TPanel.Create(AOwner);
  with FPanel do
  begin
    Parent := AOwner;

    Anchors := [akLeft, akTop, akBottom];

    BevelOuter := bvNone;

    Height := 100;
    Width := 100;

    Caption := '';
    ParentColor := True;
  end;

  FImageList := TImageList.Create(FPanel);
  with FImageList do
  begin
    ColorDepth := cd32Bit;
    DrawingStyle := dsTransparent;
  end;

  FPopupMenu := TdxBarPopupMenu.Create(FPanel);
  with FPopupMenu do
  begin
    FUpItem := TdxBarButton.Create(FAddPopupMenu);
    with FUpItem do
    begin
      Caption := 'Up';
      ShortCut := Menus.ShortCut(VK_UP, [ssCtrl]);
      OnClick := UpItemClick;
    end;
    ItemLinks.Add.Item := FUpItem;

    FDownItem := TdxBarButton.Create(FAddPopupMenu);
    with FDownItem do
    begin
      Caption := 'Down';
      ShortCut := Menus.ShortCut(VK_DOWN, [ssCtrl]);
      OnClick := DownItemClick;
    end;
    ItemLinks.Add.Item := FDownItem;
    OnPopup := PopupMenuPopUp;
  end;

  FCheckListBox := TcxCheckListBox.Create(FPanel);
  with FCheckListBox do
  begin
    Parent := FPanel;

    Anchors := [akLeft, akTop, akRight, akBottom];

    Height := 73;
    Width := 100;

    PopupMenu := FPopupMenu;

    Images := FImageList;

    OnClick := Click;
    // OnClickCheck := ;
    OnDragOver := DragOver;
    OnEndDrag := Self.EndDrag;
  end;

  // FPopupMenu := TPopupMenu.Create(FPanel);
  FAddPopupMenu := TdxBarPopupMenu.Create(FPanel);
  with FAddPopupMenu do
  begin
    FAddAllPluginsItem := TdxBarButton.Create(FAddPopupMenu);
    with FAddAllPluginsItem do
    begin
      Caption := 'Add all';
    end;
    with ItemLinks.Add do
    begin
      // index := FmiUndo.index;
      Item := FAddAllPluginsItem;
    end;

    {
      with Items do
      begin
      FAddAllPluginsItem := TMenuItem.Create(FPopupMenu);
      with FAddAllPluginsItem do
      begin
      Caption := 'Add all';
      OnClick := nil;
      end;
      Add(FAddAllPluginsItem);
      end;
      }
  end;

  FAddButton := TcxButton.Create(FPanel);
  with FAddButton do
  begin
    Parent := FPanel;

    Anchors := [akLeft, akBottom];

    Kind := cxbkDropDownButton;

    // DropDownMenu := FPopupMenu;

    Left := 0;
    Top := FPanel.Height - Height - 8;
    Width := 50;
    Height := 16;

    Caption := strAdd;

    OnDropDownMenuPopup := DropDownMenuPopup;
  end;

  FRemoveButton := TcxButton.Create(FPanel);
  with FRemoveButton do
  begin
    Parent := FPanel;

    Anchors := [akLeft, akBottom];

    Caption := StrRemove;
    Enabled := False;

    Left := FPanel.Width - Width;
    Top := FPanel.Height - Height - 8;
    Width := 50;
    Height := 16;

    OnClick := RemovePluginClick;
  end;

end;

destructor TPluginsCheckListBox.Destroy;
begin
  FRemoveButton.Free;
  FAddButton.Free;
  FAddAllPluginsItem.Free;
  FAddPopupMenu.Free;
  FCheckListBox.Free;
  FImageList.Free;
  FPanel.Free;

  inherited;
end;

end.
