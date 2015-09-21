unit uMydxBarPopupMenu;

interface

uses
  // Delphi
  Classes, Menus,
  // Dev Express
  dxBar;

type
  TMydxBarPopupMenu = class(TdxBarPopupMenu)
  public
    FmiUndo: TdxBarButton;
    FmiS1: TdxBarSeparator;
    FmiCut: TdxBarButton;
    FmiCopy: TdxBarButton;
    FmiPaste: TdxBarButton;
    FmiDelete: TdxBarButton;
    FmiS2: TdxBarSeparator;
    FmiSelectAll: TdxBarButton;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

resourcestring
  StrUndo = 'Undo';
  StrCut = 'Cut';
  StrCopy = 'Copy';
  StrPaste = 'Paste';
  StrDelete = 'Delete';
  StrSelectAll = 'Select all';

constructor TMydxBarPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FmiUndo := TdxBarButton.Create(Self);
  with FmiUndo do
  begin
    Caption := StrUndo;
    ShortCut := Menus.ShortCut($5A, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiUndo.index;
    Item := FmiUndo;
  end;

  FmiS1 := TdxBarSeparator.Create(Self);
  with FmiS1 do
  begin
    ShowCaption := False;
  end;
  with ItemLinks.Add do
  begin
    // index := FmiS1.index;
    Item := FmiS1;
  end;

  FmiCut := TdxBarButton.Create(Self);
  with FmiCut do
  begin
    Caption := StrCut;
    ImageIndex := 7;
    ShortCut := Menus.ShortCut($58, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiCut.index;
    Item := FmiCut;
  end;

  FmiCopy := TdxBarButton.Create(Self);
  with FmiCopy do
  begin
    Caption := StrCopy;
    ImageIndex := 8;
    ShortCut := Menus.ShortCut($43, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiCopy.index;
    Item := FmiCopy;
  end;

  FmiPaste := TdxBarButton.Create(Self);
  with FmiPaste do
  begin
    Caption := StrPaste;
    ImageIndex := 9;
    ShortCut := Menus.ShortCut($56, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiPaste.index;
    Item := FmiPaste;
  end;

  FmiDelete := TdxBarButton.Create(Self);
  with FmiDelete do
  begin
    Caption := StrDelete;
    ImageIndex := 10;
    ShortCut := Menus.ShortCut($2E, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiDelete.index;
    Item := FmiDelete;
  end;

  FmiS2 := TdxBarSeparator.Create(Self);
  with FmiS2 do
  begin
    ShowCaption := False;
  end;
  with ItemLinks.Add do
  begin
    // index := FmiS2.index;
    Item := FmiS2;
  end;

  FmiSelectAll := TdxBarButton.Create(Self);
  with FmiSelectAll do
  begin
    Caption := StrSelectAll;
    ImageIndex := 11;
    ShortCut := Menus.ShortCut($41, [ssCtrl]);
  end;
  with ItemLinks.Add do
  begin
    // index := FmiSelectAll.index;
    Item := FmiSelectAll;
  end;
end;

destructor TMydxBarPopupMenu.Destroy;
begin
  FmiUndo.Free;
  FmiS1.Free;
  FmiCut.Free;
  FmiCopy.Free;
  FmiPaste.Free;
  FmiDelete.Free;
  FmiS2.Free;
  FmiSelectAll.Free;

  inherited Destroy;
end;

end.
