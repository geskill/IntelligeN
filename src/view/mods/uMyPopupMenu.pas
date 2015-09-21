unit uMyPopupMenu;

interface

uses
  Classes, Menus, Clipbrd;

type
  TMyPopupMenu = class(TPopupMenu)
  public
    FmiUndo: TMenuItem;
    FmiCut: TMenuItem;
    FmiCopy: TMenuItem;
    FmiPaste: TMenuItem;
    FmiDelete: TMenuItem;
    FmiSelectAll: TMenuItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  uMain;

resourcestring
  StrUndo = 'Undo';
  StrCut = 'Cut';
  StrCopy = 'Copy';
  StrPaste = 'Paste';
  StrDelete = 'Delete';
  StrSelectAll = 'Select all';

constructor TMyPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with Items do
  begin
    FmiUndo := TMenuItem.Create(Self);
    with FmiUndo do
    begin
      Caption := StrUndo;
      ShortCut := Menus.ShortCut($5A, [ssCtrl]);
    end;
    Add(FmiUndo);

    InsertNewLineAfter(FmiUndo);

    FmiCut := TMenuItem.Create(Self);
    with FmiCut do
    begin
      Caption := StrCut;
      ImageIndex := 11;
      ShortCut := Menus.ShortCut($58, [ssCtrl]);
    end;
    Add(FmiCut);

    FmiCopy := TMenuItem.Create(Self);
    with FmiCopy do
    begin
      Caption := StrCopy;
      ImageIndex := 12;
      ShortCut := Menus.ShortCut($43, [ssCtrl]);
    end;
    Add(FmiCopy);

    FmiPaste := TMenuItem.Create(Self);
    with FmiPaste do
    begin
      Caption := StrPaste;
      ImageIndex := 13;
      ShortCut := Menus.ShortCut($56, [ssCtrl]);
    end;
    Add(FmiPaste);

    FmiDelete := TMenuItem.Create(Self);
    with FmiDelete do
    begin
      Caption := StrDelete;
      ImageIndex := 14;
      ShortCut := Menus.ShortCut($2E, [ssCtrl]);
    end;
    Add(FmiDelete);

    InsertNewLineAfter(FmiDelete);

    FmiSelectAll := TMenuItem.Create(Self);
    with FmiSelectAll do
    begin
      Caption := StrSelectAll;
      ImageIndex := 15;
      ShortCut := Menus.ShortCut($41, [ssCtrl]);
    end;
    Add(FmiSelectAll);
  end;
end;

destructor TMyPopupMenu.Destroy;
begin
  FmiCut.Free;
  FmiCopy.Free;
  FmiPaste.Free;
  FmiDelete.Free;
  FmiSelectAll.Free;

  inherited Destroy;
end;

end.
