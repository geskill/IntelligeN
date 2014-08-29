unit uMirrorSort;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls, ComCtrls, Dialogs, Clipbrd, ShellAPI, Math,
  Generics.Collections,
  // Common
  uConst, uAppInterface,
  // RegExp
  RegExpr,
  // Mods
  uMycxRichEdit,
  // Utils
  uPathUtils, uSpecialStringUtils,
  // Plugin system
  uPlugInAppClass, Menus;

type
  TNotifyMethod = procedure(const Sender: IUnknown) of object;

  TINotifyEvent = class(TInterfacedObject, INotifyEvent)
  private
    FOnNotify: TNotifyMethod;
  public
    property OnNotifyHandler: TNotifyMethod read FOnNotify write FOnNotify;
    procedure OnNotify(const Sender: IUnknown); stdcall;
  end;

  TMirrorSort = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItem: IMenuItem;
    FNotifyEvent: TINotifyEvent;
    procedure OnClick(const Sender: IUnknown);
  public
    function GetName: WideString; override; safecall;
    function Start(const AAppController: IAppController): Boolean; override; stdcall;
    procedure Stop; override; stdcall;
  end;

  TMyTabSheet = class(TTabSheet)
  private
    FRichEdit: TRichEdit;
    FPartName: string;
  public
    constructor Create(AOwner: TComponent); override;
    property RichEdit: TRichEdit read FRichEdit;
    property PartName: string read FPartName write FPartName;
    destructor Destroy; override;
  end;

  TfMirrorSort = class(TForm)
    bFilter: TButton;
    pmLinks: TPopupMenu;
    nCut: TMenuItem;
    nCopy: TMenuItem;
    nPaste: TMenuItem;
    pcsubtabs: TPageControl;
    cbMultiUpload: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure pmLinksPopup(Sender: TObject);
    procedure nCutClick(Sender: TObject);
    procedure nCopyClick(Sender: TObject);
    procedure nPasteClick(Sender: TObject);
    procedure bFilterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    reLinks: TMycxRichEdit;
    FAppController: IAppController;
    procedure finduploadforlinks;
    procedure sorttotab(ALines: TStrings; ATabSheetController: ITabSheetController = nil);
    procedure reLinksDropFiles(var message: TMessage);
  public
    constructor Create(AOwner: TComponent; AAppController: IAppController); reintroduce;
    destructor Destroy; override;
  end;

var
  fMirrorSort: TfMirrorSort;

implementation

{$R *.dfm}

procedure TINotifyEvent.OnNotify;
begin
  if (@FOnNotify <> nil) then
    FOnNotify(Sender);
end;

procedure TMirrorSort.OnClick;
begin
  fMirrorSort := TfMirrorSort.Create(nil, FAppController);

  fMirrorSort.Show;
end;

function TMirrorSort.GetName;
begin
  Result := 'MirrorSort';
end;

function TMirrorSort.Start;
begin
  FAppController := AAppController;

  Result := True;

  FNotifyEvent := TINotifyEvent.Create;
  FNotifyEvent.OnNotifyHandler := OnClick;
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItem := InsertMenuItem(GetMenuItems.GetCount, 'MirrorSort', 'This sorts mirrors', Menus.ShortCut(Ord('D'), [ssAlt]), 22, 0, FNotifyEvent);
end;

procedure TMirrorSort.Stop;
begin
  if Assigned(fMirrorSort) then
  begin
    if fMirrorSort.Visible then
      fMirrorSort.Free;
    // fMirrorSort := nil;
  end;

  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItem);
  FNotifyEvent.OnNotifyHandler := nil;
  FNotifyEvent := nil;
  FAppController := nil;
end;

{ ****************************************************************************** }

{ TMyTabSheet }

constructor TMyTabSheet.Create(AOwner: TComponent);
begin
  inherited;

  FRichEdit := TRichEdit.Create(Self);
  with FRichEdit do
  begin
    Parent := Self;
    Align := alClient;
    ScrollBars := ssBoth;
  end;
  FPartName := '';
end;

destructor TMyTabSheet.Destroy;
begin
  FRichEdit.Free;
  inherited;
end;

{ ****************************************************************************** }

procedure TfMirrorSort.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure TfMirrorSort.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfMirrorSort.FormCreate(Sender: TObject);
begin
  reLinks := TMycxRichEdit.Create(Self);
  with reLinks do
  begin
    Parent := Self;
    TabOrder := 0;
    Left := 8;
    Top := 8;
    Height := 235;
    Width := 402;
    Anchors := [akLeft, akTop, akRight, akBottom];

    with Properties do
    begin
      MemoMode := True;

      ScrollBars := ssBoth;
    end;
    OnDropFiles := reLinksDropFiles;
  end;

  DragAcceptFiles(reLinks.Handle, True);
end;

procedure TfMirrorSort.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(reLinks.Handle, False);
  reLinks.Free;
end;

procedure TfMirrorSort.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TfMirrorSort.pmLinksPopup(Sender: TObject);
begin
  nCut.Enabled := reLinks.SelText <> '';

  nCopy.Enabled := reLinks.SelText <> '';

  nPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TfMirrorSort.nCutClick(Sender: TObject);
begin
  reLinks.CutToClipboard;
end;

procedure TfMirrorSort.nCopyClick(Sender: TObject);
begin
  reLinks.CopyToClipboard;
end;

procedure TfMirrorSort.nPasteClick(Sender: TObject);
begin
  reLinks.PasteFromClipboard;
end;

procedure TfMirrorSort.bFilterClick(Sender: TObject);

  function GetRealFileName(AFileName: string): string;
  begin
    AFileName := Trim(AFileName);
    if SameText(ExtractFileExt(AFileName), '.htm') or SameText(ExtractFileExt(AFileName), '.html') then
      AFileName := ChangeFileExt(AFileName, '');
    Result := ChangeFileExt(ExtractUrlFileName(AFileName), '');
    if not(Pos('part', ExtractFileExt(Result)) = 0) then
      Result := ChangeFileExt(Result, '');
  end;

var
  _needmultitabs, _neednewtab: Boolean;
  Y, Z: Integer;
begin
  with FAppController.PageController do
  begin
    if TabSheetCount > 0 then
    begin
      if reLinks.Visible then
      begin
        _needmultitabs := False;
        Z := 0;
        while (not _needmultitabs) and (Z < reLinks.Lines.Count) do
        begin
          if not(GetRealFileName(reLinks.Lines.Strings[Z]) = '') then
            _needmultitabs := not SameText(GetRealFileName(reLinks.Lines.Strings[Z]), GetRealFileName(reLinks.Lines.Strings[0]));
          Inc(Z);
        end;

        if _needmultitabs and (cbMultiUpload.Checked) then
        begin
          reLinks.Visible := False;
          pcsubtabs.Visible := True;

          for Y := 0 to reLinks.Lines.Count - 1 do
          begin
            _neednewtab := False;
            for Z := 0 to pcsubtabs.PageCount - 1 do
            begin
              _neednewtab := not SameText(GetRealFileName(reLinks.Lines.Strings[Y]), TMyTabSheet(pcsubtabs.Pages[Z]).PartName);
              if not _neednewtab then
              begin
                TMyTabSheet(pcsubtabs.Pages[Z]).RichEdit.Lines.Add(reLinks.Lines.Strings[Y]);
                break;
              end;
            end;
            if _neednewtab or (pcsubtabs.PageCount = 0) then
              with TMyTabSheet.Create(pcsubtabs) do
              begin
                PageControl := pcsubtabs;
                Caption := GetRealFileName(reLinks.Lines.Strings[Y]);
                PartName := GetRealFileName(reLinks.Lines.Strings[Y]);
                RichEdit.Lines.Add(reLinks.Lines.Strings[Y]);
              end;
          end;
          finduploadforlinks;
        end
        else
        begin
          sorttotab(reLinks.Lines);
          Close;
        end;
      end
      else
      begin
        sorttotab(TMyTabSheet(pcsubtabs.ActivePage).RichEdit.Lines);
        TMyTabSheet(pcsubtabs.ActivePage).Free;
      end;
    end
    else
      MessageDlg('First of all add a new tab!', mtWarning, [mbOK], 0);
  end;
end;

procedure TfMirrorSort.finduploadforlinks;
var
  I, J: Integer;
  _FileName: string;
begin
  for I := pcsubtabs.PageCount - 1 downto 0 do
  begin
    _FileName := TMyTabSheet(pcsubtabs.Pages[I]).PartName;
    with FAppController.PageController do
      for J := 0 to TabSheetCount - 1 do
        if (TabSheetController[J].ReleaseName = _FileName) then
        begin
          sorttotab(TMyTabSheet(pcsubtabs.Pages[I]).RichEdit.Lines, TabSheetController[J]);
          TMyTabSheet(pcsubtabs.Pages[I]).Free;
        end;
  end;
  if (pcsubtabs.PageCount = 0) then
    Close;
end;

procedure TfMirrorSort.sorttotab(ALines: TStrings; ATabSheetController: ITabSheetController = nil);
var
  _MirrorIndex, _LinksToSortIndex, _LinkListIndex: Integer;
  _LinkList: TList<string>;
  _Found: Boolean;

  FirstEmptyMirrorIndex: Integer;
  _Mirror, _Link: string;
begin
  if not Assigned(ATabSheetController) then
    ATabSheetController := FAppController.PageController.ActiveTabSheetController;

  _LinkList := TList<string>.Create;
  try
    with ATabSheetController.MirrorController do
    begin
      for _MirrorIndex := 0 to MirrorCount - 1 do
        _LinkList.Add(Mirror[_MirrorIndex].DirectlinksMirror[0]);
    end;

    for _LinksToSortIndex := 0 to ALines.Count - 1 do
    begin
      if SameStr('', Trim(ALines.Strings[_LinksToSortIndex])) then
        Continue;
      _Found := False;

      FirstEmptyMirrorIndex := -1;
      for _LinkListIndex := 0 to _LinkList.Count - 1 do
      begin
        _Mirror := _LinkList[_LinkListIndex];
        if not SameStr('', _Mirror) then
        begin
          _Link := _Mirror;
          if not(Pos(sLineBreak, _Link) = 0) then
            _Link := copy(_Link, 1, Pos(sLineBreak, _Link) - 1);

          if SameText(RemoveW(ExtractUrlHost(_Link)), RemoveW(ExtractUrlHost(ALines.Strings[_LinksToSortIndex]))) then
          begin
            with TStringList.Create do
              try
                Text := _Mirror;
                Add(ALines.Strings[_LinksToSortIndex]);
                _LinkList[_LinkListIndex] := Text;
              finally
                Free;
              end;
            _Found := True;
            break;
          end;
        end
        else
        begin
          if FirstEmptyMirrorIndex = -1 then
            FirstEmptyMirrorIndex := _LinkListIndex
          else
            FirstEmptyMirrorIndex := Min(FirstEmptyMirrorIndex, _LinkListIndex);
        end;
      end;

      if not _Found then
      begin
        if not(FirstEmptyMirrorIndex = -1) then
        begin
          _LinkList[FirstEmptyMirrorIndex] := ALines.Strings[_LinksToSortIndex];
          _Found := True;
        end;
      end;
      if not _Found then
        _LinkList.Add(ALines.Strings[_LinksToSortIndex]);
    end;

    for _LinkListIndex := 0 to _LinkList.Count - 1 do
    begin
      with ATabSheetController.MirrorController do
        if MirrorCount <= _LinkListIndex then
          Mirror[Add].Directlink.Add(_LinkList.Items[_LinkListIndex])
        else
          Mirror[_LinkListIndex].DirectlinksMirror[0] := _LinkList.Items[_LinkListIndex];
    end;
    FAppController.PageController.CallComponentparser;
  finally
    _LinkList.Free;
  end;
end;

procedure TfMirrorSort.reLinksDropFiles(var message: TMessage);
var
  FileCount, Size: Integer;
  Filename: PChar;
begin
  FileCount := DragQueryFile(message.WParam, $FFFFFFFF, nil, 255);

  if (FileCount = 1) then
  begin
    Size := DragQueryFile(message.WParam, 0, nil, 0) + 1;
    Filename := StrAlloc(Size);

    if DragQueryFile(message.WParam, 0, Filename, Size) = 1 then
      { nothing } ;

    reLinks.Lines.LoadFromFile(Filename);

    StrDispose(Filename);
  end;

  DragFinish(message.WParam);
end;

constructor TfMirrorSort.Create(AOwner: TComponent; AAppController: IAppController);
begin
  inherited Create(AOwner);

  FAppController := AAppController;
end;

destructor TfMirrorSort.Destroy;
begin
  FAppController := nil;

  inherited;
end;

end.
