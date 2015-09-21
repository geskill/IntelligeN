unit uMycxTabSheet;

interface

uses
  // Delphi
  Windows, SysUtils, Messages, Classes, Forms, Controls, ExtCtrls, Menus, Graphics,
  // Dev Express
  cxControls, dxBar, cxPC, cxScrollBox,
  // Common
  uConst, uAppInterface,
  // Api
  uApiConst, uApiMirrorController, uApiComponentController, uApiPublishController, uApiSettings, uApiPlugins,
  // Plugin
  uPlugInEvent;

type
  TReleaseNameChange = procedure(const AReleaseName: WideString) of object;

  TIReleaseNameChange = class(TInterfacedObject, IReleaseNameChange)
  private
    FOnNotify: TReleaseNameChange;
  public
    property OnNotifyHandler: TReleaseNameChange read FOnNotify write FOnNotify;
    procedure OnNotify(const AReleaseName: WideString); stdcall;
  end;

  TPopupMenuChange = procedure(const Sender: Integer) of object;

  TIPopupMenuChange = class(TInterfacedObject, IPopupMenuChange)
  private
    FOnNotify: TPopupMenuChange;
  public
    property OnNotifyHandler: TPopupMenuChange read FOnNotify write FOnNotify;
    procedure OnNotify(const Sender: Integer); stdcall;
  end;

  TMycxTabSheet = class(TcxTabSheet, ITabSheetController)
  private
    FPageController: IPageController;
    FScrollBoxData: TcxScrollBox;
    FPopupMenu: TPopupMenu;
    FmiAddMirror: TMenuItem;
    FFileName, FFileType, FReleaseName, FTemplateFileName: string;
    FDataChanged: Boolean;
    FTemplateTypeID: TTemplateTypeID;
    FComponentController: IComponentController;
    FISpaceMouseDown, FIMirrorChange: TINotifyEvent;
    FIPopupMenuChange: TIPopupMenuChange;
    FIControlChange, FIControlEnter, FIControlExit: TIControlEvent;
    FIReleaseNameChange: TIReleaseNameChange;
    FMirrorController: IMirrorController;
    FPublishController: IPublishController;
    FPanelDesign, FPanelPreview: TPanel;
    procedure MycxTabSheetShow(OnShow: TObject);
    procedure FScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FmiAddMirrorClick(Sender: TObject);
    procedure SpaceMouseDown(const Sender: IUnknown);
    procedure ReleaseNameChange(const AReleaseName: WideString);
    procedure ControlChange(const Sender: IBasic);
    procedure ControlEnter(const Sender: IBasic);
    procedure ControlExit(const Sender: IBasic);
    procedure MirrorChange(const Sender: IUnknown);
    procedure PopupMenuChange(const Sender: Integer);
    function GetPageController: IPageController;
    function GetViewType: TViewType;
    procedure SetViewType(AViewType: TViewType);
    function GetFileName: WideString;
    procedure SetFileName(AFileName: WideString);
    function GetFileType: WideString;
    procedure SetFileType(AFileType: WideString);
    function GetReleaseName: WideString;
    procedure SetReleaseName(AFReleaseName: WideString);
    procedure SetDataChanged(ADataChanged: Boolean);
    procedure UpdateCaption;
    procedure MoveWorkWhileHoldingLeftMouse;
    function GetComponentController: IComponentController;
    procedure SetComponentController(const AComponentController: IComponentController);
    function GetMirrorController: IMirrorController;
    procedure SetMirrorController(const AMirrorController: IMirrorController);
    function GetPublishController: IPublishController;
    procedure SetPublishController(const APublishController: IPublishController);
  public
    constructor Create(AOwner: TComponent; APageController: IPageController; ATemplateTypeID: TTemplateTypeID); reintroduce;
    property WorkPanel: TcxScrollBox read FScrollBoxData;
    property PageController: IPageController read GetPageController;
    property ViewType: TViewType read GetViewType;
    property FileName: WideString read GetFileName write SetFileName;
    property FileType: WideString read GetFileType write SetFileType;
    property ReleaseName: WideString read GetReleaseName write SetReleaseName;
    procedure Save(AFileName, AFileType: WideString);
    procedure ResetDataChanged(AFileName, AFileType: WideString);
    property DataChanged: Boolean read FDataChanged write SetDataChanged;
    property TemplateFileName: string read FTemplateFileName write FTemplateFileName;
    property TemplateTypeID: TTemplateTypeID read FTemplateTypeID write FTemplateTypeID;
    property ComponentController: IComponentController read GetComponentController write SetComponentController;
    property MirrorController: IMirrorController read GetMirrorController write SetMirrorController;
    property PublishController: IPublishController read GetPublishController write SetPublishController;
    destructor Destroy; override;
  end;

implementation

uses
  uMain;

procedure TIReleaseNameChange.OnNotify;
begin
  if (@FOnNotify <> nil) then
    FOnNotify(AReleaseName);
end;

procedure TIPopupMenuChange.OnNotify(const Sender: Integer);
begin
  if (@FOnNotify <> nil) then
    FOnNotify(Sender);
end;

function MinimizeReleaseName(AReleaseName: string; AMaxLength: Integer): string;
begin
  if length(AReleaseName) > AMaxLength then
    Result := copy(AReleaseName, 1, AMaxLength) + '...'
  else
    Result := AReleaseName;
end;

procedure TMycxTabSheet.MycxTabSheetShow(OnShow: TObject);
begin
  Main.fControlEditor.Control := nil;
  FScrollBoxData.SetParentComponent(Self);
end;

procedure TMycxTabSheet.FScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MoveWorkWhileHoldingLeftMouse;
end;

procedure TMycxTabSheet.FmiAddMirrorClick(Sender: TObject);
begin
  MirrorController.Add;
  Main.fMain.CallComponentparser;
end;

procedure TMycxTabSheet.SpaceMouseDown(const Sender: IInterface);
begin
  MoveWorkWhileHoldingLeftMouse;
end;

procedure TMycxTabSheet.ReleaseNameChange(const AReleaseName: WideString);
begin
  ReleaseName := AReleaseName;
end;

procedure TMycxTabSheet.PopupMenuChange(const Sender: Integer);
begin
  Main.SetEditMenu(TdxBarItemLinks(Sender));
end;

procedure TMycxTabSheet.ControlChange(const Sender: IBasic);
begin
  DataChanged := True;
end;

procedure TMycxTabSheet.ControlEnter(const Sender: IBasic);
begin
  Main.fControlEditor.Control := Sender;
end;

procedure TMycxTabSheet.ControlExit(const Sender: IBasic);
begin
  // unused
end;

procedure TMycxTabSheet.MirrorChange(const Sender: IInterface);
begin
  DataChanged := True;
end;

function TMycxTabSheet.GetPageController;
begin
  Result := FPageController;
end;

function TMycxTabSheet.GetViewType;
begin
  //
end;

procedure TMycxTabSheet.SetViewType(AViewType: TViewType);
begin
  //
end;

function TMycxTabSheet.GetFileName;
begin
  Result := FFileName;
end;

procedure TMycxTabSheet.SetFileName(AFileName: WideString);
begin
  FFileName := AFileName;
end;

function TMycxTabSheet.GetFileType;
begin
  Result := FFileType;
end;

procedure TMycxTabSheet.SetFileType(AFileType: WideString);
begin
  FFileType := AFileType;
end;

function TMycxTabSheet.GetReleaseName: WideString;
begin
  Result := FReleaseName;
end;

procedure TMycxTabSheet.SetReleaseName(AFReleaseName: WideString);
begin
  FReleaseName := AFReleaseName;
  UpdateCaption;
  TabHint := FReleaseName;
end;

procedure TMycxTabSheet.SetDataChanged(ADataChanged: Boolean);
begin
  FDataChanged := ADataChanged;
  UpdateCaption;
end;

procedure TMycxTabSheet.UpdateCaption;
begin
  if not(ReleaseName = '') then
    Self.Caption := MinimizeReleaseName(ReleaseName, 30)
  else
    Self.Caption := '<ReleaseName>';
  if DataChanged then
    Self.Caption := Self.Caption + '*';
end;

procedure TMycxTabSheet.MoveWorkWhileHoldingLeftMouse;
begin
  if SettingsManager.Settings.MoveWorkWhileHoldingLeftMouse and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
  begin
    ReleaseCapture;
    SendMessage(Main.Handle, WM_SYSCOMMAND, SC_MOVE + HTCAPTION, 0);
  end;
end;

function TMycxTabSheet.GetComponentController;
begin
  Result := FComponentController;
end;

procedure TMycxTabSheet.SetComponentController(const AComponentController: IComponentController);
begin
  FComponentController := AComponentController;
end;

function TMycxTabSheet.GetMirrorController;
begin
  Result := FMirrorController;
end;

procedure TMycxTabSheet.SetMirrorController(const AMirrorController: IMirrorController);
begin
  FMirrorController := AMirrorController;
end;

function TMycxTabSheet.GetPublishController;
begin
  Result := FPublishController;
end;

procedure TMycxTabSheet.SetPublishController(const APublishController: IPublishController);
begin
  FPublishController := APublishController;
end;

constructor TMycxTabSheet.Create;
begin
  inherited Create(AOwner);

  FPageController := APageController;

  ImageIndex := Integer(ATemplateTypeID);
  TemplateTypeID := ATemplateTypeID;

  OnShow := MycxTabSheetShow;

  FPopupMenu := TPopupMenu.Create(Self);
  with FPopupMenu.Items do
  begin
    FmiAddMirror := TMenuItem.Create(FPopupMenu);
    with FmiAddMirror do
    begin
      Caption := StrAdd;
      OnClick := FmiAddMirrorClick;
    end;
    Add(FmiAddMirror);
  end;

  FScrollBoxData := TcxScrollBox.Create(Self);
  with FScrollBoxData do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := cxcbsNone;
    Color := clWhite;

    HorzScrollBar.Visible := False;
    VertScrollBar.Tracking := True;

    PopupMenu := FPopupMenu;

    OnMouseDown := FScrollBoxMouseDown;
  end;

  FISpaceMouseDown := TINotifyEvent.Create;
  FISpaceMouseDown.OnNotifyHandler := SpaceMouseDown;

  FIPopupMenuChange := TIPopupMenuChange.Create;
  FIPopupMenuChange.OnNotifyHandler := PopupMenuChange;

  ComponentController := TComponentController.Create(FScrollBoxData);
  with ComponentController do
  begin
    TabSheetController := Self;
    TemplateTypeID := Self.TemplateTypeID;

    OnSpaceMouseDown := FISpaceMouseDown;

    FIControlChange := TIControlEvent.Create;
    FIControlChange.OnNotifyHandler := ControlChange;
    OnControlChange.Add(FIControlChange);

    FIControlEnter := TIControlEvent.Create;
    FIControlEnter.OnNotifyHandler := ControlEnter;
    OnControlEnter := FIControlEnter;

    FIControlExit := TIControlEvent.Create;
    FIControlExit.OnNotifyHandler := ControlExit;
    OnControlExit := FIControlExit;

    FIReleaseNameChange := TIReleaseNameChange.Create;
    FIReleaseNameChange.OnNotifyHandler := ReleaseNameChange;
    OnReleaseNameChange := FIReleaseNameChange;

    OnPopupMenuChange := FIPopupMenuChange;
  end;

  MirrorController := TMirrorController.Create(FScrollBoxData);
  with MirrorController do
  begin
    TabSheetController := Self;

    OnSpaceMouseDown := FISpaceMouseDown;

    FIMirrorChange := TINotifyEvent.Create;
    FIMirrorChange.OnNotifyHandler := MirrorChange;
    OnChange.Add(FIMirrorChange);

    OnPopupMenuChange := FIPopupMenuChange;
  end;

  FPublishController := TIPublishController.Create(Self);

  UpdateCaption;
end;

procedure TMycxTabSheet.Save(AFileName: WideString; AFileType: WideString);
begin
  with SettingsManager.Settings.Plugins do
    TApiPlugin.SaveFile(FindPlugInCollectionItemFromCollection(AFileType, FileFormats), AFileName, GetTemplatesTypeFolder + TemplateFileName + '.xml', Self);
  FileName := AFileName;
  FileType := AFileType;
  DataChanged := False;
end;

procedure TMycxTabSheet.ResetDataChanged(AFileName, AFileType: WideString);
begin
  FileName := AFileName;
  FileType := AFileType;
  DataChanged := False;
end;

destructor TMycxTabSheet.Destroy;
var
  I, J: Integer;
  Picture: IPicture;
begin
  PageController.CrawlerManager.RemoveCrawlerJob(ComponentController);

  with Main do
  begin
    SetEditMenu(nil);
    fControlEditor.Control := nil;
  end;

  FPublishController := nil;

  with MirrorController do
  begin
    OnSpaceMouseDown := nil;

    OnChange.Remove(FIMirrorChange);
    // OnChange := nil;

    OnPopupMenuChange := nil;

    for I := MirrorCount - 1 downto 0 do
      Remove(I);

    TabSheetController := nil;
  end;
  MirrorController := nil;

  with ComponentController do
  begin
    for I := 0 to ControlCount - 1 do
    begin
      if Control[I].QueryInterface(IPicture, Picture) = 0 then
      begin
        for J := 0 to Picture.MirrorCount - 1 do
          Picture.Mirror[J].Picture := nil;
        Picture := nil;
      end;
      Control[I].ComponentController := nil;
    end;

    OnSpaceMouseDown := nil;

    OnControlChange.Remove(FIControlChange);
    // OnControlChange := nil;

    OnControlEnter := nil;

    OnControlExit := nil;

    OnReleaseNameChange := nil;

    OnPopupMenuChange := nil;

    TabSheetController := nil;
  end;
  ComponentController := nil;

  FISpaceMouseDown := nil;
  FIMirrorChange := nil;
  FIPopupMenuChange := nil;
  FIControlChange := nil;
  FIControlEnter := nil;
  FIControlExit := nil;
  FIReleaseNameChange := nil;

  FScrollBoxData.Free;

  FmiAddMirror.Free;
  FPopupMenu.Free;

  FPageController := nil;

  inherited Destroy;
end;

end.
