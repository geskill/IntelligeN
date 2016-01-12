unit uApiTabSheetController;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Graphics, Controls, Messages,
  // Dev Express
  cxPC,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiPlugins, uApiTabSheetItem,
  // MultiEvent
  Generics.MultiEvents.Handler, Generics.MultiEvents.NotifyHandler,
  // Plugin
  uPlugInEvent;

type
  TReleaseNameChangeMethod = procedure(const AReleaseName: WideString) of object;

  TIReleaseNameChangeEventHandler = class(TGenericEventHandler<TReleaseNameChangeMethod>, IReleaseNameChange)
  public
    procedure Invoke(const AReleaseName: WideString); safecall;
  end;

  TTabSheetController = class(TcxTabSheet, ITabSheetController)
  private
    FPageController: IPageController;
    FTypeID: TTypeID;

    FFileName, FFileType, FReleaseName, FTemplateFileName: string;
    FInitialized, FDataChanged: Boolean;

    FDataTabSheetItem: TDataTabSheetItem;
    FDesignTabSheetItem: TDesignTabSheetItem;

    FIReleaseNameChange: TIReleaseNameChangeEventHandler;
    FIControlChange: TIControlEventHandler;
    FIMirrorChange: TINotifyEventHandler;

    FIUpdateCMSListEvent: IUpdateCMSListEventHandler;
    FIUpdateCMSWebsiteListEvent: IUpdateCMSWebsiteListEventHandler;
  protected
    function GetPageController: IPageController;
    function GetIsTabActive: WordBool;
    procedure SetIsTabActive(ATabActive: WordBool);
    function GetTabSheetIndex: Integer;
    procedure SetTabSheetIndex(ATabSheetIndex: Integer);
    function GetViewType: TTabViewType;
    procedure SetViewType(AViewType: TTabViewType);
    function GetFileName: WideString;
    procedure SetFileName(const AFileName: WideString);
    function GetFileType: WideString;
    procedure SetFileType(const AFileType: WideString);
    function GetReleaseName: WideString;
    procedure SetReleaseName(const AReleaseName: WideString);
    function GetReleaseNameShort: WideString;
    function GetDataChanged: Boolean;
    procedure SetDataChanged(ADataChanged: Boolean);
    function GetTypeID: TTypeID;
    function GetActiveWebsite: WideString;
    procedure SetActiveWebsite(const AWebsite: WideString);

    procedure UpdateCaption;

    procedure ReleaseNameChange(const AReleaseName: WideString);
    procedure ControlChange(const Sender: IControlBasic);
    procedure MirrorChange(const Sender: IUnknown);

    function GetControlController: IControlController;
    function GetMirrorController: IMirrorController;
    function GetPublishController: IPublishController;
  public
    constructor Create(const AOwner: TcxPageControl; APageController: IPageController; ATypeID: TTypeID); reintroduce;

    procedure Install;
    procedure AddEvents;

    property DataTabSheetItem: TDataTabSheetItem read FDataTabSheetItem;
    property DesignTabSheetItem: TDesignTabSheetItem read FDesignTabSheetItem;

    property PageController: IPageController read GetPageController;

    property IsTabActive: WordBool read GetIsTabActive write SetIsTabActive;
    property TabSheetIndex: Integer read GetTabSheetIndex write SetTabSheetIndex;

    property ViewType: TTabViewType read GetViewType write SetViewType;

    property FileName: WideString read GetFileName write SetFileName;
    property FileType: WideString read GetFileType write SetFileType;

    property ReleaseName: WideString read GetReleaseName write SetReleaseName;
    property ReleaseNameShort: WideString read GetReleaseNameShort;

    procedure Save(const AFileName, AFileType: WideString);
    procedure Initialized(); overload;
    procedure Initialized(const AFileName, AFileType: WideString); overload;
    procedure ResetControlFocused();

    property DataChanged: Boolean read GetDataChanged write SetDataChanged;

    property TemplateFileName: string read FTemplateFileName write FTemplateFileName;
    property TypeID: TTypeID read GetTypeID write FTypeID;

    property ActiveWebsite: WideString read GetActiveWebsite write SetActiveWebsite;

    property ControlController: IControlController read GetControlController;
    property MirrorController: IMirrorController read GetMirrorController;
    property PublishController: IPublishController read GetPublishController;

    procedure RemoveEvents;
    destructor Destroy; override;
  end;

implementation

uses
  uMain,
  // Api
  uApiSettings;

{ TIReleaseNameChangeEventHandler }

procedure TIReleaseNameChangeEventHandler.Invoke;
begin
  if (@FHandler <> nil) then
    FHandler(AReleaseName);
end;

function MinimizeReleaseName(const AReleaseName: string; AMaxLength: Integer): string;
begin
  if length(AReleaseName) > AMaxLength then
    Result := copy(AReleaseName, 1, AMaxLength) + '...'
  else
    Result := AReleaseName;
end;

function TTabSheetController.GetPageController;
begin
  Result := FPageController;
end;

function TTabSheetController.GetIsTabActive;
begin
  Result := (Self = PageControl.ActivePage);
end;

procedure TTabSheetController.SetIsTabActive(ATabActive: WordBool);
begin
  if ATabActive and not IsTabActive then
    PageControl.ActivePage := Self;
end;

function TTabSheetController.GetTabSheetIndex: Integer;
begin
  Result := PageIndex;
end;

procedure TTabSheetController.SetTabSheetIndex(ATabSheetIndex: Integer);
begin
  PageIndex := ATabSheetIndex;
end;

function TTabSheetController.GetViewType;
begin
  if DataTabSheetItem.Visible then
    Result := vtData
  else
    Result := DesignTabSheetItem.ViewType;
end;

procedure TTabSheetController.SetViewType(AViewType: TTabViewType);
begin
  case AViewType of
    vtData:
      begin
        DesignTabSheetItem.Visible := False;
        DataTabSheetItem.Visible := True;
      end;
    vtCode, vtPreview:
      begin
        DataTabSheetItem.Visible := False;
        DesignTabSheetItem.ViewType := AViewType;
        DesignTabSheetItem.Visible := True;
      end;
  end;
end;

function TTabSheetController.GetFileName;
begin
  Result := FFileName;
end;

procedure TTabSheetController.SetFileName(const AFileName: WideString);
begin
  FFileName := AFileName;
end;

function TTabSheetController.GetFileType;
begin
  Result := FFileType;
end;

procedure TTabSheetController.SetFileType(const AFileType: WideString);
begin
  FFileType := AFileType;
end;

function TTabSheetController.GetReleaseName: WideString;
begin
  Result := FReleaseName;
end;

procedure TTabSheetController.SetReleaseName(const AReleaseName: WideString);
begin
  FReleaseName := AReleaseName;
  UpdateCaption;
  TabHint := ReleaseName;
end;

function TTabSheetController.GetReleaseNameShort: WideString;
begin
  if not SameStr('', ReleaseName) then
    Result := MinimizeReleaseName(ReleaseName, 30)
  else
    Result := StrReleaseNameEmpty;
end;

function TTabSheetController.GetDataChanged: Boolean;
begin
  Result := FDataChanged;
end;

procedure TTabSheetController.SetDataChanged(ADataChanged: Boolean);
begin
  FDataChanged := ADataChanged;
  UpdateCaption;
end;

function TTabSheetController.GetTypeID: TTypeID;
begin
  Result := FTypeID;
end;

function TTabSheetController.GetActiveWebsite: WideString;
begin
  Result := FDesignTabSheetItem.ActiveWebsite;
end;

procedure TTabSheetController.SetActiveWebsite(const AWebsite: WideString);
begin
  FDesignTabSheetItem.ActiveWebsite := AWebsite;
end;

procedure TTabSheetController.UpdateCaption;
begin
  if FInitialized then
  begin
    Self.Caption := ReleaseNameShort + IfThen(DataChanged, '*');
    if IsTabActive then
      Main.UpdateCaption(Self.Caption); // TODO: Impelment this better in 130
  end;
end;

procedure TTabSheetController.ReleaseNameChange(const AReleaseName: WideString);
begin
  ReleaseName := AReleaseName;
end;

procedure TTabSheetController.ControlChange(const Sender: IControlBasic);
begin
  DataChanged := True;
end;

procedure TTabSheetController.MirrorChange(const Sender: IInterface);
begin
  DataChanged := True;
end;

function TTabSheetController.GetControlController;
begin
  Result := DataTabSheetItem.ControlController;
end;

function TTabSheetController.GetMirrorController;
begin
  Result := DataTabSheetItem.MirrorController;
end;

function TTabSheetController.GetPublishController;
begin
  Result := DesignTabSheetItem.PublishController;
end;

constructor TTabSheetController.Create(const AOwner: TcxPageControl; APageController: IPageController; ATypeID: TTypeID);
begin
  inherited Create(AOwner);

  FPageController := APageController;
  FTypeID := ATypeID;

  ParentColor := False;
  Color := clWhite;
  // Enabled := False; cause issue with focused controls
  ImageIndex := Integer(ATypeID);

  FInitialized := False;
  FDataChanged := False;
end;

procedure TTabSheetController.Install;
begin
  FDataTabSheetItem := TDataTabSheetItem.Create(Self, Self);

  FDesignTabSheetItem := TDesignTabSheetItem.Create(Self, Self);

  AddEvents;
end;

procedure TTabSheetController.AddEvents;
begin
  FIReleaseNameChange := TIReleaseNameChangeEventHandler.Create(ReleaseNameChange);
  ControlController.OnReleaseNameChange := FIReleaseNameChange;

  FIControlChange := TIControlEventHandler.Create(ControlChange);
  ControlController.OnControlChange.Add(FIControlChange);

  FIMirrorChange := TINotifyEventHandler.Create(MirrorChange);
  MirrorController.OnChange.Add(FIMirrorChange);

  FIUpdateCMSListEvent := TIUpdateCMSListEventHandler.Create(FDesignTabSheetItem.UpdateCMSList);
  PublishController.OnUpdateCMSList.Add(FIUpdateCMSListEvent);

  FIUpdateCMSWebsiteListEvent := TIUpdateCMSWebsiteListEventHandler.Create(FDesignTabSheetItem.UpdateCMSWebsiteList);
  PublishController.OnUpdateCMSWebsiteList.Add(FIUpdateCMSWebsiteListEvent);
end;

procedure TTabSheetController.Save(const AFileName, AFileType: WideString);
begin
  with SettingsManager.Settings.Plugins do
    TPluginBasic.SaveFile(FindPlugInCollectionItemFromCollection(AFileType, FileFormats), AFileName, GetTemplatesTypeFolder + TemplateFileName + '.xml', Self);
  FileName := AFileName;
  FileType := AFileType;
  DataChanged := False;
end;

procedure TTabSheetController.Initialized;
begin
  Initialized('', '');
end;

procedure TTabSheetController.Initialized(const AFileName, AFileType: WideString);
begin
  FileName := AFileName;
  FileType := AFileType;
  FInitialized := True;
  DataChanged := False;
  ResetControlFocused();
  PublishController.Active := True;

  // Enabled := True;

  PageController.OnAddTab.Invoke(Self);
end;

procedure TTabSheetController.ResetControlFocused;
begin
  if SettingsManager.Settings.ControlAligner.MirrorPosition = mpTop then
  begin
    if (MirrorController.MirrorCount > 0) then
      MirrorController.Mirror[0].Focus := True
    else if (ControlController.ControlCount > 0) then
      ControlController.Control[0].Focus := True;
  end
  else
  begin
    if (ControlController.ControlCount > 0) then
      ControlController.Control[0].Focus := True
    else if (MirrorController.MirrorCount > 0) then
      MirrorController.Mirror[0].Focus := True;
  end;
end;

procedure TTabSheetController.RemoveEvents;
begin
  /// removing tabs with active crawler is forbidden, nevertheless additionally blacklist entry
  PageController.CrawlerManager.RemoveCrawlerJob(ControlController);

  ControlController.OnReleaseNameChange := nil;

  PublishController.OnUpdateCMSWebsiteList.Remove(FIUpdateCMSWebsiteListEvent);
  PublishController.OnUpdateCMSList.Remove(FIUpdateCMSListEvent);

  MirrorController.OnChange.Remove(FIMirrorChange);
  ControlController.OnControlChange.Remove(FIControlChange);
end;

destructor TTabSheetController.Destroy;
begin
  /// doing this not in a separate function causes the interfaces MirrorController
  /// and ControlController not to destroy immediately
  RemoveEvents;

  FIUpdateCMSListEvent := nil;
  FIUpdateCMSWebsiteListEvent := nil;

  FIMirrorChange := nil;
  FIControlChange := nil;
  FIReleaseNameChange := nil;

  FDesignTabSheetItem.Free;
  FDataTabSheetItem.Free;

  FPageController := nil;

  inherited Destroy;
end;

end.
