unit uApiTabSheetController;

interface

uses
  // Delphi
  Windows, SysUtils, Classes,
  // Dev Express
  cxPC,
  // Common
  uBase, uConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiSettings, uApiPlugins, uApiTabSheetItem,
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
    FTemplateTypeID: TTemplateTypeID;

    FFileName, FFileType, FReleaseName, FTemplateFileName: string;
    FDataChanged: Boolean;

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
    function GetTabSheetIndex: Integer;
    function GetViewType: TViewType;
    procedure SetViewType(AViewType: TViewType);
    function GetFileName: WideString;
    procedure SetFileName(AFileName: WideString);
    function GetFileType: WideString;
    procedure SetFileType(AFileType: WideString);
    function GetReleaseName: WideString;
    procedure SetReleaseName(AReleaseName: WideString);
    function GetDataChanged: Boolean;
    procedure SetDataChanged(ADataChanged: Boolean);
    function GetTemplateTypeID: TTemplateTypeID;
    function GetActiveWebsite: WideString;
    procedure SetActiveWebsite(AWebsite: WideString);

    procedure UpdateCaption;

    procedure ReleaseNameChange(const AReleaseName: WideString);
    procedure ControlChange(const Sender: IBasic);
    procedure MirrorChange(const Sender: IUnknown);

    function GetComponentController: IComponentController;
    function GetMirrorController: IMirrorController;
    function GetPublishController: IPublishController;
  public
    constructor Create(AOwner: TComponent; APageController: IPageController; ATemplateTypeID: TTemplateTypeID); reintroduce;
    procedure Install;
    procedure AddEvents;

    property DataTabSheetItem: TDataTabSheetItem read FDataTabSheetItem;
    property DesignTabSheetItem: TDesignTabSheetItem read FDesignTabSheetItem;

    property PageController: IPageController read GetPageController;

    property IsTabActive: WordBool read GetIsTabActive;
    property TabSheetIndex: Integer read GetTabSheetIndex;

    property ViewType: TViewType read GetViewType write SetViewType;

    property FileName: WideString read GetFileName write SetFileName;
    property FileType: WideString read GetFileType write SetFileType;

    property ReleaseName: WideString read GetReleaseName write SetReleaseName;

    procedure Save(AFileName, AFileType: WideString);
    procedure ResetDataChanged(AFileName, AFileType: WideString);

    property DataChanged: Boolean read GetDataChanged write SetDataChanged;

    property TemplateFileName: string read FTemplateFileName write FTemplateFileName;
    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID write FTemplateTypeID;

    property ActiveWebsite: WideString read GetActiveWebsite write SetActiveWebsite;

    property ComponentController: IComponentController read GetComponentController;
    property MirrorController: IMirrorController read GetMirrorController;
    property PublishController: IPublishController read GetPublishController;

    procedure RemoveEvents;
    destructor Destroy; override;
  end;

implementation

{ TIReleaseNameChangeEventHandler }

procedure TIReleaseNameChangeEventHandler.Invoke;
begin
  if (@FHandler <> nil) then
    FHandler(AReleaseName);
end;

function MinimizeReleaseName(AReleaseName: string; AMaxLength: Integer): string;
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

function TTabSheetController.GetTabSheetIndex: Integer;
begin
  Result := PageIndex;
end;

function TTabSheetController.GetViewType;
begin
  if DataTabSheetItem.Visible then
    Exit(vtData)
  else
    Result := DesignTabSheetItem.ViewType;
end;

procedure TTabSheetController.SetViewType(AViewType: TViewType);
begin
  DataTabSheetItem.Visible := (AViewType = vtData);
  DesignTabSheetItem.Visible := (AViewType = vtCode) or (AViewType = vtPreview);
  DesignTabSheetItem.ViewType := AViewType;
end;

function TTabSheetController.GetFileName;
begin
  Result := FFileName;
end;

procedure TTabSheetController.SetFileName(AFileName: WideString);
begin
  FFileName := AFileName;
end;

function TTabSheetController.GetFileType;
begin
  Result := FFileType;
end;

procedure TTabSheetController.SetFileType(AFileType: WideString);
begin
  FFileType := AFileType;
end;

function TTabSheetController.GetReleaseName: WideString;
begin
  Result := FReleaseName;
end;

procedure TTabSheetController.SetReleaseName(AReleaseName: WideString);
begin
  FReleaseName := AReleaseName;
  UpdateCaption;
  TabHint := ReleaseName;
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

function TTabSheetController.GetTemplateTypeID: TTemplateTypeID;
begin
  Result := FTemplateTypeID;
end;

function TTabSheetController.GetActiveWebsite: WideString;
begin
  Result := FDesignTabSheetItem.ActiveWebsite;
end;

procedure TTabSheetController.SetActiveWebsite(AWebsite: WideString);
begin
  FDesignTabSheetItem.ActiveWebsite := AWebsite;
end;

procedure TTabSheetController.UpdateCaption;
begin
  if not(ReleaseName = '') then
    Self.Caption := MinimizeReleaseName(ReleaseName, 30)
  else
    Self.Caption := '<ReleaseName>';
  if DataChanged then
    Self.Caption := Self.Caption + '*';
end;

procedure TTabSheetController.ReleaseNameChange(const AReleaseName: WideString);
begin
  ReleaseName := AReleaseName;
end;

procedure TTabSheetController.ControlChange(const Sender: IBasic);
begin
  DataChanged := True;
end;

procedure TTabSheetController.MirrorChange(const Sender: IInterface);
begin
  DataChanged := True;
end;

function TTabSheetController.GetComponentController;
begin
  Result := DataTabSheetItem.ComponentController;
end;

function TTabSheetController.GetMirrorController;
begin
  Result := DataTabSheetItem.MirrorController;
end;

function TTabSheetController.GetPublishController;
begin
  Result := DesignTabSheetItem.PublishController;
end;

constructor TTabSheetController.Create;
begin
  inherited Create(AOwner);

  FPageController := APageController;

  ImageIndex := Integer(ATemplateTypeID);
  TemplateTypeID := ATemplateTypeID;
end;

procedure TTabSheetController.Install;
begin
  FDataTabSheetItem := TDataTabSheetItem.Create(Self, Self);

  FDesignTabSheetItem := TDesignTabSheetItem.Create(Self, Self);

  UpdateCaption;

  AddEvents;
end;

procedure TTabSheetController.AddEvents;
begin
  FIReleaseNameChange := TIReleaseNameChangeEventHandler.Create(ReleaseNameChange);
  ComponentController.OnReleaseNameChange := FIReleaseNameChange;

  FIControlChange := TIControlEventHandler.Create(ControlChange);
  ComponentController.OnControlChange.Add(FIControlChange);

  FIMirrorChange := TINotifyEventHandler.Create(MirrorChange);
  MirrorController.OnChange.Add(FIMirrorChange);

  FIUpdateCMSListEvent := TIUpdateCMSListEventHandler.Create(FDesignTabSheetItem.UpdateCMSList);
  PublishController.OnUpdateCMSList.Add(FIUpdateCMSListEvent);

  FIUpdateCMSWebsiteListEvent := TIUpdateCMSWebsiteListEventHandler.Create(FDesignTabSheetItem.UpdateCMSWebsiteList);
  PublishController.OnUpdateCMSWebsiteList.Add(FIUpdateCMSWebsiteListEvent);
end;

procedure TTabSheetController.Save(AFileName: WideString; AFileType: WideString);
begin
  with SettingsManager.Settings.Plugins do
    TApiPlugin.SaveFile(FindPlugInCollectionItemFromCollection(AFileType, FileFormats), AFileName, GetTemplatesTypeFolder + TemplateFileName + '.xml', Self);
  FileName := AFileName;
  FileType := AFileType;
  DataChanged := False;
end;

procedure TTabSheetController.ResetDataChanged(AFileName, AFileType: WideString);
begin
  FileName := AFileName;
  FileType := AFileType;
  DataChanged := False;
end;

procedure TTabSheetController.RemoveEvents;
begin
  /// removing tabs with active crawler is forbidden, nevertheless additionally blacklist entry
  PageController.CrawlerManager.RemoveCrawlerJob(ComponentController);

  ComponentController.OnReleaseNameChange := nil;

  PublishController.OnUpdateCMSWebsiteList.Remove(FIUpdateCMSWebsiteListEvent);
  PublishController.OnUpdateCMSList.Remove(FIUpdateCMSListEvent);

  MirrorController.OnChange.Remove(FIMirrorChange);
  ComponentController.OnControlChange.Remove(FIControlChange);
end;

destructor TTabSheetController.Destroy;
begin
  /// doing this not in a separate function causes the interfaces MirrorController
  /// and ComponentController not to destroy immediately
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
