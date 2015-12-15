unit ufPublish;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  // Dev Express
  cxCustomData, cxGraphics, cxFilter, cxData, cxButtons, cxDataStorage, cxEdit, cxControls, cxGridCustomView, cxGridCustomTableView, cxGridCardView, cxClasses,
  cxGridLevel, cxGrid, cxGridTableView, cxCheckBox, cxLabel, cxButtonEdit, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxContainer, cxSplitter,
  cxTextEdit, cxMemo, cxRichEdit, ExtCtrls, cxMaskEdit, cxDropDownEdit, dxBar, cxNavigator,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent,
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiMultiCastEvent, uApiPlugins, uApiSettings,
  // Plugin system
  uPlugInEvent;

type
  TfPublish = class(TFrame)
    cxGrid: TcxGrid;
    Sections: TcxGridLevel;
    Values: TcxGridLevel;
    tvSections: TcxGridTableView;
    tvSectionsColumn: TcxGridColumn;
    tvValues: TcxGridTableView;
    tvValuesColumn1: TcxGridColumn;
    tvValuesColumn2: TcxGridColumn;
    procedure tvValuesCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure tvValuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tvValuesColumn2PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FtvDataController: TcxCustomDataController;

    FIUpdateCMSListEvent: IUpdateCMSListEventHandler;
    FIUpdateCMSWebsiteListEvent: IUpdateCMSWebsiteListEventHandler;
    FIUpdateCMSWebsiteEvent: IUpdateCMSWebsiteEventHandler;

    function GetSelectedCMSCollectionItem: TCMSWebsitesCollectionItem;

    procedure UpdateCMSList(const Sender: IPublishController);
    procedure UpdateCMSWebsiteList(const Sender: ICMSContainer; CMSIndex: Integer);
    procedure UpdateCMSWebsite(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ExecuteActivePublishItem(AAction: Byte);

    function GetUpdateCMSListEvent: IUpdateCMSListEventHandler;
    function GetUpdateCMSWebsiteListEvent: IUpdateCMSWebsiteListEventHandler;
    function GetUpdateCMSWebsiteEvent: IUpdateCMSWebsiteEventHandler;

    destructor Destroy; override;
  end;

implementation

uses
  uMain, uSettings;

{$R *.dfm}

procedure TfPublish.tvValuesCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
var
  LColumn: TcxGridColumn;
  LCMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  LColumn := ACellViewInfo.Item as TcxGridColumn;

  if (LColumn.Index = tvValuesColumn2.Index) and (Main.fMain.ActiveViewType in [vtCode, vtPreview]) then
  begin
    LCMSWebsitesCollectionItem := GetSelectedCMSCollectionItem;
    Main.fMain.ActiveTabSheetController.ActiveWebsite := LCMSWebsitesCollectionItem.Website;
  end;
end;

procedure TfPublish.tvValuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Site: TcxGridSite;
  HitTest: TcxCustomGridHitTest;
  Item: TcxCustomGridTableItem;
  // Rec: TcxCustomGridRecord;
begin
  Site := Sender as TcxGridSite;
  HitTest := Site.GridView.ViewInfo.GetHitTest(X, Y);

  if HitTest is TcxGridRecordCellHitTest then
  begin
    Item := TcxGridRecordCellHitTest(HitTest).Item;
    // Rec := TcxGridRecordCellHitTest(HitTest).GridRecord;

    FtvDataController := Item.GridView.DataController;
  end;
end;

procedure TfPublish.tvValuesColumn2PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  APoint: TPoint;
begin
  with TcxButtonEdit(Sender) do
  begin
    APoint.X := Left + Width - 17;
    APoint.Y := Top + Height;
    APoint := Parent.ClientToScreen(APoint);

    Main.dxBpmPublishDropDownClick.Popup(APoint.X, APoint.Y);
  end;
end;

function TfPublish.GetSelectedCMSCollectionItem: TCMSWebsitesCollectionItem;
var
  LCMSType, LCMSPluginName: string;
begin
  with FtvDataController do
  begin
    LCMSType := GetMasterDataController.Values[GetMasterDataController.FocusedRecordIndex, tvSectionsColumn.Index];
    LCMSPluginName := Values[FocusedRecordIndex, tvValuesColumn2.index];
  end;

  with SettingsManager.Settings.Plugins do
    result := TCMSCollectionItem(FindPlugInCollectionItemFromCollection(LCMSType, CMS)).FindCMSWebsite(LCMSPluginName);
end;

procedure TfPublish.UpdateCMSList(const Sender: IPublishController);
var
  LSenderIndex: Integer;
begin
  with tvSections do
  begin
    BeginUpdate;
    try

      with DataController do
      begin
        if Assigned(Sender) then
        begin
          RecordCount := Sender.Count;
          for LSenderIndex := 0 to Sender.Count - 1 do
            Values[LSenderIndex, tvSectionsColumn.Index] := Sender.CMS[LSenderIndex].Name;
        end
        else
        begin
          RecordCount := 0;
        end;
      end;

    finally
      EndUpdate;
    end;
  end;

  if Assigned(Sender) then
  begin
    with tvSections do
    begin
      BeginUpdate;
      try
        for LSenderIndex := 0 to Sender.Count - 1 do
          UpdateCMSWebsiteList(Sender.CMS[LSenderIndex], LSenderIndex);

        ViewData.Expand(True);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TfPublish.UpdateCMSWebsiteList(const Sender: ICMSContainer; CMSIndex: Integer);
var
  LSenderIndex: Integer;
  LCustomDataController: TcxCustomDataController;
begin
  with tvSections.DataController do
    LCustomDataController := GetDetailDataController(CMSIndex, Self.Values.Index);

  with LCustomDataController do
  begin
    BeginUpdate;
    try

      RecordCount := Sender.Count;
      for LSenderIndex := 0 to Sender.Count - 1 do
      begin
        Values[LSenderIndex, tvValuesColumn1.Index] := Sender.Website[LSenderIndex].Active;
        Values[LSenderIndex, tvValuesColumn2.Index] := Sender.Website[LSenderIndex].Name;
      end;

    finally
      EndUpdate;
    end;
  end;
end;

procedure TfPublish.UpdateCMSWebsite(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool);
var
  LCustomDataController: TcxCustomDataController;
begin
  with tvSections.DataController do
    LCustomDataController := GetDetailDataController(CMSIndex, Self.Values.Index);

  with LCustomDataController do
  begin
    BeginUpdate;
    try
      Values[WebsiteIndex, tvValuesColumn1.Index] := NewStatus;
    finally
      EndUpdate;
    end;
  end;
end;

constructor TfPublish.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIUpdateCMSListEvent := TIUpdateCMSListEventHandler.Create(UpdateCMSList);

  FIUpdateCMSWebsiteListEvent := TIUpdateCMSWebsiteListEventHandler.Create(UpdateCMSWebsiteList);

  FIUpdateCMSWebsiteEvent := TIUpdateCMSWebsiteEventHandler.Create(UpdateCMSWebsite);
end;

procedure TfPublish.ExecuteActivePublishItem(AAction: Byte);
var
  LCMSType: string;
  LCMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  with FtvDataController do
    LCMSType := GetMasterDataController.Values[GetMasterDataController.FocusedRecordIndex, tvSectionsColumn.Index];

  LCMSWebsitesCollectionItem := GetSelectedCMSCollectionItem;

  case AAction of
    0: // Preview
      begin
        with Main.fMain do
        begin
          ActiveViewType := vtPreview;
          ActiveTabSheetController.ActiveWebsite := LCMSWebsitesCollectionItem.Name;
        end;
      end;
    1: // Publish
      begin
        with Main.fMain do
          PublishManager.AddPublishJob(ActiveTabSheetController.PublishController.CMS[LCMSType].Website[FtvDataController.FocusedRecordIndex].GeneratePublishJob);
      end;
    2: // Settings
      begin
        with Settings do
        begin
          cxPCMain.ActivePage := cxTSPlugins;
          cxPCPlugins.ActivePage := cxTSCMS;
          with CMSPluginsCheckListBox do
          begin
            with InnerCheckListBox do
              ItemIndex := Items.IndexOf(LCMSType);
            OnClick(nil);
          end;
          Show;
          cxGCMSTableView1.DataController.FocusedRecordIndex := LCMSWebsitesCollectionItem.Index;
        end;
      end;
    3: // Visit
      begin
        ShellExecute(Handle, 'open', PChar(LCMSWebsitesCollectionItem.Website), nil, nil, SW_SHOW);
      end;
  end;
end;

function TfPublish.GetUpdateCMSListEvent: IUpdateCMSListEventHandler;
begin
  result := FIUpdateCMSListEvent;
end;

function TfPublish.GetUpdateCMSWebsiteListEvent: IUpdateCMSWebsiteListEventHandler;
begin
  result := FIUpdateCMSWebsiteListEvent;
end;

function TfPublish.GetUpdateCMSWebsiteEvent: IUpdateCMSWebsiteEventHandler;
begin
  result := FIUpdateCMSWebsiteEvent;
end;

destructor TfPublish.Destroy;
begin
  FIUpdateCMSWebsiteEvent := nil;
  FIUpdateCMSWebsiteListEvent := nil;
  FIUpdateCMSListEvent := nil;

  FtvDataController := nil;

  inherited Destroy;
end;

end.
