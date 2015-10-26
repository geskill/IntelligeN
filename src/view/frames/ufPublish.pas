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

function TfPublish.GetSelectedCMSCollectionItem: TCMSWebsitesCollectionItem;
var
  CMSType, CMSPluginName: string;
begin
  with FtvDataController do
  begin
    CMSType := GetMasterDataController.Values[GetMasterDataController.FocusedRecordIndex, tvSectionsColumn.index];
    CMSPluginName := Values[FocusedRecordIndex, tvValuesColumn2.index];
  end;

  with SettingsManager.Settings.Plugins do
    result := TCMSCollectionItem(FindPlugInCollectionItemFromCollection(CMSType, CMS)).FindCMSWebsite(CMSPluginName);
end;

procedure TfPublish.UpdateCMSList(const Sender: IPublishController);
var
  I: Integer;
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
          for I := 0 to Sender.Count - 1 do
            Values[I, tvSectionsColumn.Index] := Sender.CMS[I].Name;
        end
        else
          RecordCount := 0;
      end;

    finally
      EndUpdate;
    end;
  end;

  if Assigned(Sender) then
  begin
    for I := 0 to Sender.Count - 1 do
      UpdateCMSWebsiteList(Sender.CMS[I], I);

    tvSections.ViewData.Expand(True);
  end;
end;

procedure TfPublish.UpdateCMSWebsiteList(const Sender: ICMSContainer; CMSIndex: Integer);
var
  I: Integer;
  CustomDataController: TcxCustomDataController;
begin
  with tvSections.DataController do
    CustomDataController := GetDetailDataController(CMSIndex, Self.Values.Index);

  with CustomDataController do
  begin
    BeginUpdate;
    try

      RecordCount := Sender.Count;
      for I := 0 to Sender.Count - 1 do
      begin
        Values[I, tvValuesColumn1.Index] := Sender.Website[I].Active;
        Values[I, tvValuesColumn2.Index] := Sender.Website[I].Name;
      end;

    finally
      EndUpdate;
    end;
  end;
end;

procedure TfPublish.UpdateCMSWebsite(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool);
var
  CustomDataController: TcxCustomDataController;
begin
  with tvSections.DataController do
    CustomDataController := GetDetailDataController(CMSIndex, Self.Values.Index);

  with CustomDataController do
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
  CMSType: string;
  CMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  with FtvDataController do
    CMSType := GetMasterDataController.Values[GetMasterDataController.FocusedRecordIndex, tvSectionsColumn.index];

  CMSWebsitesCollectionItem := GetSelectedCMSCollectionItem;

  case AAction of
    0: // Visit
      begin
        ShellExecute(Handle, 'open', PChar(CMSWebsitesCollectionItem.Website), nil, nil, SW_SHOW);
      end;
    1: // Preview
      begin
        with Main.fMain do
        begin
          ActiveViewType := vtPreview;
          ActiveTabSheetController.ActiveWebsite := CMSWebsitesCollectionItem.Name;
        end;
      end;
    2: // Publish
      begin
        with Main.fMain do
          PublishManager.AddPublishJob(ActiveTabSheetController.PublishController.CMS[CMSType].Website[FtvDataController.FocusedRecordIndex].GeneratePublishJob);
      end;
    3: // Settings
      begin
        with Settings do
        begin
          cxPCMain.ActivePage := cxTSPlugins;
          cxPCPlugins.ActivePage := cxTSCMS;
          with CMSPluginsCheckListBox do
          begin
            with InnerCheckListBox do
              ItemIndex := Items.IndexOf(CMSType);
            OnClick(nil);
          end;
          Show;
          cxGCMSTableView1.DataController.FocusedRecordIndex := CMSWebsitesCollectionItem.Index;
        end;
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

  inherited Destroy;
end;

end.
