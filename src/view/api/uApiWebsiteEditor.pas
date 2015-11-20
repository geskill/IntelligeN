unit uApiWebsiteEditor;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, StdCtrls, Dialogs, Forms, ExtCtrls, ComCtrls, Variants, Generics.Collections, XMLDoc, XMLIntf,
  ActiveX,
  // DevExpress
  dxBar, cxPC, cxLabel, cxTreeView, cxListView, cxButtons, cxTextEdit, cxCheckBox, cxDropDownEdit, cxGrid, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxDataStorage, cxFilter, cxBlobEdit, cxButtonEdit, cxListBox, cxEdit, cxCheckComboBox,
  // Dev Express Mod
  uMycxCheckComboBox,
  // HTTPManager implementation
  uHTTPIndyHelper,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiFile,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInCMSSettingsHelper,
  // Utils,
  uStringUtils;

type
  TTextA = record
    ID, StringTemplateTypeID: string;
    constructor Create(AValue: string);
    function Value: string;
  end;

  TTextB = record
    ID, StringComponentID, StringComponentValue: string;
    constructor Create(AValue: string);
    function Value: string;
  end;

  TIDInfos = array of TIDInfo;

  TSettingsEdit = record
    FBasis: TPanel;
    FTitle: TcxLabel;
    FTextEdit: TcxTextEdit;
    FName: string;
    FTop: Boolean;
  end;

  TSettingsCheckbox = record
    FCheckBox: TcxCheckBox;
    FName: string;
    FTop: Boolean;
  end;

  TBasisWebsiteEditor = class;

  TAcceptEvent = procedure(TextValue: string) of object;

  TIDEditPanel = class(TPanel)
  protected
    FcxGID: TcxGrid;
    FcxGIDLevel: TcxGridLevel;
    FcxGIDTableView: TcxGridTableView;
    FcxGIDTableViewColumn1: TcxGridColumn;
    FcxGIDTableViewColumn2: TcxGridColumn;

    FIDInfos: TIDInfos;

    FcxLID: TcxLabel;
    FcxPEID: TcxPopupEdit;
    FcxBCancel, FcxBAccept: TcxButton;

    FOnClose: TNotifyEvent;
    FOnAccept: TAcceptEvent;

    procedure FcxGIDDblClick(Sender: TObject);
    procedure FcxGIDFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure FcxGIDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FcxPEIDInitPopup(Sender: TObject);
    procedure FcxBCancelClick(Sender: TObject);
    procedure FcxBAcceptClick(Sender: TObject); virtual;
  public
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnAccept: TAcceptEvent read FOnAccept write FOnAccept;

    constructor Create(AOwner: TComponent); override;
    property IDInfos: TIDInfos read FIDInfos write FIDInfos;
    procedure RefreshIDInfos;
    destructor Destroy; override;
  end;

  TIDEditAPanel = class(TIDEditPanel)
  protected
    FcxLType: TcxLabel;
    FcxCOBType: TcxComboBox;

    procedure FcxBAcceptClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TIDEditBPanel = class(TIDEditPanel)
  protected
    FAppController: IAppController;
    FStringTypeID: string;

    FcxLControlName, FcxLControlValue: TcxLabel;
    FcxCOBControlName, FcxCOBControlValue: TcxComboBox;

    procedure FcxBAcceptClick(Sender: TObject); override;
    procedure FcxCOBControlValueInitPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AAppController: IAppController; AStringTemplateTypeID: string); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TIDPanel = class;

  TIDGrabberThread = class(TThread)
  private
    FCMSPlugIn: ICMSPlugIn;
    FIDPanel: TIDPanel;
  protected
    procedure Execute; override;
  public
    constructor Create(ACMSPlugIn: ICMSPlugIn; AIDPanel: TIDPanel); virtual;
  end;

  TIDPanel = class(TPanel)
  protected
    FBasisWebsiteEditor: TBasisWebsiteEditor;
    FdxBPM: TdxBarPopupMenu;
    FdxBBUp, FdxBBDown, FdxBBAdd, FdxBBEdit, FdxBBDelete: TdxBarButton;
    FdxBS1: TdxBarSeparator;
    FcxTreeView: TcxTreeView;
    FcxBAdd, FcxBEdit, FcxBDelete: TcxButton;
    FIDInfos: TIDInfos;
    FEditID: TIDEditPanel;
    procedure FdxBPMPopup(Sender: TObject);
    procedure FcxTreeViewClick(Sender: TObject);
    procedure FcxTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FcxTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure AddID(TextValue: string);
    procedure EditID(TextValue: string);
    procedure EditIDClose(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure SetIDInfos(AIDInfos: TIDInfos);
    procedure TabsEnabled(AEnabled: Boolean);
    procedure CanEditandDelete(ACanEditandDelete: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property IDInfos: TIDInfos read FIDInfos write SetIDInfos;
    procedure ShowEditPanel(AEdit: Boolean = False);
    procedure HideEditPanel;
    destructor Destroy; override;
  end;

  THosterPanel = class(TPanel)
  private
    FDragDropIndex: Integer;
    FHosterType: THosterType;
  protected
    FBasisWebsiteEditor: TBasisWebsiteEditor;

    FcxLRankedWhiteList, FcxLBlackList: TcxLabel;
    FcxLBRankedWhiteList, FcxLBBlackList: TcxListBox;
    FcxCBRanking: TcxCheckBox;
    FcxCOBAddToBlackList: TcxComboBox;
    FcxBRemoveFromBlackList: TcxButton;
    procedure FcxLBRankedWhiteListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FcxLBRankedWhiteListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FcxCBRankingChange(Sender: TObject);
    procedure FcxCOBAddToBlackListPropertiesChange(Sender: TObject);
    procedure FcxCOBAddToBlackListPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure FcxLBBlackListClick(Sender: TObject);
    procedure FcxBRemoveFromBlackListClick(Sender: TObject);
    procedure SetHosterType(AHosterType: THosterType);
    function GetHosterName: string;
    procedure UpdateRankedWhiteList;
  public
    constructor Create(AOwner: TComponent); override;
    property HosterType: THosterType read FHosterType write SetHosterType;
    destructor Destroy; override;
  end;

  TBasisWebsiteEditor = class(TForm, IWebsiteEditor)
  private
    FIDGrabberThread: TIDGrabberThread;
  protected
    FCMSPlugIn: ICMSPlugIn;
    FAppController: IAppController;
    FWebsiteSettingsFileName: TFileName;
    FcxPCWebsiteSettings: TcxPageControl;
    FcxLCharset: TcxLabel;
    FcxCOBCharset: TcxComboBox;
    FcxBCancel, FcxBAccept: TcxButton;
    FcxTSGeneral: TcxTabSheet;
    FEditArray: array of TSettingsEdit;
    FCheckboxArray: array of TSettingsCheckbox;
    FFlowPanel: TFlowPanel;
    FcxTSIDs: TcxTabSheet;
    FIDPanelList: TList<TIDPanel>;
    FcxTCIDs: TcxTabControl;
    FcxTSFilter: TcxTabSheet;
    FFilterPanel: TPanel;
    FcxCBFilterEnabled: TcxCheckBox;
    FcxLFilterCategories, FcxLFilterControls: TcxLabel;
    FcxCCBFilterCategories: TMycxCheckComboBox;
    FcxGridFilterControls: TcxGrid;
    FcxGridFilterControlsLevel: TcxGridLevel;
    FcxGridFilterControlsTableView: TcxGridTableView;
    FcxGridFilterControlsTableViewColumn1: TcxGridColumn;
    FcxGridFilterControlsTableViewColumn2: TcxGridColumn;
    FcxGridFilterControlsTableViewColumn3: TcxGridColumn;
    FcxGridFilterControlsTableViewColumn4: TcxGridColumn;
    FcxTSHoster: TcxTabSheet;
    FHosterPanelList: TList<THosterPanel>;
    FcxTCHoster: TcxTabControl;
    FcxTSCustomFields: TcxTabSheet;
    FcxGridCustomFields: TcxGrid;
    FcxGridCustomFieldsLevel: TcxGridLevel;
    FcxGridCustomFieldsTableView: TcxGridTableView;
    FcxGridCustomFieldsTableViewColumn1: TcxGridColumn;
    FcxGridCustomFieldsTableViewColumn2: TcxGridColumn;
    FcxGridCustomFieldsTableViewColumn3: TcxGridColumn;
    procedure Show(Sender: TObject);
    procedure FcxBAcceptClick(Sender: TObject);
    procedure FcxTCIDsChange(Sender: TObject);
    procedure FcxCBFilterEnabledChange(Sender: TObject);
    procedure FcxGridFilterControlsTableViewColumn3PropertiesInitPopup(Sender: TObject);
    procedure FcxGridFilterControlsTableViewColumn4GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption;
      var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
    procedure FcxGridFilterControlsTableViewColumn4PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure FcxTCHosterChange(Sender: TObject);
    procedure FcxGridCustomFieldsTableViewColumn3GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean;
      var AHintTextRect: TRect);
    procedure FcxGridCustomFieldsTableViewColumn3PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    function GetCustomFields: WordBool; safecall;
    procedure SetCustomFields(ACustomFields: WordBool); safecall;
  public
    constructor Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName); reintroduce; virtual;
    procedure AddEdit(AName: WideString; ADefaultValue: WideString = ''; ATopValue: WordBool = False); safecall;
    procedure AddCheckbox(AName: WideString; ADefaultValue: WordBool = False; ATopValue: WordBool = False); safecall;
    procedure AddCategoryTab(AName: WideString); safecall;
    procedure AddHosterTab(AHosterType: THosterType);
    property CustomFields: WordBool read GetCustomFields write SetCustomFields;

    function ShowModal: Integer; reintroduce; safecall;
    property CMSPlugIn: ICMSPlugIn read FCMSPlugIn write FCMSPlugIn;
    property WebsiteSettingsFileName: TFileName read FWebsiteSettingsFileName write FWebsiteSettingsFileName;
    destructor Destroy; override;
  end;

  TBoardWebsiteEditor = class(TBasisWebsiteEditor)
  public
    constructor Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName); override;
    destructor Destroy; override;
  end;

  TBlogWebsiteEditor = class(TBasisWebsiteEditor)
  public
    constructor Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName); override;
    destructor Destroy; override;
  end;

  TFormbasedWebsiteEditor = class(TBlogWebsiteEditor)
  protected
    FcxTSHosterBlacklist: TcxTabSheet;
    FcxLVHosterBlacklist: TcxListView;
  public
    constructor Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName); override;
    destructor Destroy; override;
  end;

  TWebsiteEditorFactory = class
  type
    TWebsiteEditorMeta = class of TBasisWebsiteEditor;
  public
    class function GetClassType(ACMSType: TCMSType): TWebsiteEditorMeta;
  end;

implementation

{ TTextA }

constructor TTextA.Create(AValue: string);
begin
  StringTemplateTypeID := copy(AValue, 1, Pos(' ', AValue) - 1);
  ID := copy(AValue, Pos('"', AValue) + 1);
  ID := copy(ID, 1, length(ID) - 1);
end;

function TTextA.Value;
begin
  result := StringTemplateTypeID + ' ="' + ID + '"';
end;

{ TTextB }

constructor TTextB.Create(AValue: string);
begin
  ID := copy(AValue, Pos('"', AValue) + 1);
  ID := copy(ID, 1, Pos('"', ID) - 1);

  StringComponentID := copy(AValue, Pos('" ', AValue) + 2);
  StringComponentID := copy(StringComponentID, 1, Pos('="', StringComponentID) - 1);

  StringComponentValue := copy(AValue, 1, length(AValue) - 1);
  StringComponentValue := copy(StringComponentValue, LastDelimiter('"', StringComponentValue) + 1);
end;

function TTextB.Value;
begin
  result := '="' + ID + '" ' + StringComponentID + '="' + StringComponentValue + '"';
end;

{ TIDEditPanel }

procedure TIDEditPanel.FcxGIDDblClick(Sender: TObject);
begin
  FcxPEID.DroppedDown := False;
end;

procedure TIDEditPanel.FcxGIDFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  if AFocusedRecord <> nil then
    FcxPEID.Text := AFocusedRecord.Values[FcxGIDTableViewColumn1.Index];
end;

procedure TIDEditPanel.FcxGIDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FcxPEID.DroppedDown := False;
end;

procedure TIDEditPanel.FcxPEIDInitPopup(Sender: TObject);

  function FindRecordIndexByText(const AText: string): Integer;
  var
    LRecordIndex: Integer;
  begin
    result := -1;

    with FcxGIDTableView.DataController do
    begin
      for LRecordIndex := 0 to RecordCount - 1 do
        if SameText(Values[LRecordIndex, 0], AText) then
        begin
          result := LRecordIndex;
          Break;
        end;
    end;
  end;

var
  LRecordIndex: Integer;
begin
  RefreshIDInfos;

  LRecordIndex := FindRecordIndexByText(FcxPEID.Text);

  with FcxGIDTableView.DataController do
    FocusedRecordIndex := LRecordIndex;
end;

procedure TIDEditPanel.FcxBCancelClick(Sender: TObject);
begin
  FcxBCancel.Cancel := False;
  FcxBAccept.Default := False;

  if Assigned(FOnClose) then
    FOnClose(Sender);
end;

procedure TIDEditPanel.FcxBAcceptClick(Sender: TObject);
begin
  FcxBCancel.Cancel := False;
  FcxBAccept.Default := False;

  if Assigned(FOnClose) then
    FOnClose(Sender);
end;

constructor TIDEditPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Parent := TWinControl(AOwner);

  BevelOuter := bvNone;
  Caption := '';
  ShowCaption := False;

  FcxGID := TcxGrid.Create(nil);
  with FcxGID do
  begin
    Height := 200;
    Width := 300;

    FcxGIDLevel := Levels.Add;
    FcxGIDTableView := CreateView(TcxGridTableView) as TcxGridTableView;
    FcxGIDLevel.GridView := FcxGIDTableView;

    with FcxGIDTableView.DataController do
    begin
      Filter.Options := Filter.Options + [fcoCaseInsensitive];
    end;

    with FcxGIDTableView.FilterRow do
    begin
      ApplyChanges := fracImmediately;
      Visible := True;
    end;

    with FcxGIDTableView.OptionsBehavior do
    begin
      CellHints := False;
    end;

    with FcxGIDTableView.OptionsSelection do
    begin
      CellSelect := False;
    end;

    with FcxGIDTableView.OptionsView do
    begin
      ColumnAutoWidth := True;

      GroupByBox := False;
      ScrollBars := ssVertical;
    end;

    with FcxGIDTableView do
    begin
      OnDblClick := FcxGIDDblClick;
      OnFocusedRecordChanged := FcxGIDFocusedRecordChanged;
      OnKeyDown := FcxGIDKeyDown;
    end;

    FcxGIDTableViewColumn1 := FcxGIDTableView.CreateColumn;
    with FcxGIDTableViewColumn1 do
    begin
      Caption := 'ID';
      Width := 20;
      // PropertiesClass := TcxLabelProperties;
    end;

    FcxGIDTableViewColumn2 := FcxGIDTableView.CreateColumn;
    with FcxGIDTableViewColumn2 do
    begin
      Caption := 'Path';
      // PropertiesClass := TcxLabelProperties;
    end;
  end;

  FcxLID := TcxLabel.Create(Self);
  with FcxLID do
  begin
    Parent := Self;

    Left := 0;
    Top := 0;

    // Height: 17

    Caption := 'ID:';
    Transparent := True;
  end;

  FcxPEID := TcxPopupEdit.Create(Self);
  with FcxPEID do
  begin
    Parent := Self;

    Left := 0;
    Top := 20;

    with Properties do
    begin
      PopupAutoSize := False;
      ImmediateDropDownWhenKeyPressed := False;
      PopupControl := FcxGID;

      PopupSysPanelStyle := True;

      OnInitPopup := FcxPEIDInitPopup;
    end;
  end;

  FcxBCancel := TcxButton.Create(Self);
  with FcxBCancel do
  begin
    Parent := Self;

    Left := Self.Width - Width - 75 - 6;
    Top := 16;

    Cancel := True;
    Caption := 'Cancel';

    Anchors := [akRight, akBottom];

    OnClick := FcxBCancelClick;
  end;

  FcxBAccept := TcxButton.Create(Self);
  with FcxBAccept do
  begin
    Parent := Self;

    Left := Self.Width - Width;
    Top := 16;

    Default := True;
    Caption := 'Accept';

    Anchors := [akRight, akBottom];

    OnClick := FcxBAcceptClick;
  end;
end;

procedure TIDEditPanel.RefreshIDInfos;
var
  I: Integer;
begin
  with FcxGIDTableView do
  begin
    with DataController do
      try
        BeginUpdate;
        RecordCount := length(FIDInfos);

        if (length(FIDInfos) > 0) then
          if IsNumber(FIDInfos[0].ID) then
            FcxGIDTableViewColumn1.DataBinding.ValueTypeClass := TcxIntegerValueType
          else
            FcxGIDTableViewColumn1.DataBinding.ValueTypeClass := TcxStringValueType;

        for I := 0 to length(FIDInfos) - 1 do
        begin
          Values[I, FcxGIDTableViewColumn1.Index] := FIDInfos[I].ID;
          Values[I, FcxGIDTableViewColumn2.Index] := FIDInfos[I].Path;
        end;
      finally
        EndUpdate;
      end;
  end;
end;

destructor TIDEditPanel.Destroy;
begin
  FcxBAccept.Free;
  FcxBCancel.Free;
  FcxLID.Free;
  FcxPEID.Free;
  // FcxGIDTableViewColumn2.Free;
  // FcxGIDTableViewColumn1.Free; wird automatisch mit freigegeben
  // ansonsten exception wenn man dieses sortiert und danach einen
  // anderen öffnet
  FcxGIDTableView.Free;
  FcxGIDLevel.Free;
  FcxGID.Free;
  inherited Destroy;
end;

{ TIDEditAPanel }

procedure TIDEditAPanel.FcxBAcceptClick(Sender: TObject);
var
  TextA: TTextA;
begin
  TextA.ID := FcxPEID.Text;
  TextA.StringTemplateTypeID := FcxCOBType.Text;

  if Assigned(FOnAccept) then
    FOnAccept(TextA.Value);

  inherited;
end;

constructor TIDEditAPanel.Create(AOwner: TComponent);
var
  LTypeID: TTypeID;
begin
  inherited Create(AOwner);

  FcxLType := TcxLabel.Create(Self);
  with FcxLType do
  begin
    Parent := Self;

    Left := FcxPEID.Left + FcxPEID.Width + 6;
    Top := 0;

    // Height: 17

    Caption := 'Type:';
    Transparent := True;
  end;

  FcxCOBType := TcxComboBox.Create(Self);
  with FcxCOBType do
  begin
    Parent := Self;

    Left := FcxPEID.Left + FcxPEID.Width + 6;
    Top := 20;

    with Properties do
    begin
      // DropDownListStyle

      for LTypeID := Low(TTypeID) to High(TTypeID) do
        Items.Add(TypeIDToString(LTypeID));
    end;
  end;
end;

destructor TIDEditAPanel.Destroy;
begin
  FcxCOBType.Free;
  FcxLType.Free;
  inherited Destroy;
end;

{ TIDEditBPanel }

procedure TIDEditBPanel.FcxBAcceptClick(Sender: TObject);
var
  TextB: TTextB;
begin
  TextB.ID := FcxPEID.Text;
  TextB.StringComponentID := FcxCOBControlName.Text;
  TextB.StringComponentValue := FcxCOBControlValue.Text;

  if Assigned(FOnAccept) then
    FOnAccept(TextB.Value);

  inherited;
end;

procedure TIDEditBPanel.FcxCOBControlValueInitPopup(Sender: TObject);
begin
  if StringInTypeID(FStringTypeID) and StringInControlID(FcxCOBControlName.Text) then
    FcxCOBControlValue.Properties.Items.Text := FAppController.GetControlValues(StringToTypeID(FStringTypeID), StringToControlID(FcxCOBControlName.Text));
end;

constructor TIDEditBPanel.Create(AOwner: TComponent; AAppController: IAppController; AStringTemplateTypeID: string);
var
  LControlID: TControlID;
begin
  inherited Create(AOwner);

  FAppController := AAppController;
  FStringTypeID := AStringTemplateTypeID;

  FcxLControlName := TcxLabel.Create(Self);
  with FcxLControlName do
  begin
    Parent := Self;

    Left := FcxPEID.Left + FcxPEID.Width + 6;
    Top := 0;

    // Height: 17

    Caption := 'Control name:';
    Transparent := True;
  end;

  FcxCOBControlName := TcxComboBox.Create(Self);
  with FcxCOBControlName do
  begin
    Parent := Self;

    Left := FcxPEID.Left + FcxPEID.Width + 6;
    Top := 20;

    with Properties do
    begin
      // DropDownListStyle

      for LControlID := Low(TControlID) to High(TControlID) do
        Items.Add(ControlIDToString(LControlID));
    end;
  end;

  FcxLControlValue := TcxLabel.Create(Self);
  with FcxLControlValue do
  begin
    Parent := Self;

    Left := FcxCOBControlName.Left + FcxCOBControlName.Width + 6;
    Top := 0;

    // Height: 17

    Caption := 'Control value:';
    Transparent := True;
  end;

  FcxCOBControlValue := TcxComboBox.Create(Self);
  with FcxCOBControlValue do
  begin
    Parent := Self;

    Left := FcxCOBControlName.Left + FcxCOBControlName.Width + 6;
    Top := 20;

    with Properties do
    begin
      OnInitPopup := FcxCOBControlValueInitPopup;
    end;
  end;

end;

destructor TIDEditBPanel.Destroy;
begin
  FcxCOBControlValue.Free;
  FcxLControlValue.Free;
  FcxCOBControlName.Free;
  FcxLControlName.Free;

  FAppController := nil;

  inherited Destroy;
end;

{ TIDGrabberThread }

procedure TIDGrabberThread.Execute;
var
  IDIndex, IDCount: Integer;
  IDInfos: TIDInfos;
begin
  IDCount := FCMSPlugIn.GetIDs;

  for IDIndex := 0 to IDCount - 1 do
  begin
    SetLength(IDInfos, IDIndex + 1);
    IDInfos[IDIndex] := FCMSPlugIn.ReadID(IDIndex);
  end;

  if not Terminated then
    Synchronize(
      { } procedure
      { } begin
      { . } try
      { ... } FIDPanel.IDInfos := IDInfos;
      { . } except
      { . } end;
      { } end);
end;

constructor TIDGrabberThread.Create(ACMSPlugIn: ICMSPlugIn; AIDPanel: TIDPanel);
begin
  inherited Create(True);

  FCMSPlugIn := ACMSPlugIn;
  FIDPanel := AIDPanel;
end;

{ TIDPanel }

procedure TIDPanel.FdxBPMPopup(Sender: TObject);
var
  p: TPoint;
  _Selected: Boolean;
begin
  p := FcxTreeView.ScreenToClient(Mouse.CursorPos);
  _Selected := (FcxTreeView.GetHitTestInfoAt(p.X, p.Y) <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon]);
  if _Selected then
    FcxTreeView.Select(FcxTreeView.GetNodeAt(p.X, p.Y), [])
  else
    FcxTreeView.Select(nil, []);

  CanEditandDelete(_Selected);

  FdxBBUp.Enabled := _Selected and (FcxTreeView.Selected.getPrevSibling <> nil);
  FdxBBDown.Enabled := _Selected and (FcxTreeView.Selected.getNextSibling <> nil);
end;

procedure TIDPanel.FcxTreeViewClick(Sender: TObject);
var
  p: TPoint;
begin
  p := FcxTreeView.ScreenToClient(Mouse.CursorPos);
  if not(FcxTreeView.GetHitTestInfoAt(p.X, p.Y) <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon]) then
  begin
    FcxTreeView.Select(nil, []);
    CanEditandDelete(False);
  end
  else
    CanEditandDelete(True);
end;

procedure TIDPanel.FcxTreeViewDragDrop(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
var
  p: TPoint;
  MouseOverTargetNode: Boolean;
  TargetNode, SourceNode, NewItem: TTreeNode;
  ChildIndex: Integer;
begin
  with FcxTreeView do
  begin
    p := ScreenToClient(Mouse.CursorPos);
    MouseOverTargetNode := (GetHitTestInfoAt(p.X, p.Y) <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon]);
    TargetNode := GetNodeAt(X, Y);
    SourceNode := Selected;

    if (TargetNode = SourceNode) or (SourceNode.Level = 0) or ((SourceNode.Level > 0) and not Assigned(TargetNode)) or ((TargetNode.Level = 0) and not MouseOverTargetNode) then
    begin
      MessageBeep(MB_ICONEXCLAMATION);
      EndDrag(False);
    end
    else
    begin
      if not MouseOverTargetNode then
      begin
        NewItem := TargetNode.getNextSibling;
        if Assigned(NewItem) then
          SourceNode.MoveTo(TargetNode, naInsert)
        else
          SourceNode.MoveTo(TargetNode, naAdd)
      end
      else
      begin
        NewItem := Items.AddChild(TargetNode, SourceNode.Text);

        for ChildIndex := 1 to SourceNode.Count do
        begin
          NewItem := Items.AddChild(NewItem, SourceNode.GetFirstChild.Text);
          Items.Delete(SourceNode.GetFirstChild);
        end;

        SourceNode.Delete;
      end;
    end;
  end;
end;

procedure TIDPanel.FcxTreeViewDragOver(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender = FcxTreeView);
end;

procedure TIDPanel.AddID(TextValue: string);
begin
  FcxTreeView.Items.AddChild(FcxTreeView.Selected, TextValue);
end;

procedure TIDPanel.EditID(TextValue: string);
begin
  FcxTreeView.Selected.Text := TextValue;
end;

procedure TIDPanel.EditIDClose(Sender: TObject);
begin
  HideEditPanel;
end;

procedure TIDPanel.AddClick(Sender: TObject);
begin
  ShowEditPanel;
end;

procedure TIDPanel.EditClick(Sender: TObject);
begin
  ShowEditPanel(True);
end;

procedure TIDPanel.DeleteClick(Sender: TObject);
begin
  if (MessageDlg('Do you want to remove the selected item?', mtConfirmation, [mbyes, mbno, mbcancel], 0) = ID_YES) then
    FcxTreeView.Selected.Free;

  CanEditandDelete(Assigned(FcxTreeView.Selected));
end;

procedure TIDPanel.MoveUpClick(Sender: TObject);
begin
  with FcxTreeView.Selected do
    MoveTo(getPrevSibling, naInsert);
end;

procedure TIDPanel.MoveDownClick(Sender: TObject);
begin
  with FcxTreeView.Selected do
    if getNextSibling.getNextSibling <> nil then
      MoveTo(getNextSibling.getNextSibling, naInsert)
    else
      MoveTo(getNextSibling, naAdd)
end;

procedure TIDPanel.SetIDInfos(AIDInfos: TIDInfos);
begin
  FIDInfos := AIDInfos;
  if Assigned(FEditID) then
  begin
    FEditID.IDInfos := FIDInfos;
    FEditID.RefreshIDInfos;
  end;
end;

procedure TIDPanel.TabsEnabled(AEnabled: Boolean);
var
  I: Integer;
begin
  with FBasisWebsiteEditor do
  begin
    with FcxPCWebsiteSettings do
      for I := 0 to PageCount - 1 do
        Pages[I].Enabled := AEnabled or (ActivePage = Pages[I]);
    with FcxTCIDs do
      for I := 0 to Tabs.Count - 1 do
        Tabs.Tabs[I].Enabled := AEnabled or (TabIndex = I);
  end;
end;

procedure TIDPanel.CanEditandDelete(ACanEditandDelete: Boolean);
begin
  FdxBBEdit.Enabled := ACanEditandDelete;
  FdxBBDelete.Enabled := ACanEditandDelete;

  FcxBEdit.Enabled := ACanEditandDelete;
  FcxBDelete.Enabled := ACanEditandDelete;
end;

constructor TIDPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBasisWebsiteEditor := TBasisWebsiteEditor(AOwner);

  BevelOuter := bvNone;
  Caption := '';
  ShowCaption := False;

  FdxBPM := TdxBarPopupMenu.Create(Self);
  with FdxBPM do
  begin
    OnPopup := FdxBPMPopup;
  end;

  FdxBBUp := TdxBarButton.Create(FdxBPM);
  with FdxBBUp do
  begin
    Caption := 'Up';
    Enabled := False;
    OnClick := MoveUpClick;
  end;
  FdxBPM.ItemLinks.Add.Item := FdxBBUp;

  FdxBBDown := TdxBarButton.Create(FdxBPM);
  with FdxBBDown do
  begin
    Caption := 'Down';
    Enabled := False;
    OnClick := MoveDownClick;
  end;
  FdxBPM.ItemLinks.Add.Item := FdxBBDown;

  FdxBS1 := TdxBarSeparator.Create(FdxBPM);
  with FdxBS1 do
    ShowCaption := False;
  FdxBPM.ItemLinks.Add.Item := FdxBS1;

  FdxBBAdd := TdxBarButton.Create(FdxBPM);
  with FdxBBAdd do
  begin
    Caption := 'Add';

    OnClick := AddClick;
  end;
  FdxBPM.ItemLinks.Add.Item := FdxBBAdd;

  FdxBBEdit := TdxBarButton.Create(FdxBPM);
  with FdxBBEdit do
  begin
    Caption := 'Edit';
    Enabled := False;
    OnClick := EditClick;
  end;
  FdxBPM.ItemLinks.Add.Item := FdxBBEdit;

  FdxBBDelete := TdxBarButton.Create(FdxBPM);
  with FdxBBDelete do
  begin
    Caption := 'Remove';
    Enabled := False;
    OnClick := DeleteClick;
  end;
  FdxBPM.ItemLinks.Add.Item := FdxBBDelete;

  FcxTreeView := TcxTreeView.Create(Self);
  with FcxTreeView do
  begin
    Parent := Self;

    Left := 6;
    Top := 6;
    Width := Self.Width - Left - 6;
    Height := Self.Height - Top - 33;

    // DragMode := dmAutomatic;
    HideSelection := False;
    HotTrack := True;
    ReadOnly := True;

    Anchors := [akLeft, akTop, akRight, akBottom];

    PopupMenu := FdxBPM;

    OnClick := FcxTreeViewClick;
    // OnDragDrop := FcxTreeViewDragDrop;
    // OnDragOver := FcxTreeViewDragOver;
  end;

  FcxBAdd := TcxButton.Create(Self);
  with FcxBAdd do
  begin
    Parent := Self;

    Caption := 'Add';

    Left := 6;
    Top := FcxTreeView.Top + FcxTreeView.Height + 6;
    Width := 75;
    Height := 25;

    Anchors := [akLeft, akBottom];

    OnClick := AddClick;
  end;

  FcxBEdit := TcxButton.Create(Self);
  with FcxBEdit do
  begin
    Parent := Self;

    Caption := 'Edit';

    Left := 6 + 75 + 6;
    Top := FcxTreeView.Top + FcxTreeView.Height + 6;
    Width := 75;
    Height := 25;

    Enabled := False;

    Anchors := [akLeft, akBottom];

    OnClick := EditClick;
  end;

  FcxBDelete := TcxButton.Create(Self);
  with FcxBDelete do
  begin
    Parent := Self;

    Caption := 'Remove';

    Left := 6 + 75 + 6 + 75 + 6;
    Top := FcxTreeView.Top + FcxTreeView.Height + 6;
    Width := 75;
    Height := 25;

    Enabled := False;

    Anchors := [akLeft, akBottom];

    OnClick := DeleteClick;
  end;

end;

procedure TIDPanel.ShowEditPanel(AEdit: Boolean = False);

  function GetRootName(ATreeNode: TTreeNode): string;
  begin
    while not(ATreeNode.Level = 0) do
      ATreeNode := ATreeNode.GetPrev;
    result := ATreeNode.Text;
  end;

begin
  with FBasisWebsiteEditor do
  begin
    TabsEnabled(False);
    FcxBCancel.Cancel := False;
    FcxBAccept.Default := False;
  end;
  FcxTreeView.Enabled := False;
  FcxBAdd.Visible := False;
  FcxBEdit.Visible := False;
  FcxBDelete.Visible := False;

  with FcxTreeView do
    Height := Height - 16;

  if Assigned(FEditID) then
    FEditID.Free;

  if not Assigned(FcxTreeView.Selected) xor (AEdit and (FcxTreeView.Selected.Level = 0)) then
    FEditID := TIDEditAPanel.Create(Self)
  else
    FEditID := TIDEditBPanel.Create(Self, FBasisWebsiteEditor.FAppController, TTextA.Create(GetRootName(FcxTreeView.Selected)).StringTemplateTypeID);
  with FEditID do
  begin
    Left := 6;
    Top := FcxBAdd.Top - 16;
    Width := FcxTreeView.Width;
    Height := 41;

    Anchors := [akLeft, akRight, akBottom];

    IDInfos := Self.IDInfos;

    OnClose := EditIDClose;
    OnAccept := AddID;

    if AEdit then
    begin
      if FcxTreeView.Selected.Level = 0 then
      begin
        with TTextA.Create(FcxTreeView.Selected.Text) do
        begin
          FcxPEID.Text := ID;

          with FEditID as TIDEditAPanel do
          begin
            FcxCOBType.Text := StringTemplateTypeID;

            OnAccept := EditID;
          end;
        end;
      end
      else
        with TTextB.Create(FcxTreeView.Selected.Text) do
        begin
          FcxPEID.Text := ID;

          with FEditID as TIDEditBPanel do
          begin
            FcxCOBControlName.Text := StringComponentID;
            FcxCOBControlValue.Text := StringComponentValue;

            OnAccept := EditID;
          end;
        end;
    end;
  end;
end;

procedure TIDPanel.HideEditPanel;
begin
  FEditID.Visible := False;
  with FcxTreeView do
    Height := Height + 16;
  with FBasisWebsiteEditor do
  begin
    FcxBCancel.Cancel := True;
    FcxBAccept.Default := True;
    TabsEnabled(True);
  end;
  FcxTreeView.Enabled := True;
  FcxBAdd.Visible := True;
  FcxBEdit.Visible := True;
  FcxBDelete.Visible := True;
end;

destructor TIDPanel.Destroy;
begin
  if Assigned(FEditID) then
    FEditID.Free;

  FcxBDelete.Free;

  FcxBEdit.Free;

  FcxBAdd.Free;

  FcxTreeView.Free;

  FdxBBDelete.Free;

  FdxBBEdit.Free;

  FdxBBAdd.Free;

  FdxBS1.Free;

  FdxBBDown.Free;

  FdxBBUp.Free;

  FdxBPM.Free;

  inherited Destroy;
end;

{ THosterPanel }

procedure THosterPanel.FcxLBRankedWhiteListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  FDragDropIndex := FcxLBRankedWhiteList.ItemAtPos(Point(X, Y), True);
  if (FDragDropIndex <> -1) and (FDragDropIndex <> FcxLBRankedWhiteList.ItemIndex) and (FcxLBRankedWhiteList.ItemIndex <> -1) then
  begin
    Accept := True;
  end
  else
    FDragDropIndex := -1;
end;

procedure THosterPanel.FcxLBRankedWhiteListEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if FDragDropIndex <> -1 then
    with FcxLBRankedWhiteList do
    begin
      Items.Move(ItemIndex, FDragDropIndex);
      Selected[FDragDropIndex] := True;
      FDragDropIndex := -1;
    end;
end;

procedure THosterPanel.FcxCOBAddToBlackListPropertiesChange(Sender: TObject);
begin
  with TcxComboBoxProperties(FcxCOBAddToBlackList.Properties) do
    Buttons[1].Enabled := (FcxCOBAddToBlackList.ItemIndex >= 0);
end;

procedure THosterPanel.FcxCBRankingChange(Sender: TObject);
begin
  FcxLBRankedWhiteList.Enabled := FcxCBRanking.Checked;
end;

procedure THosterPanel.FcxCOBAddToBlackListPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = 1 then
  begin
    if FcxLBBlackList.Items.IndexOf(FcxCOBAddToBlackList.Text) = -1 then
    begin
      FcxLBBlackList.Items.Add(FcxCOBAddToBlackList.Text);
      UpdateRankedWhiteList;
    end
    else
      MessageDlg('This hoster is already inside the blacklist.', mtError, [mbOK], 0);
  end;
end;

procedure THosterPanel.FcxLBBlackListClick(Sender: TObject);
begin
  FcxBRemoveFromBlackList.Enabled := not(FcxLBBlackList.ItemIndex = -1);
end;

procedure THosterPanel.FcxBRemoveFromBlackListClick(Sender: TObject);
begin
  with FcxLBBlackList do
    Items.Delete(ItemIndex);

  UpdateRankedWhiteList;

  FcxBRemoveFromBlackList.Enabled := not(FcxLBBlackList.ItemIndex = -1);
end;

procedure THosterPanel.SetHosterType(AHosterType: THosterType);
begin
  FHosterType := AHosterType;

  with FcxCOBAddToBlackList.Properties do
    case AHosterType of
      htFile:
        Items.Text := FBasisWebsiteEditor.FAppController.GetFileHosters;
      htImage:
        Items.Text := FBasisWebsiteEditor.FAppController.GetImageHosters;
    end;
end;

function THosterPanel.GetHosterName: string;
begin
  result := GetHosterTypeName(FHosterType);
end;

procedure THosterPanel.UpdateRankedWhiteList;

  procedure IntersectionStrings(const a, b, intersection: TStrings);
  var
    I: Integer;
  begin
    Assert(Assigned(a));
    Assert(Assigned(b));
    Assert(Assigned(intersection));

    intersection.BeginUpdate;
    try
      intersection.Clear;
      for I := 0 to a.Count - 1 do
      begin
        if b.IndexOf(a.Strings[I]) >= 0 then
          intersection.AddObject(a.Strings[I], a.Objects[I]);
      end;
    finally
      intersection.EndUpdate;
    end;
  end;

  procedure DifferenceStrings(const a, b, diff: TStrings);
  var
    I, idx: Integer;
  begin
    Assert(Assigned(a));
    Assert(Assigned(b));
    Assert(Assigned(diff));
    diff.BeginUpdate;
    try
      diff.Assign(a);
      for I := 0 to b.Count - 1 do
      begin
        idx := diff.IndexOf(b.Strings[I]);
        if idx >= 0 then
          diff.Delete(idx);
      end;
    finally
      diff.EndUpdate;
    end;
  end;

  procedure UnionStrings(const a, b, union: TStrings);
  var
    I: Integer;
  begin
    Assert(Assigned(a));
    Assert(Assigned(b));
    Assert(Assigned(union));
    union.BeginUpdate;
    try
      union.Assign(a);
      for I := 0 to b.Count - 1 do
      begin
        if union.IndexOf(b.Strings[I]) = -1 then
          union.AddObject(b.Strings[I], b.Objects[I]);
      end;
    finally
      union.EndUpdate;
    end;
  end;

var
  WhiteList, InWhitelist: TStringList;
begin
  WhiteList := TStringList.Create;
  try
    DifferenceStrings(FcxCOBAddToBlackList.Properties.Items, FcxLBBlackList.Items, WhiteList);

    InWhitelist := TStringList.Create;
    try
      IntersectionStrings(FcxLBRankedWhiteList.Items, WhiteList, InWhitelist);

      UnionStrings(InWhitelist, WhiteList, FcxLBRankedWhiteList.Items);
    finally
      InWhitelist.Free;
    end;
  finally
    WhiteList.Free;
  end;
end;

constructor THosterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDragDropIndex := -1;
  FBasisWebsiteEditor := TBasisWebsiteEditor(AOwner);

  BevelOuter := bvNone;
  Caption := '';
  ShowCaption := False;

  FcxLRankedWhiteList := TcxLabel.Create(Self);
  with FcxLRankedWhiteList do
  begin
    Parent := Self;

    Caption := 'Ranked whitelist';

    Left := 6;
    Top := 6;

    Transparent := True;
  end;

  FcxLBRankedWhiteList := TcxListBox.Create(Self);
  with FcxLBRankedWhiteList do
  begin
    Parent := Self;

    Anchors := [akLeft, akTop, akBottom];

    Left := 6;
    Top := FcxLRankedWhiteList.Top + 23;
    Width := 153;
    Height := Self.Height - Top - 16 - 6;

    DragMode := dmAutomatic;
    Enabled := False;

    OnDragOver := FcxLBRankedWhiteListDragOver;
    OnEndDrag := FcxLBRankedWhiteListEndDrag;
  end;

  FcxCBRanking := TcxCheckBox.Create(Self);
  with FcxCBRanking do
  begin
    Parent := Self;

    Anchors := [akLeft, akBottom];

    Left := 6;
    Top := Self.Height - 23;

    Caption := 'ranking';

    Transparent := True;

    with Properties do
      OnChange := FcxCBRankingChange;
  end;

  FcxLBlackList := TcxLabel.Create(Self);
  with FcxLBlackList do
  begin
    Parent := Self;

    Caption := 'Blacklist';

    Left := 176;
    Top := 6;

    Transparent := True;
  end;

  FcxCOBAddToBlackList := TcxComboBox.Create(Self);
  with FcxCOBAddToBlackList do
  begin
    Parent := Self;

    Left := FcxLBlackList.Left;
    Top := FcxLBlackList.Top + 23;
    Width := 153;

    with Properties do
      DropDownListStyle := lsEditFixedList;
  end;

  with TcxComboBoxProperties(FcxCOBAddToBlackList.Properties) do
  begin
    OnChange := FcxCOBAddToBlackListPropertiesChange;
    OnButtonClick := FcxCOBAddToBlackListPropertiesButtonClick;

    with TcxEditButton(Buttons.Add) do
    begin
      Caption := 'ADD';
      LeftAlignment := True;
      Kind := bkText;
      Hint := 'Add to blacklist';
      ShowHint := True;
      Enabled := False;
    end;
  end;

  FcxLBBlackList := TcxListBox.Create(Self);
  with FcxLBBlackList do
  begin
    Parent := Self;

    Anchors := [akLeft, akTop, akBottom];

    Left := FcxLBlackList.Left;
    Top := FcxCOBAddToBlackList.Top + 27;
    Width := FcxCOBAddToBlackList.Width;
    Height := Self.Height - Top - 16 - 6;

    OnClick := FcxLBBlackListClick;
  end;

  FcxBRemoveFromBlackList := TcxButton.Create(Self);
  with FcxBRemoveFromBlackList do
  begin
    Parent := Self;

    Anchors := [akLeft, akBottom];

    Caption := 'Remove';

    Width := 50;
    Height := 16;

    Left := FcxLBBlackList.Left + FcxLBBlackList.Width - Width;
    Top := Self.Height - Height - 3;

    Enabled := False;

    OnClick := FcxBRemoveFromBlackListClick;
  end;
end;

destructor THosterPanel.Destroy;
begin

  FcxBRemoveFromBlackList.Free;

  FcxLBBlackList.Free;

  FcxCOBAddToBlackList.Free;

  FcxLBlackList.Free;

  FcxCBRanking.Free;

  FcxLBRankedWhiteList.Free;

  FcxLRankedWhiteList.Free;

  inherited Destroy;
end;

{ TBasisWebsiteEditor }

procedure TBasisWebsiteEditor.Show(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure TBasisWebsiteEditor.FcxBAcceptClick(Sender: TObject);

  procedure SubAdd(ATreeNode: TTreeNode; ANode: IXMLNode; AType: string);
  var
    I: Integer;
    _tempnode: IXMLNode;
  begin
    if ATreeNode.HasChildren then
      for I := 0 to ATreeNode.Count - 1 do
      begin
        _tempnode := ANode.AddChild('subtype');

        with TTextB.Create(ATreeNode.Item[I].Text) do
        begin
          with _tempnode do
          begin
            Attributes['type'] := StringComponentID;
            Attributes['value'] := StringComponentValue;
            Attributes[AType] := ID;
          end;
        end;
        SubAdd(ATreeNode.Item[I], _tempnode, AType);
      end;
  end;

var
  XMLDoc: IXMLDocument;

  I, Y: Integer;

  _tempnode: IXMLNode;
begin
  CoInitialize(nil);
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        NodeIndentStr := #9;

        LoadFromFile(WebsiteSettingsFileName);

        Active := True;

        with DocumentElement do
          if HasChildNodes then
          begin
            if ChildNodes.FindNode('charset') = nil then
              AddChild('charset');
            ChildNodes.Nodes['charset'].NodeValue := FcxCOBCharset.Text;

            if ChildNodes.FindNode('settings') = nil then
              AddChild('settings');

            for I := 0 to length(FEditArray) - 1 do
            begin
              with FEditArray[I] do
                if FTop then
                  ChildNodes.Nodes['name'].Attributes[FName] := FTextEdit.Text
                else
                  with ChildNodes.Nodes['settings'] do
                  begin
                    if ChildNodes.FindNode(FName) = nil then
                      AddChild(FName);
                    ChildNodes.Nodes[FName].NodeValue := FTextEdit.Text;
                  end;
            end;

            for I := 0 to length(FCheckboxArray) - 1 do
            begin
              with FCheckboxArray[I] do
                if FTop then
                  ChildNodes.Nodes['name'].Attributes[FName] := FCheckBox.Checked
                else
                  with ChildNodes.Nodes['settings'] do
                  begin
                    if ChildNodes.FindNode(FName) = nil then
                      AddChild(FName);
                    ChildNodes.Nodes[FName].NodeValue := FCheckBox.Checked;
                  end;
            end;

            for I := 0 to FcxTCIDs.Tabs.Count - 1 do
            begin
              if ChildNodes.FindNode(FcxTCIDs.Tabs.Tabs[I].Caption) = nil then
                AddChild(FcxTCIDs.Tabs.Tabs[I].Caption);
              with ChildNodes.Nodes[FcxTCIDs.Tabs.Tabs[I].Caption] do
              begin
                ChildNodes.Clear;

                with FIDPanelList.Items[I].FcxTreeView do
                  for Y := 0 to Items.Count - 1 do
                    if (Items.Item[Y].Level = 0) then
                    begin
                      _tempnode := AddChild('type');
                      with _tempnode do
                        with Items.Item[Y] do
                          with TTextA.Create(Text) do
                          begin
                            Attributes['name'] := StringTemplateTypeID;
                            Attributes['id'] := ID;
                          end;
                      SubAdd(Items.Item[Y], _tempnode, 'id');
                    end;
              end;
            end;

            if ChildNodes.FindNode('filters') = nil then
              AddChild('filters');
            with ChildNodes.Nodes['filters'] do
            begin
              // category/control-based-filter
              Attributes['active'] := FcxCBFilterEnabled.Checked;

              if ChildNodes.FindNode('categories') = nil then
                AddChild('categories');
              ChildNodes.Nodes['categories'].NodeValue := FcxCCBFilterCategories.Text;

              if ChildNodes.FindNode('controls') = nil then
                AddChild('controls');
              with ChildNodes.Nodes['controls'] do
                with FcxGridFilterControlsTableView.DataController do
                begin
                  ChildNodes.Clear;
                  for I := 0 to RecordCount - 1 do
                  begin
                    with AddChild('control') do
                    begin
                      Attributes['name'] := VarToStr(Values[I, FcxGridFilterControlsTableViewColumn1.index]);
                      Attributes['rel'] := VarToStr(Values[I, FcxGridFilterControlsTableViewColumn2.index]);
                      NodeValue := VarToStr(Values[I, FcxGridFilterControlsTableViewColumn3.index]);
                    end;
                  end;
                end;

              // hoster-based-filter
              for I := 0 to FHosterPanelList.Count - 1 do
              begin
                if ChildNodes.FindNode(FHosterPanelList.Items[I].GetHosterName) = nil then
                  AddChild(FHosterPanelList.Items[I].GetHosterName);
                with ChildNodes.Nodes[FHosterPanelList.Items[I].GetHosterName] do
                  with FHosterPanelList.Items[I] do
                  begin
                    Attributes['ranked'] := FcxCBRanking.Checked;

                    if ChildNodes.FindNode('blacklist') = nil then
                      AddChild('blacklist');

                    with TStringList.Create do
                      try
                        Text := FcxLBBlackList.Items.Text;
                        Delimiter := ';';
                        ChildNodes.Nodes['blacklist'].NodeValue := DelimitedText;
                      finally
                        Free;
                      end;

                    if ChildNodes.FindNode('whitelist') = nil then
                      AddChild('whitelist');
                    with TStringList.Create do
                      try
                        Text := FcxLBRankedWhiteList.Items.Text;
                        Delimiter := ';';
                        ChildNodes.Nodes['whitelist'].NodeValue := DelimitedText;
                      finally
                        Free;
                      end;
                  end;
              end;
            end;

            if FcxTSCustomFields.TabVisible then
            begin
              if ChildNodes.FindNode('customfields') = nil then
                AddChild('customfields');
              with ChildNodes.Nodes['customfields'] do
              begin
                ChildNodes.Clear;

                with FcxGridCustomFieldsTableView do
                  with DataController do
                  begin
                    for I := 0 to RecordCount - 1 do
                      with AddChild('customfield') do
                      begin
                        Attributes['name'] := VarToStr(Values[I, 0]);
                        NodeValue := VarToStr(Values[I, 1]);
                      end;
                  end;
              end;
            end;
          end;
      end;

      XMLDoc.SaveToFile(WebsiteSettingsFileName);
    finally
      XMLDoc := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TBasisWebsiteEditor.FcxTCIDsChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FIDPanelList.Count - 1 do
    FIDPanelList.Items[I].Visible := (FcxTCIDs.TabIndex = I);
end;

procedure TBasisWebsiteEditor.FcxCBFilterEnabledChange(Sender: TObject);
begin
  FcxCCBFilterCategories.Enabled := FcxCBFilterEnabled.Checked;
  FcxGridFilterControls.Enabled := FcxCBFilterEnabled.Checked;
end;

procedure TBasisWebsiteEditor.FcxGridFilterControlsTableViewColumn3PropertiesInitPopup(Sender: TObject);
var
  LTypeID: TTypeID;
  LControlID: TControlID;
  StringList: TStringList;
begin
  with FcxGridFilterControlsTableView.DataController do
    LControlID := StringToControlID(Values[GetFocusedRecordIndex, FcxGridFilterControlsTableViewColumn1.index]);

  with TStringList.Create do
    try
      Sorted := True;
      Duplicates := dupIgnore;

      for LTypeID := Low(TTypeID) to High(TTypeID) do
      begin
        StringList := TStringList.Create;
        try
          StringList.Text := FAppController.GetControlValues(LTypeID, LControlID);
          AddStrings(StringList);
        finally
          StringList.Free;
        end;
      end;

      TcxComboBoxProperties(FcxGridFilterControlsTableViewColumn3.Properties).Items.Text := Text;
      TcxComboBox(Sender).Properties.Items.Text := Text;
    finally
      Free;
    end;
end;

procedure TBasisWebsiteEditor.FcxGridFilterControlsTableViewColumn4GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption;
  var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
begin
  with FcxGridFilterControlsTableView.DataController do
    if NewItemRowFocused then
      AHintText := 'Add new detail filter'
    else
      AHintText := 'Remove selected detail filter';
end;

procedure TBasisWebsiteEditor.FcxGridFilterControlsTableViewColumn4PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  with FcxGridFilterControlsTableView.DataController do
    if NewItemRowFocused then
      Insert
    else
      DeleteRecord(GetFocusedRecordIndex);
end;

procedure TBasisWebsiteEditor.FcxTCHosterChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FHosterPanelList.Count - 1 do
    FHosterPanelList.Items[I].Visible := (FcxTCHoster.TabIndex = I);
end;

procedure TBasisWebsiteEditor.FcxGridCustomFieldsTableViewColumn3GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption;
  var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
begin
  with FcxGridCustomFieldsTableView.DataController do
    if NewItemRowFocused then
      AHintText := 'Add new custom field'
    else
      AHintText := 'Remove selected custom field';
end;

procedure TBasisWebsiteEditor.FcxGridCustomFieldsTableViewColumn3PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  with FcxGridCustomFieldsTableView.DataController do
    if NewItemRowFocused then
      Insert
    else
      DeleteRecord(GetFocusedRecordIndex);
end;

function TBasisWebsiteEditor.GetCustomFields: WordBool;
begin
  result := FcxTSCustomFields.TabVisible;
end;

procedure TBasisWebsiteEditor.SetCustomFields(ACustomFields: WordBool);
begin
  FcxTSCustomFields.TabVisible := ACustomFields;
end;

constructor TBasisWebsiteEditor.Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName);
var
  LTypeID: TTypeID;
  LControlID: TControlID;
begin
  CreateNew(nil);

  FCMSPlugIn := ACMSPlugIn;
  FAppController := AAppController;
  FWebsiteSettingsFileName := AWebsiteSettingsFileName;

  ClientHeight := 331;
  ClientWidth := 635;
  Position := poMainFormCenter;

  OnShow := Show;

  FcxPCWebsiteSettings := TcxPageControl.Create(Self);
  with FcxPCWebsiteSettings do
  begin
    Parent := Self;

    Left := 8;
    Top := 8;
    Width := Self.ClientWidth - 16;
    Height := Self.ClientHeight - 16 - 31;

    Focusable := False;

    Anchors := [akLeft, akTop, akRight, akBottom];
  end;

  FcxTSGeneral := TcxTabSheet.Create(FcxPCWebsiteSettings);
  with FcxTSGeneral do
  begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'General';
  end;

  FFlowPanel := TFlowPanel.Create(FcxTSGeneral);
  with FFlowPanel do
  begin
    Parent := FcxTSGeneral;

    BevelOuter := bvNone;
    Caption := '';
    ShowCaption := False;

    Left := 8;
    Top := 8;
    Width := Self.ClientWidth - 16;
    Height := Self.ClientHeight - 16;

    Anchors := [akLeft, akTop, akRight, akBottom];
  end;

  FcxTSIDs := TcxTabSheet.Create(FcxPCWebsiteSettings);
  with FcxTSIDs do
  begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'IDs';
  end;

  FIDPanelList := TList<TIDPanel>.Create;

  FcxTCIDs := TcxTabControl.Create(FcxTSIDs);
  with FcxTCIDs do
  begin
    Parent := FcxTSIDs;

    Left := 3;
    Top := 3;
    Width := FcxTSIDs.Width - 6;
    Height := FcxTSIDs.Height - 6;

    Focusable := False;

    Anchors := [akLeft, akTop, akRight, akBottom];

    OnChange := FcxTCIDsChange;
  end;

  FcxTSFilter := TcxTabSheet.Create(FcxPCWebsiteSettings);
  with FcxTSFilter do
  begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'Filter';
  end;

  FFilterPanel := TPanel.Create(FcxTSFilter);
  with FFilterPanel do
  begin
    Parent := FcxTSFilter;

    Anchors := [akLeft, akTop, akRight, akBottom];

    BevelOuter := bvNone;
    Caption := '';
    ShowCaption := False;

    Left := 8;
    Top := 8;
    Width := Self.ClientWidth - 16;
    Height := Self.ClientHeight - 16;
  end;

  FcxCBFilterEnabled := TcxCheckBox.Create(FFilterPanel);
  with FcxCBFilterEnabled do
  begin
    Parent := FFilterPanel;

    Left := 0;
    Top := 0;
    Width := 250;

    Checked := False;

    Caption := 'Enable category and control filter';

    Transparent := True;

    with Properties do
      OnChange := FcxCBFilterEnabledChange;
  end;

  FcxLFilterCategories := TcxLabel.Create(FFilterPanel);
  with FcxLFilterCategories do
  begin
    Parent := FFilterPanel;

    Left := FcxCBFilterEnabled.Left;
    Top := FcxCBFilterEnabled.Top + FcxCBFilterEnabled.Height + 6;

    Caption := 'Category filter:';

    Transparent := True;
  end;

  FcxCCBFilterCategories := TMycxCheckComboBox.Create(FFilterPanel);
  with FcxCCBFilterCategories do
  begin
    Parent := FFilterPanel;

    Left := FcxCBFilterEnabled.Left;
    Top := FcxLFilterCategories.Top + FcxLFilterCategories.Height + 3;
    Width := 150;

    Enabled := False;

    with Properties do
    begin
      EmptySelectionText := '';
      for LTypeID := Low(TTypeID) to High(TTypeID) do
        with Items.Add do
        begin
          Enabled := True;
          Description := TypeIDToString(LTypeID);
        end;
    end;

    CheckAll;
  end;

  FcxLFilterControls := TcxLabel.Create(FFilterPanel);
  with FcxLFilterControls do
  begin
    Parent := FFilterPanel;

    Left := FcxCBFilterEnabled.Left;
    Top := FcxCCBFilterCategories.Top + FcxCCBFilterCategories.Height + 6;

    Caption := 'Control filter:';

    Transparent := True;
  end;

  FcxGridFilterControls := TcxGrid.Create(FFilterPanel);
  with FcxGridFilterControls do
  begin
    Parent := FFilterPanel;

    Anchors := [akLeft, akTop, akRight, akBottom];

    Left := FcxCBFilterEnabled.Left;
    Top := FcxLFilterControls.Top + FcxLFilterControls.Height + 3;
    Height := 145;
    Width := 593;

    Enabled := False;

    FcxGridFilterControlsLevel := Levels.Add;
    FcxGridFilterControlsTableView := CreateView(TcxGridTableView) as TcxGridTableView;
    FcxGridFilterControlsLevel.GridView := FcxGridFilterControlsTableView;

    with FcxGridFilterControlsTableView.NewItemRow do
      Visible := True;

    with FcxGridFilterControlsTableView.OptionsBehavior do
      FocusCellOnTab := True;

    with FcxGridFilterControlsTableView.OptionsView do
    begin
      ColumnAutoWidth := True;

      GroupByBox := False;
      ScrollBars := ssVertical;

      ShowEditButtons := gsebAlways;
    end;

    FcxGridFilterControlsTableViewColumn1 := FcxGridFilterControlsTableView.CreateColumn;
    with FcxGridFilterControlsTableViewColumn1 do
    begin
      Caption := 'Control name';
      PropertiesClass := TcxComboBoxProperties;
      with Properties as TcxComboBoxProperties do
        for LControlID := Low(TControlID) to High(TControlID) do
          Items.Add(ControlIDToString(LControlID));
    end;

    FcxGridFilterControlsTableViewColumn2 := FcxGridFilterControlsTableView.CreateColumn;
    with FcxGridFilterControlsTableViewColumn2 do
    begin
      Caption := 'Relationship';
      PropertiesClass := TcxComboBoxProperties;
      with Properties as TcxComboBoxProperties do
      begin
        DropDownListStyle := lsFixedList;

        Items.Add('=');
        Items.Add('≠');
      end;
      Width := 20;
    end;

    FcxGridFilterControlsTableViewColumn3 := FcxGridFilterControlsTableView.CreateColumn;
    with FcxGridFilterControlsTableViewColumn3 do
    begin
      Caption := 'Control value';
      PropertiesClass := TcxComboBoxProperties;
      with Properties as TcxComboBoxProperties do
      begin
        OnInitPopup := FcxGridFilterControlsTableViewColumn3PropertiesInitPopup;
      end;
    end;

    FcxGridFilterControlsTableViewColumn4 := FcxGridFilterControlsTableView.CreateColumn;
    with FcxGridFilterControlsTableViewColumn4 do
    begin
      Caption := 'Add/Delete';
      PropertiesClass := TcxButtonEditProperties;
      with Options do
      begin
        Filtering := False;
        HorzSizing := False;
      end;
      with Properties as TcxButtonEditProperties do
      begin
        ViewStyle := vsButtonsOnly;

        OnButtonClick := FcxGridFilterControlsTableViewColumn4PropertiesButtonClick;
      end;
      Width := 20;

      OnGetCellHint := FcxGridFilterControlsTableViewColumn4GetCellHint;
    end;
  end;

  FcxTSHoster := TcxTabSheet.Create(FcxPCWebsiteSettings);
  with FcxTSHoster do
  begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'Hoster';
  end;

  FHosterPanelList := TList<THosterPanel>.Create;

  FcxTCHoster := TcxTabControl.Create(FcxTSHoster);
  with FcxTCHoster do
  begin
    Parent := FcxTSHoster;

    Anchors := [akLeft, akTop, akRight, akBottom];

    Left := 3;
    Top := 3;
    Width := FcxTSHoster.Width - 6;
    Height := FcxTSHoster.Height - 6;

    Focusable := False;

    AddHosterTab(htFile);
    AddHosterTab(htImage);

    OnChange := FcxTCHosterChange;
  end;

  FcxTSCustomFields := TcxTabSheet.Create(FcxPCWebsiteSettings);
  with FcxTSCustomFields do
  begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'CustomFields';

    TabVisible := CustomFields;
  end;

  CustomFields := False;

  FcxGridCustomFields := TcxGrid.Create(FcxTSCustomFields);
  with FcxGridCustomFields do
  begin
    Parent := FcxTSCustomFields;

    Anchors := [akLeft, akTop, akRight, akBottom];

    Height := FcxTSCustomFields.Height - 12;
    Left := 6;
    Top := 6;
    Width := FcxTSCustomFields.Width - 12;

    FcxGridCustomFieldsLevel := Levels.Add;
    FcxGridCustomFieldsTableView := CreateView(TcxGridTableView) as TcxGridTableView;
    FcxGridCustomFieldsLevel.GridView := FcxGridCustomFieldsTableView;

    with FcxGridCustomFieldsTableView.NewItemRow do
      Visible := True;

    with FcxGridCustomFieldsTableView.OptionsBehavior do
      FocusCellOnTab := True;

    with FcxGridCustomFieldsTableView.OptionsView do
    begin
      ColumnAutoWidth := True;

      GroupByBox := False;
      ScrollBars := ssVertical;

      ShowEditButtons := gsebAlways;
    end;

    FcxGridCustomFieldsTableViewColumn1 := FcxGridCustomFieldsTableView.CreateColumn;
    with FcxGridCustomFieldsTableViewColumn1 do
    begin
      Caption := 'Name';
      PropertiesClass := TcxTextEditProperties;
    end;

    FcxGridCustomFieldsTableViewColumn2 := FcxGridCustomFieldsTableView.CreateColumn;
    with FcxGridCustomFieldsTableViewColumn2 do
    begin
      Caption := 'Value';
      PropertiesClass := TcxBlobEditProperties;
      with Properties as TcxBlobEditProperties do
      begin
        BlobEditKind := bekMemo;
        BlobPaintStyle := bpsText;

        MemoScrollBars := ssBoth;
      end;
    end;

    FcxGridCustomFieldsTableViewColumn3 := FcxGridCustomFieldsTableView.CreateColumn;
    with FcxGridCustomFieldsTableViewColumn3 do
    begin
      Caption := 'Add/Delete';
      PropertiesClass := TcxButtonEditProperties;
      with Options do
      begin
        Filtering := False;
        HorzSizing := False;
      end;
      with Properties as TcxButtonEditProperties do
      begin
        ViewStyle := vsButtonsOnly;

        OnButtonClick := FcxGridCustomFieldsTableViewColumn3PropertiesButtonClick;
      end;
      Width := 20;

      OnGetCellHint := FcxGridCustomFieldsTableViewColumn3GetCellHint;
    end;
  end;

  FcxLCharset := TcxLabel.Create(Self);
  with FcxLCharset do
  begin
    Parent := Self;

    Left := 8;
    Top := Self.ClientHeight - Height - 11;

    Caption := 'Charset:';

    Anchors := [akLeft, akBottom];
  end;

  FcxCOBCharset := TcxComboBox.Create(Self);
  with FcxCOBCharset do
  begin
    Parent := Self;

    Left := FcxLCharset.Left + FcxLCharset.Width + 6;
    Top := Self.ClientHeight - Height - 8;

    with Properties do
    begin
      DropDownListStyle := lsFixedList;
      Items.Text := THTTPIndyHelper.Charsets;
    end;

    Anchors := [akLeft, akBottom];
  end;

  FcxBCancel := TcxButton.Create(Self);
  with FcxBCancel do
  begin
    Parent := Self;

    Left := Self.ClientWidth - Width - 75 - 6 - 8;
    Top := Self.ClientHeight - Height - 8;

    Cancel := True;
    Caption := 'Cancel';

    ModalResult := mrCancel;

    Anchors := [akRight, akBottom];
  end;

  FcxBAccept := TcxButton.Create(Self);
  with FcxBAccept do
  begin
    Parent := Self;

    Left := Self.ClientWidth - Width - 8;
    Top := Self.ClientHeight - Height - 8;

    Default := True;
    Caption := 'Accept';

    ModalResult := mrOk;

    Anchors := [akRight, akBottom];

    OnClick := FcxBAcceptClick;
  end;
end;

procedure TBasisWebsiteEditor.AddEdit(AName: WideString; ADefaultValue: WideString = ''; ATopValue: WordBool = False);
var
  SettingsEdit: TSettingsEdit;
begin
  with SettingsEdit do
  begin
    FBasis := TPanel.Create(Self);
    with FBasis do
    begin
      Parent := FFlowPanel;

      BevelOuter := bvNone;

      Caption := '';
      ParentColor := True;
    end;

    FTitle := TcxLabel.Create(FBasis);
    with FTitle do
    begin
      Parent := FBasis;

      Top := 0;
      Left := 0;

      Caption := AName;
      Transparent := True;

      Show;
    end;

    FTextEdit := TcxTextEdit.Create(FBasis);
    with FTextEdit do
    begin
      Parent := FBasis;

      Top := 16;
      Left := 0;
      Width := FBasis.Width;

      Anchors := [akLeft, akTop, akRight];

      Text := ADefaultValue;
    end;
    FName := AName;
    FTop := ATopValue;
  end;

  SetLength(FEditArray, length(FEditArray) + 1);
  FEditArray[length(FEditArray) - 1] := SettingsEdit;
end;

procedure TBasisWebsiteEditor.AddCheckbox(AName: WideString; ADefaultValue: WordBool = False; ATopValue: WordBool = False);
const
  CheckboxInfo: array [0 .. 60, 0 .. 2] of string = ( { }
    ('use_plainlinks', 'Plainlinks', 'if active IntelligeN uses directlinks, otherwise it uses the links form the first crypter'), { }
    ('use_textasdescription', 'text as description',
      'activate this to use the IScript-Message template' + sLineBreak + 'to be posted in the description field, otherwise the' + sLineBreak + 'normal description ll be posted in the description field'), { }
    ('htmlon', 'Html Code', ''), { }
    ('allowimgcode', '[img] Code', ''), { }
    ('parseurloff', 'Disable Parse URL', ''), { }
    ('smileyoff', 'Disable Smilies', ''), { }
    ('bbcodeoff', 'Disable Discuz! Code', ''), { }
    ('tagoff', 'Disable Parse Tag', ''), { }
    ('usesig', 'Show Signature', ''), { }
    ('addtoblog', 'Add to blog', ''), { }
    ('allowimgurl', 'Auto image links', ''), { }
    ('ordertype', 'Reverse replies', ''), { }
    ('allownoticeauthor', 'Notify on reply', ''), { }
    ('noshort', 'no short', 'no short description inside full description'), { }
    ('approve', 'Publish article', ''), { }
    ('allow_main', 'Show in main page', ''), { }
    ('allow_comm', 'Allow comments', ''), { }
    ('allow_rating', 'Allow article rating', ''), { }
    ('hide_smilies', 'hide smilies', 'Never show smilies as icons for this post'), { }
    ('enableemo', 'Enable emoticons', ''), { }
    ('enablesig', 'Enable signature', ''), { }
    ('use_coverlink', '', ''), { }
    ('signature', 'Signature', 'include your signature'), { }
    ('disablesmilies', 'Disable Smilies', 'disable smilies from showing in this post'), { }
    ('round_size', 'round size', ''), { }
    ('disable_bbcode', 'Disable BBCode in this post', ''), { }
    ('disable_smilies', 'Disable Smilies in this post', ''), { }
    ('notify', 'Notify me when a reply is posted', ''), { }
    ('disable_magic_url', 'Do not automatically parse URLs', ''), { }
    ('attach_sig', 'Attach a signature', ''), { }
    ('lock_topic', 'Lock topic', ''), { }
    ('showsig', 'Show Signature', ''), { }
    ('ns', 'Don''t use smileys', ''), { }
    ('extra_login', 'extra login', 'activate this if you have to login before you make a new post'), { }
    ('need_captcha', 'need captcha', 'activate this if the website requests a CAPTCHA'), { }
    ('hidden', 'Hidden', 'this attribute marks the entry as hidden'), { }
    ('unchecked', 'Untested', 'this attribute marks the entry as untested'), { }
    ('error_report', 'Error reported', 'this attribute is set automatically, when the upload is reported as incorrect'), { }
    ('protected', 'Protected', 'this attribute marks the entry as a CAPTCHA protected'), { }
    ('parseurl', 'Automatically parse links in text', 'Will turn www.example.com into [URL]http://www.example.com[/URL].'), { }
    ('openclose', '', ''), { }
    ('stickunstick', '', ''), { }
    ('thxbot', '', ''), { }
    ('emailnotify', '', ''), { }
    ('disablehtml', '', ''), { }
    ('disablebbcode', '', ''), { }
    ('disableimages', '', ''), { }
    ('showsignature', '', ''), { }
    ('parseURL', 'Convert URLs', 'Automatically converts internet addresses into links by adding [url] and [/url] around them'), { }
    ('enableSmilies', 'Enable smileys in this message', 'Smiley code in your message such as :) is automatically displayed as image'), { }
    ('enableBBCodes', 'Enable BBCode in this message', 'You can use BBCode to format your message, if this option is enabled'), { }
    ('showSignature', 'Show signature', 'Do you want to display your signature in this message?'), { }
    ('hasThank', 'Activate Thankomat', 'Activate the Thankomat for this post'), { }
    ('closeThread', 'Close thread', 'Closes the thread after this post'), { }
    ('draft', 'Draft', ''), { }
    ('intelligent_posting', '', 'Before this plugin creates a new post,' + sLineBreak + 'it checks whether there are similar' + sLineBreak + 'entries and posts in this (if existing)'),
    { }
    ('intelligent_posting_helper', '', 'a dialog will popup to let you verify the search'), { }
    ('intelligent_posting_mask_search', '', 'surrounds search value with quotation marks (")'), { }
    ('intelligent_posting_keepshortwords', '', ''), { }
    ('intelligent_posting_alternativesearch', '', ''), { }
    ('intelligent_posting_boundedsearch', '', 'searches only in the defined board-IDs') { }
  );
var
  SettingsCheckbox: TSettingsCheckbox;
  I: Integer;
begin
  with SettingsCheckbox do
  begin
    FCheckBox := TcxCheckBox.Create(Self);
    with FCheckBox do
    begin
      Parent := FFlowPanel;

      Width := 185; // default width of TPanel

      Checked := ADefaultValue;

      Caption := AName;

      for I := 0 to length(CheckboxInfo) - 1 do
        if SameStr(AName, CheckboxInfo[I][0]) then
        begin
          if not SameStr('', CheckboxInfo[I][1]) then
            Caption := CheckboxInfo[I][1];

          if not SameStr('', CheckboxInfo[I][2]) then
          begin
            Hint := CheckboxInfo[I][2];
            ShowHint := True;
          end;
        end;

      Transparent := True;
    end;
    FName := AName;
    FTop := ATopValue;
  end;

  SetLength(FCheckboxArray, length(FCheckboxArray) + 1);
  FCheckboxArray[length(FCheckboxArray) - 1] := SettingsCheckbox;
end;

procedure TBasisWebsiteEditor.AddCategoryTab(AName: WideString);
var
  NewIDPanel: TIDPanel;
begin
  FcxTCIDs.Tabs.Add(AName);

  NewIDPanel := TIDPanel.Create(Self);
  with NewIDPanel do
  begin
    Parent := FcxTCIDs;

    Align := alClient;

    Visible := False;
  end;

  FIDPanelList.Add(NewIDPanel);
end;

procedure TBasisWebsiteEditor.AddHosterTab(AHosterType: THosterType);
var
  NewHosterPanel: THosterPanel;
begin
  case AHosterType of
    htFile:
      FcxTCHoster.Tabs.Add('File Hoster');
    htImage:
      FcxTCHoster.Tabs.Add('Image Hoster');
  end;

  NewHosterPanel := THosterPanel.Create(Self);
  with NewHosterPanel do
  begin
    Parent := FcxTCHoster;

    Anchors := [akLeft, akTop, akRight, akBottom];

    Left := 4;
    Top := 24;
    Width := FcxTCHoster.Width - 8;
    Height := FcxTCHoster.Height - Top - 4;

    HosterType := AHosterType;

    Visible := False;
  end;

  FHosterPanelList.Add(NewHosterPanel);
end;

function TBasisWebsiteEditor.ShowModal: Integer;

  procedure SubAdd(AcxTreeView: TcxTreeView; ATreeNode: TTreeNode; ANode: IXMLNode);
  var
    z: Integer;
    s: string;
  begin
    with ANode do
      if HasChildNodes then
        for z := 0 to ChildNodes.Count - 1 do
          if ChildNodes.Nodes[z].NodeType = ntElement then
          begin
            s := '="' + TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[z]) + '" ' + VarToStr(ChildNodes.Nodes[z].Attributes['type']) + '="' + VarToStr(ChildNodes.Nodes[z].Attributes['value']) + '"';
            SubAdd(AcxTreeView, AcxTreeView.Items.AddChild(ATreeNode, s), ChildNodes.Nodes[z]);
          end;
  end;

  function FetchOldHosterBlackList(AHosters: string; ACurrentHosters: TStrings): string;
  var
    I: Integer;
  begin
    with TStringList.Create do
      try
        for I := 0 to ACurrentHosters.Count - 1 do
          if (Pos(ACurrentHosters.Strings[I], AHosters) > 0) then
            Add(ACurrentHosters.Strings[I]);

        result := Text;
      finally
        Free;
      end;
  end;

var
  XMLDoc: IXMLDocument;

  I, Y: Integer;
begin
  CoInitialize(nil);
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        NodeIndentStr := #9;

        LoadFromFile(WebsiteSettingsFileName);

        Active := True;

        with DocumentElement do
          if HasChildNodes then
          begin
            if Assigned(ChildNodes.FindNode('charset')) and not SameStr('', VarToStrDef(ChildNodes.Nodes['charset'].NodeValue, '')) then
              FcxCOBCharset.Text := ChildNodes.Nodes['charset'].NodeValue
            else
              FcxCOBCharset.Text := FCMSPlugIn.DefaultCharset;

            for I := 0 to length(FEditArray) - 1 do
            begin
              with FEditArray[I] do
                if FTop then
                begin
                  with ChildNodes.Nodes['name'] do
                    if Assigned(AttributeNodes.FindNode(FName)) then
                      FTextEdit.Text := VarToStr(Attributes[FName]);
                end
                else
                begin
                  with ChildNodes.Nodes['settings'] do
                    if Assigned(ChildNodes.FindNode(FName)) then
                      FTextEdit.Text := VarToStr(ChildNodes.Nodes[FName].NodeValue);
                end;
            end;

            for I := 0 to length(FCheckboxArray) - 1 do
            begin
              with FCheckboxArray[I] do
                if FTop then
                begin
                  with ChildNodes.Nodes['name'] do
                    if Assigned(AttributeNodes.FindNode(FName)) then
                      FCheckBox.Checked := Attributes[FName];
                end
                else
                begin
                  with ChildNodes.Nodes['settings'] do
                    if Assigned(ChildNodes.FindNode(FName)) then
                      FCheckBox.Checked := ChildNodes.Nodes[FName].NodeValue;
                end;
            end;

            for I := 0 to FcxTCIDs.Tabs.Count - 1 do
            begin
              with ChildNodes.Nodes[FcxTCIDs.Tabs.Tabs[I].Caption] do
              begin
                if HasChildNodes then
                  for Y := 0 to ChildNodes.Count - 1 do
                    SubAdd(FIDPanelList.Items[I].FcxTreeView, FIDPanelList.Items[I].FcxTreeView.Items.AddChild(nil, VarToStr(ChildNodes.Nodes[Y].Attributes['name']) + ' ="' + TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[Y]) + '"'),
                      ChildNodes.Nodes[Y]);
              end;
              with FIDPanelList.Items[I] do
                CanEditandDelete(FcxTreeView.Items.Count > 0);
            end;

            if Assigned(ChildNodes.FindNode('filters')) then
            begin
              with ChildNodes.Nodes['filters'] do
              begin
                // category/control-based-filter
                FcxCBFilterEnabled.Checked := Attributes['active'];

                FcxCCBFilterCategories.TextValue := VarToStr(ChildNodes.Nodes['categories'].NodeValue);

                if Assigned(ChildNodes.FindNode('controls')) then
                  with ChildNodes.Nodes['controls'] do
                    with FcxGridFilterControlsTableView.DataController do
                    begin
                      RecordCount := ChildNodes.Count;

                      for I := 0 to ChildNodes.Count - 1 do
                      begin
                        Values[I, FcxGridFilterControlsTableViewColumn1.index] := VarToStr(ChildNodes.Nodes[I].Attributes['name']);
                        Values[I, FcxGridFilterControlsTableViewColumn2.index] := VarToStr(ChildNodes.Nodes[I].Attributes['rel']);
                        Values[I, FcxGridFilterControlsTableViewColumn3.index] := VarToStr(ChildNodes.Nodes[I].NodeValue);
                      end;
                    end;

                // hoster-based-filter
                for I := 0 to FHosterPanelList.Count - 1 do
                  with ChildNodes.Nodes[FHosterPanelList.Items[I].GetHosterName] do
                    with FHosterPanelList.Items[I] do
                    begin
                      FcxCBRanking.Checked := StrToBoolDef(VarToStr(Attributes['ranked']), False);

                      if not SameStr('', VarToStr(ChildNodes.Nodes['blacklist'].NodeValue)) then
                        with SplittString(';', VarToStr(ChildNodes.Nodes['blacklist'].NodeValue)) do
                          try
                            FcxLBBlackList.Items.Text := Text;
                          finally
                            Free;
                          end;

                      if not SameStr('', VarToStr(ChildNodes.Nodes['whitelist'].NodeValue)) then
                        with SplittString(';', VarToStr(ChildNodes.Nodes['whitelist'].NodeValue)) do
                          try
                            FcxLBRankedWhiteList.Items.Text := Text;
                          finally
                            Free;
                          end;

                      UpdateRankedWhiteList;
                    end;
              end;
            end
            else
            begin
              with ChildNodes.Nodes['settings'] do
                if Assigned(ChildNodes.FindNode('hoster_blacklist')) then
                begin
                  for I := 0 to FHosterPanelList.Count - 1 do
                    with FHosterPanelList.Items[I] do
                      if HosterType = htFile then
                      begin
                        FcxLBBlackList.Items.Text := FetchOldHosterBlackList(VarToStr(ChildNodes.Nodes['hoster_blacklist'].NodeValue), FcxCOBAddToBlackList.Properties.Items);
                        UpdateRankedWhiteList;
                      end;
                end;
            end;

            if FcxTSCustomFields.TabVisible then
            begin
              with ChildNodes.Nodes['customfields'] do
                if HasChildNodes then
                  for I := 0 to ChildNodes.Count - 1 do
                  begin
                    if not ChildNodes.Nodes[I].HasAttribute('name') then
                      raise Exception.Create('no name attribute');

                    with FcxGridCustomFieldsTableView do
                      with DataController do
                        try
                          BeginUpdate;

                          RecordCount := I + 1;
                          Values[I, 0] := VarToStr(ChildNodes.Nodes[I].Attributes['name']);
                          Values[I, 1] := VarToStr(ChildNodes.Nodes[I].NodeValue);
                        finally
                          EndUpdate;
                        end;
                  end;
            end;
          end;
      end;

    finally
      XMLDoc := nil;
    end;
  finally
    CoUninitialize;
  end;

  if FIDPanelList.Count > 0 then
  begin
    FIDPanelList.Items[0].Visible := True;

    FIDGrabberThread := TIDGrabberThread.Create(Self.FCMSPlugIn, FIDPanelList.Items[0]);
    FIDGrabberThread.Start;
  end;

  FcxTCHoster.OnChange(FcxTCHoster);

  Caption := FCMSPlugIn.GetName + ' - WebsiteEditor - ' + ExtractFileName(WebsiteSettingsFileName);

  result := inherited ShowModal;
end;

destructor TBasisWebsiteEditor.Destroy;
var
  I: Integer;
begin
  FcxBAccept.Free;

  FcxBCancel.Free;

  FcxLCharset.Free;

  FcxCOBCharset.Free;

  FcxGridCustomFieldsTableViewColumn3.Free;
  FcxGridCustomFieldsTableViewColumn2.Free;
  FcxGridCustomFieldsTableViewColumn1.Free;
  FcxGridCustomFieldsTableView.Free;
  FcxGridCustomFieldsLevel.Free;
  FcxGridCustomFields.Free;

  FcxTSCustomFields.Free;

  FcxTCHoster.Free;

  FHosterPanelList.Free;

  FcxTSHoster.Free;

  FcxGridFilterControlsTableViewColumn4.Free;
  FcxGridFilterControlsTableViewColumn3.Free;
  FcxGridFilterControlsTableViewColumn2.Free;
  FcxGridFilterControlsTableViewColumn1.Free;
  FcxGridFilterControlsTableView.Free;
  FcxGridFilterControlsLevel.Free;
  FcxGridFilterControls.Free;
  FcxLFilterControls.Free;
  FcxCCBFilterCategories.Free;
  FcxLFilterCategories.Free;
  FcxCBFilterEnabled.Free;

  FFilterPanel.Free;

  FcxTSFilter.Free;

  FcxTCIDs.Free;

  FIDPanelList.Free;

  FcxTSIDs.Free;

  for I := 0 to length(FEditArray) - 1 do
    with FEditArray[I] do
    begin
      FTextEdit.Free;
      FTitle.Free;
      FBasis.Free;
    end;

  for I := 0 to length(FCheckboxArray) - 1 do
    FCheckboxArray[I].FCheckBox.Free;

  FFlowPanel.Free;

  FcxTSGeneral.Free;

  FcxPCWebsiteSettings.Free;

  FAppController := nil;

  FCMSPlugIn := nil;

  if Assigned(FIDGrabberThread) then
  begin
    FIDGrabberThread.Terminate;
    FIDGrabberThread.WaitFor;
    FIDGrabberThread.Free;
  end;

  inherited Destroy;
end;

{ TBoardWebsiteEditor }

constructor TBoardWebsiteEditor.Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName);
begin
  inherited Create(ACMSPlugIn, AAppController, AWebsiteSettingsFileName);

  {
    with FcxTCIDs.Tabs do
    begin
    Add('forum');
    Add('thread');
    end;
    }
end;

destructor TBoardWebsiteEditor.Destroy;
begin
  inherited Destroy;
end;

{ TBlogWebsiteEditor }

constructor TBlogWebsiteEditor.Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName);
begin
  inherited Create(ACMSPlugIn, AAppController, AWebsiteSettingsFileName);

  // FcxTCIDs.Tabs.Add('category');
end;

destructor TBlogWebsiteEditor.Destroy;
begin

  inherited Destroy;
end;

{ TFormbasedWebsiteEditor }

constructor TFormbasedWebsiteEditor.Create(ACMSPlugIn: ICMSPlugIn; AAppController: IAppController; AWebsiteSettingsFileName: TFileName);
begin
  inherited Create(ACMSPlugIn, AAppController, AWebsiteSettingsFileName);

  {
    FcxTSHosterBlacklist := TcxTabSheet.Create(FcxPCWebsiteSettings);
    with FcxTSHosterBlacklist do
    begin
    PageControl := FcxPCWebsiteSettings;

    Caption := 'Hosterblacklist';
    end;

    FcxLVHosterBlacklist := TcxListView.Create(FcxTSHosterBlacklist);
    with FcxLVHosterBlacklist do
    begin
    Parent := FcxTSHosterBlacklist;

    Left := 3;
    Top := 3;
    Width := FcxTSHosterBlacklist.Width - 6;
    Height := FcxTSHosterBlacklist.Height - 6;

    Anchors := [akLeft, akTop, akRight, akBottom];
    end;
    }
end;

destructor TFormbasedWebsiteEditor.Destroy;
begin
  // FcxLVHosterBlacklist.Free;

  // FcxTSHosterBlacklist.Free;

  inherited Destroy;
end;

class function TWebsiteEditorFactory.GetClassType(ACMSType: TCMSType): TWebsiteEditorMeta;
begin
  case ACMSType of
    cmsBoard:
      result := TBoardWebsiteEditor;
    cmsBlog:
      result := TBlogWebsiteEditor;
    cmsFormbased:
      result := TFormbasedWebsiteEditor;
  else
    raise Exception.Create('Unknown component');
  end;
end;

end.
