unit uApiPublishController;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, Math, Dialogs, Variants,
  // Spring Framework
  Spring.Collections.Lists,
  // MultiEvent
  Generics.MultiEvents.NotifyInterface, Generics.MultiEvents.NotifyHandler,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiFile, uApiIScriptParser, uApiMultiCastEvent, uApiTabSheetData, uApiMirrorControlBase, uApiMirrorControllerBase, uApiSettings,
  // Plugin system
  uPlugInEvent,
  // Utils
  uPathUtils, uStringUtils;

type
  TICMSWebsite = class(TInterfacedObject, ICMSWebsite)
  private
    FAccountName, FAccountPassword, FSettingsFileName, FHostWithPath, FWebsite, FSubject, FTags, FMessage: WideString;
  protected
    function GetAccountName: WideString;
    function GetAccountPassword: WideString;
    function GetSettingsFileName: WideString;
    function GetHostWithPath: WideString;
    function GetWebsite: WideString;
    function GetSubject: WideString;
    function GetTags: WideString;
    function GetMessage: WideString;
  public
    constructor Create(AAccountName, AAccountPassword, ASettingsFileName, AHostWithPath, AWebsite, ASubject, ATags, AMessage: WideString);
    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;

    property SettingsFileName: WideString read GetSettingsFileName;

    property HostWithPath: WideString read GetHostWithPath;
    property Website: WideString read GetWebsite;
    property Subject: WideString read GetSubject;
    property Tags: WideString read GetTags;
    property Message: WideString read GetMessage;
  end;

  TIPublishTab = class(TInterfacedObject, IPublishTab)
  private
    FReleaseName: WideString;
    FPublishItemList: TInterfaceList<IPublishItem>;
  protected
    function GetReleaseName: WideString;
    function GetItem(const IndexOrName: OleVariant): IPublishItem;
  public
    constructor Create(AReleaseName: WideString);
    procedure Add(const APublishItem: IPublishItem);
    property ReleaseName: WideString read GetReleaseName;
    property Item[const IndexOrName: OleVariant]: IPublishItem read GetItem;
    function Count: Integer;
    destructor Destroy; override;
  end;

  TIPublishJob = class(TInterfacedObject, IPublishJob)
  private
    FUniqueID: Longword;
    FDescription: WideString;
    FPublishTabList: TInterfaceList<IPublishTab>;
  protected
    function GetUniqueID: Longword;
    procedure SetUniqueID(AUniqueID: Longword);
    function GetDescription: WideString;
    function GetUpload(const IndexOrName: OleVariant): IPublishTab;
  public
    constructor Create(ADescription: WideString);
    procedure Add(const APublishTab: IPublishTab);
    property UniqueID: Longword read GetUniqueID write SetUniqueID;
    property Description: WideString read GetDescription;
    property Upload[const IndexOrName: OleVariant]: IPublishTab read GetUpload;
    function Count: Integer;
    destructor Destroy; override;
  end;

  TICMSWebsiteContainer = class(TInterfacedObject, ICMSWebsiteContainer)
  strict private
  type
    TPartlyType = (ptControls, ptMirrors);

    TICMSWebsiteContainerActiveController = class
    private
      FTabConnection: ITabSheetController;
      FACMSCollectionItem: TCMSWebsitesCollectionItem;

      FCanUpdatePartly: Boolean;

      FControlsCategories: Boolean;

      FControlsSide: Boolean;
      FHosterSide: Boolean;

      function IsControlValueAllowed(AControl: IControlBasic): Boolean;
      function IsHosterAllowed(AHoster: IMirrorControl): Boolean;

      function IsAllowed: Boolean;
      function AllControlsAllowed: Boolean;
      function HasAtLeastOneHosterAllowed: Boolean;

      property ControlsSide: Boolean read FControlsSide write FControlsSide;
      property HosterSide: Boolean read FHosterSide write FHosterSide;
    public
      constructor Create(const ATabConnection: ITabSheetController; ACMSWebsitesCollectionItem: TCMSWebsitesCollectionItem); reintroduce;
      function Active(APartlyType: TPartlyType): Boolean; overload;
      function Active: Boolean; overload;
      property CanUpdatePartly: Boolean read FCanUpdatePartly;
      destructor Destroy; override;
    end;

  var
    FICMSWebsiteContainerActiveController: TICMSWebsiteContainerActiveController;

  type
    TIPublishItem = class(TICMSWebsite, IPublishItem)
    private
      FCMSPluginPath: WideString;
      FCMSWebsiteData: ITabSheetData;
    protected
      function GetCMSPluginPath: WideString;
      function GetData: ITabSheetData;
    public
      constructor Create(AAccountName, AAccountPassword, ASettingsFileName, AHost, AWebsite, ASubject, ATags, AMessage, ACMSPluginPath: WideString; const ACMSWebsiteData: ITabSheetData);
      property CMSPluginPath: WideString read GetCMSPluginPath;
      property WebsiteData: ITabSheetData read GetData;
      destructor Destroy; override;
    end;

  private
    FTopIndex, FIndex: Integer;
    FTabConnection: ITabSheetController;
    FCMSCollectionItem: TCMSCollectionItem;
    FCMSWebsiteCollectionItem: TCMSWebsitesCollectionItem;
    FActiveChanged: Boolean;
    FActiveBuffer: Boolean;
    FDataChanged: Boolean;
    FDataBuffer: ITabSheetData;
    FControlsPreviousValue, FMirrorPreviousValue: Boolean;
    FIWebsiteChange: TICMSItemChangeEventHandler;
    FIControlChange: TIControlEventHandler;
    FIMirrorChange: TINotifyEventHandler;

    procedure ValidateFile(ARelativeFileName, AFileName, AFileType: string);
    function ValidateFiles: Boolean;
    function LoadFromFile(AFileName: string): string;

    procedure HandleBlackWhitelist(ACMSWebsiteCollectionItem: TCMSWebsitesCollectionItem; out AControlList: TControlDataList; out AMirrorList: TMirrorContainerList);

    procedure WebsiteChange(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
    procedure ControlChange(const Sender: IControlBasic);
    procedure MirrorChange(const Sender: IUnknown);
  protected
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetCMS: WideString;
    function GetCMSInnerIndex: Integer;
    function GetCMSPluginPath: WideString;
    function GetName: WideString;
    function GetTopIndex: Integer;
    procedure SetTopIndex(ATopIndex: Integer);
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetActive: Boolean;
    function GetEnabled: Boolean;
    function GetAccountName: WideString;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString;
    procedure SetAccountPassword(AAccountPassword: WideString);

    function GetSettingsFileName: WideString;

    function GetHostWithPath: WideString;
    function GetWebsite: WideString;

    function GetSubject: WideString;
    function GetSubjectFileName: WideString;
    procedure SetSubjectFileName(ASubjectFileName: WideString);
    function GetTags: WideString;
    function GetMessage: WideString;
    function GetMessageFileName: WideString;
    procedure SetMessageFileName(AMessageFileName: WideString);
  public
    constructor Create(const ATabConnection: ITabSheetController; ACMSCollectionItem: TCMSCollectionItem; ACMSWebsitesCollectionItem: TCMSWebsitesCollectionItem);
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property CMS: WideString read GetCMS;
    property CMSInnerIndex: Integer read GetCMSInnerIndex;
    property Name: WideString read GetName;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property Index: Integer read GetIndex write SetIndex;
    property Active: Boolean read GetActive;
    property Enabled: Boolean read GetEnabled;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function CheckIScript(AIScript: WideString): RIScriptResult;
    function ParseIScript(AIScript: WideString): RIScriptResult;
    function GenerateData: ITabSheetData;

    function GeneratePublishItem: IPublishItem;
    function GeneratePublishTab: IPublishTab;
    function GeneratePublishJob: IPublishJob;

    property SettingsFileName: WideString read GetSettingsFileName;

    property HostWithPath: WideString read GetHostWithPath;
    property Website: WideString read GetWebsite;

    property Subject: WideString read GetSubject;
    property SubjectFileName: WideString read GetSubjectFileName write SetSubjectFileName;
    property Tags: WideString read GetTags;
    property Message: WideString read GetMessage;
    property MessageFileName: WideString read GetMessageFileName write SetMessageFileName;

    destructor Destroy; override;
  end;

  TICMSContainer = class(TInterfacedObject, ICMSContainer)
  private
    FIndex: Integer;
    FTabConnection: ITabSheetController;
    FWebsiteList: TInterfaceList<ICMSWebsiteContainer>;
    FCMSCollectionItem: TCMSCollectionItem;
    FSettingsChangeEventHandler: ICMSItemChangeEventHandler;
    function CreateNewWebsiteContainer(AWebsiteIndex: Integer): ICMSWebsiteContainer;
    procedure SettingsUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer);
    procedure UpdateInternalListItemIndex;
    procedure UpdateCMSWebsiteList;
  protected
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetName: WideString;
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetWebsite(AIndex: Integer): ICMSWebsiteContainer;
  public
    constructor Create(const ATabConnection: ITabSheetController; ACMSCollectionItem: TCMSCollectionItem);
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property Name: WideString read GetName;
    property Index: Integer read GetIndex write SetIndex;
    property Website[index: Integer]: ICMSWebsiteContainer read GetWebsite;
    function Count: Integer;
    destructor Destroy; override;
  end;

  TIPublishController = class(TInterfacedObject, IPublishController)
  private
    FTabSheetController: ITabSheetController;
    FCMSList: TInterfaceList<ICMSContainer>;
    FActive: Boolean;
    FUpdateCMSList: IUpdateCMSListEvent;
    FUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent;
    FUpdateCMSWebsite: IUpdateCMSWebsiteEvent;
    FIChange: TINotifyEventHandler;
    FPluginChangeEventHandler: IPluginChangeEventHandler;
    function CreateNewCMSContainer(ACMSIndex: Integer): ICMSContainer;
    procedure CMSUpdate(ACMSChangeType: TPluginChangeType; AIndex: Integer; AParam: Integer);
    procedure TabChange(const Sender: IUnknown);
    procedure UpdateInternalListItemIndex;
    procedure UpdateCMSList;
    function FindCMSContainer(AName: WideString): Integer;
  protected
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);

    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);

    function GetCMS(const IndexOrName: OleVariant): ICMSContainer;

    function GetUpdateCMSList: IUpdateCMSListEvent;
    function GetUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent;
    function GetUpdateCMSWebsite: IUpdateCMSWebsiteEvent;
  public
    constructor Create(const ATabConnection: ITabSheetController);
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property Active: WordBool read GetActive write SetActive;

    property CMS[const IndexOrName: OleVariant]: ICMSContainer read GetCMS;
    function Count: Integer;

    function GeneratePublishTab: IPublishTab;
    function GeneratePublishJob: IPublishJob;

    property OnUpdateCMSList: IUpdateCMSListEvent read GetUpdateCMSList;
    property OnUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent read GetUpdateCMSWebsiteList;
    property OnUpdateCMSWebsite: IUpdateCMSWebsiteEvent read GetUpdateCMSWebsite;
    destructor Destroy; override;
  end;

implementation

uses
  uMain;

{ TICMSWebsite }

function TICMSWebsite.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

function TICMSWebsite.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

function TICMSWebsite.GetSettingsFileName: WideString;
begin
  Result := FSettingsFileName;
end;

function TICMSWebsite.GetHostWithPath: WideString;
begin
  Result := FHostWithPath;
end;

function TICMSWebsite.GetWebsite: WideString;
begin
  Result := FWebsite;
end;

function TICMSWebsite.GetSubject: WideString;
begin
  Result := FSubject;
end;

function TICMSWebsite.GetTags: WideString;
begin
  Result := FTags;
end;

function TICMSWebsite.GetMessage: WideString;
begin
  Result := FMessage;
end;

constructor TICMSWebsite.Create;
begin
  inherited Create;

  FAccountName := AAccountName;
  FAccountPassword := AAccountPassword;
  FSettingsFileName := ASettingsFileName;
  FHostWithPath := AHostWithPath;
  FWebsite := AWebsite;
  FSubject := ASubject;
  FTags := ATags;
  FMessage := AMessage;
end;

{ TIPublishTab }

function TIPublishTab.GetReleaseName: WideString;
begin
  Result := FReleaseName;
end;

function TIPublishTab.GetItem(const IndexOrName: OleVariant): IPublishItem;

  function Find(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with FPublishItemList do
      for I := 0 to Count - 1 do
        if SameText(AName, Items[I].Website) then
          Exit(I);
  end;

var
  Index: Integer;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    Index := -1;
    if VarIsNumeric(IndexOrName) then
      Index := IndexOrName
    else
      Index := Find(IndexOrName);

    if not((Index < 0) or (Index > FPublishItemList.Count)) then
      Result := FPublishItemList.Items[Index];
  end;
end;

constructor TIPublishTab.Create(AReleaseName: WideString);
begin
  inherited Create;
  FReleaseName := AReleaseName;
  FPublishItemList := TInterfaceList<IPublishItem>.Create;
end;

procedure TIPublishTab.Add(const APublishItem: IPublishItem);
begin
  if Assigned(APublishItem) then
    FPublishItemList.Add(APublishItem);
end;

function TIPublishTab.Count: Integer;
begin
  Result := FPublishItemList.Count;
end;

destructor TIPublishTab.Destroy;
begin
  FPublishItemList.Free;
  inherited Destroy;
end;

{ TIPublishJob }

function TIPublishJob.GetUniqueID: Longword;
begin
  Result := FUniqueID;
end;

procedure TIPublishJob.SetUniqueID(AUniqueID: Longword);
begin
  FUniqueID := AUniqueID;
end;

function TIPublishJob.GetDescription: WideString;
begin
  Result := FDescription;
end;

function TIPublishJob.GetUpload(const IndexOrName: OleVariant): IPublishTab;

  function Find(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with FPublishTabList do
      for I := 0 to Count - 1 do
        if SameText(AName, Items[I].ReleaseName) then
          Exit(I);
  end;

var
  Index: Integer;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    Index := -1;
    if VarIsNumeric(IndexOrName) then
      Index := IndexOrName
    else
      Index := Find(IndexOrName);

    if not((Index < 0) or (Index > FPublishTabList.Count)) then
      Result := FPublishTabList.Items[Index];
  end;
end;

constructor TIPublishJob.Create;
begin
  inherited Create;
  FUniqueID := 0;
  FDescription := ADescription;
  FPublishTabList := TInterfaceList<IPublishTab>.Create;
end;

procedure TIPublishJob.Add(const APublishTab: IPublishTab);
begin
  FPublishTabList.Add(APublishTab);
end;

function TIPublishJob.Count: Integer;
begin
  Result := FPublishTabList.Count;
end;

destructor TIPublishJob.Destroy;
begin
  FPublishTabList.Free;
  inherited Destroy; ;
end;

{ TICMSWebsiteContainer.TICMSWebsiteContainerActiveController }

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.IsAllowed: Boolean;
begin
  Result := True;

  with FACMSCollectionItem.Filter do
    if Active then
    begin
      if not CanUpdatePartly then
        FControlsCategories := FTabConnection.ControlController.TypeID in FACMSCollectionItem.Filter.GetCategoriesAsTTypeIDs;
      Result := FControlsCategories;
    end;
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.IsControlValueAllowed(AControl: IControlBasic): Boolean;

  function RelToBool(ARel: string): Boolean;
  begin
    Result := (ARel = '=');
  end;

var
  I: Integer;
  LCategory: string;
  LIsCategory: Boolean;
  Allowed: Boolean;
begin
  Result := True;
  for I := 0 to FACMSCollectionItem.Filter.Controls.Count - 1 do
  begin
    LCategory := FACMSCollectionItem.Filter.Controls.Items[I].Category;
    LIsCategory := StringInTypeID(LCategory);
    if (not LIsCategory or ((LIsCategory) and (AControl.TypeID = StringToTypeID(LCategory)))) and
    { . } (AControl.ControlID = StringToControlID(FACMSCollectionItem.Filter.Controls.Items[I].Name)) then
    begin
      Allowed := RelToBool(FACMSCollectionItem.Filter.Controls.Items[I].Relation) = MatchTextMask(FACMSCollectionItem.Filter.Controls.Items[I].Value, AControl.Value);

      if not Allowed then
        Exit(False);
    end;
  end;
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.IsHosterAllowed(AHoster: IMirrorControl): Boolean;
var
  I: Integer;
begin
  Result := False or (FACMSCollectionItem.Filter.Hosters.Count = 0);

  for I := 0 to FACMSCollectionItem.Filter.Hosters.Count - 1 do
    if GetHosterNameType(FACMSCollectionItem.Filter.Hosters.Items[I].Name) = htFile then
      if SameStr('', AHoster.Hoster) or (FACMSCollectionItem.Filter.Hosters.Items[I].Blacklist.IndexOf(AHoster.Hoster) = -1) then
        Exit(True);
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.AllControlsAllowed: Boolean;
var
  I: Integer;
  Allowed: Boolean;
begin
  Result := True;
  if FACMSCollectionItem.Filter.Active then
    for I := 0 to FTabConnection.ControlController.ControlCount - 1 do
    begin
      Allowed := IsControlValueAllowed(FTabConnection.ControlController.Control[I]);
      if not Allowed then
        Exit(False);
    end;
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.HasAtLeastOneHosterAllowed: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FTabConnection.MirrorController.MirrorCount - 1 do
    if IsHosterAllowed(FTabConnection.MirrorController.Mirror[I]) then
      Exit(True);
end;

constructor TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.Create;
begin
  FTabConnection := ATabConnection;
  FACMSCollectionItem := ACMSWebsitesCollectionItem;

  FCanUpdatePartly := False;

  ControlsSide := True;
  HosterSide := True;
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.Active(APartlyType: TPartlyType): Boolean;
begin
  case APartlyType of
    ptControls:
      ControlsSide := IsAllowed and AllControlsAllowed;
    ptMirrors:
      HosterSide := HasAtLeastOneHosterAllowed;
  end;
  Result := ControlsSide and HosterSide;
end;

function TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.Active: Boolean;
begin
  if not CanUpdatePartly then
  begin
    Active(ptControls);
    Active(ptMirrors);

    FCanUpdatePartly := True;
  end;

  Result := ControlsSide and HosterSide;
end;

destructor TICMSWebsiteContainer.TICMSWebsiteContainerActiveController.Destroy;
begin
  FACMSCollectionItem := nil;
  FTabConnection := nil;
  inherited Destroy;
end;

{ TICMSWebsiteContainer.TIPublishItem }

function TICMSWebsiteContainer.TIPublishItem.GetCMSPluginPath: WideString;
begin
  Result := FCMSPluginPath;
end;

constructor TICMSWebsiteContainer.TIPublishItem.Create;
begin
  inherited Create(AAccountName, AAccountPassword, ASettingsFileName, AHost, AWebsite, ASubject, ATags, AMessage);
  FCMSPluginPath := ACMSPluginPath;
  FCMSWebsiteData := ACMSWebsiteData;
end;

function TICMSWebsiteContainer.TIPublishItem.GetData: ITabSheetData;
begin
  Result := FCMSWebsiteData;
end;

destructor TICMSWebsiteContainer.TIPublishItem.Destroy;
begin
  FCMSWebsiteData := nil;
  inherited Destroy;
end;

{ TICMSWebsiteContainer }

procedure TICMSWebsiteContainer.ValidateFile(ARelativeFileName, AFileName, AFileType: string);
begin
  if SameStr('', ARelativeFileName) then
    raise Exception.Create('You have to define a ' + AFileType + ' file for ' + Name + ' [' + CMS + '] inside CMS/website settings');
  if not FileExists(AFileName) then
    raise Exception.Create('The defined ' + AFileType + ' file for ' + Name + ' [' + CMS + '] was not found (relative path: ' + ARelativeFileName + ')' + sLineBreak + sLineBreak + 'Full path: ' + AFileName);
end;

function TICMSWebsiteContainer.ValidateFiles: Boolean;
begin
  Result := True;
  try
    ValidateFile(FCMSWebsiteCollectionItem.SubjectFileName, SubjectFileName, 'Subject');
    ValidateFile(FCMSWebsiteCollectionItem.MessageFileName, MessageFileName, 'Message');
  except
    on E: Exception do
    begin
      Result := False;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

function TICMSWebsiteContainer.LoadFromFile(AFileName: string): string;
begin
  with TStringStream.Create do
    try
      LoadFromFile(AFileName);
      Result := DataString;
    finally
      Free;
    end;
end;

procedure TICMSWebsiteContainer.HandleBlackWhitelist(ACMSWebsiteCollectionItem: TCMSWebsitesCollectionItem; out AControlList: TControlDataList; out AMirrorList: TMirrorContainerList);
var
  LHostersIntex, LWhitelistIndex, LControlIndex: Integer;
  LHosterType: THosterType;
  LBlackList, LWhitelist: TStringList;

  LControlBasic: IControlBasic;
  LPicture: IPicture;

  LHasMirror: Boolean;
  LHasPicture: Boolean;
begin
  AControlList := TControlDataList.Create;
  AMirrorList := TMirrorContainerList.Create;

  LControlBasic := TabSheetController.ControlController.FindControl(cPicture);
  if Assigned(LControlBasic) then
  begin
    LControlBasic.QueryInterface(IPicture, LPicture);
  end;

  LHasMirror := False;
  LHasPicture := False;

  // Handle if hoster ranking/whitelist or blacklist is defined
  for LHostersIntex := 0 to ACMSWebsiteCollectionItem.Filter.Hosters.Count - 1 do
  begin
    LHosterType := GetHosterNameType(ACMSWebsiteCollectionItem.Filter.Hosters.Items[LHostersIntex].Name);

    LBlackList := TStringList.Create;
    LWhitelist := TStringList.Create;
    try
      // Load black- and whitelist
      LBlackList.Text := ACMSWebsiteCollectionItem.Filter.Hosters.Items[LHostersIntex].Blacklist.Text;
      if ACMSWebsiteCollectionItem.Filter.Hosters.Items[LHostersIntex].Ranked then
        LWhitelist.Text := ACMSWebsiteCollectionItem.Filter.Hosters.Items[LHostersIntex].Whitelist.Text;

      // Reduce black- and whitelist to used hosters
      case LHosterType of
        htFile:
          begin

            for LWhitelistIndex := LWhitelist.Count - 1 downto 0 do
              if not Assigned(TabSheetController.MirrorController.Mirror[LWhitelist.Strings[LWhitelistIndex]]) then
              begin
                LWhitelist.Delete(LWhitelistIndex);
              end;
            for LWhitelistIndex := LBlackList.Count - 1 downto 0 do
              if not Assigned(TabSheetController.MirrorController.Mirror[LBlackList.Strings[LWhitelistIndex]]) then
              begin
                LBlackList.Delete(LWhitelistIndex);
              end;

          end;
        htImage:
          begin

            if Assigned(LPicture) then
            begin
              for LWhitelistIndex := LWhitelist.Count - 1 downto 0 do
                if not Assigned(LPicture.Mirror[LWhitelist.Strings[LWhitelistIndex]]) then
                begin
                  LWhitelist.Delete(LWhitelistIndex);
                end;
              for LWhitelistIndex := LBlackList.Count - 1 downto 0 do
                if not Assigned(LPicture.Mirror[LBlackList.Strings[LWhitelistIndex]]) then
                begin
                  LBlackList.Delete(LWhitelistIndex);
                end;
            end;

          end;
      end;

      // Make the ordering and filter for blacklist
      for LWhitelistIndex := 0 to LWhitelist.Count - 1 do
      begin
        // If not in blacklist
        if (LBlackList.IndexOf(LWhitelist.Strings[LWhitelistIndex]) = -1) then
        begin
          LBlackList.Add(LWhitelist.Strings[LWhitelistIndex]); // Add to blacklist, to not let add again into mirror list

          case LHosterType of
            htFile:
              begin

                for LControlIndex := 0 to TabSheetController.MirrorController.MirrorCount - 1 do
                  if SameText(LWhitelist.Strings[LWhitelistIndex], TabSheetController.MirrorController.Mirror[LControlIndex].Hoster) then
                  begin
                    AMirrorList.Add(TabSheetController.MirrorController.Mirror[LControlIndex].CloneInstance());
                    Break;
                  end;

              end;
            htImage:
              begin

                if Assigned(LPicture) then
                begin
                  for LControlIndex := 0 to LPicture.MirrorCount - 1 do
                    if SameText(LWhitelist.Strings[LWhitelistIndex], LPicture.Mirror[LControlIndex].Name) and not SameStr('', LPicture.Mirror[LControlIndex].Value) then
                    begin
                      AControlList.Add(LPicture.Mirror[LControlIndex].CloneInstance());
                      LHasPicture := True;
                      Break;
                    end
                    else if SameText(LWhitelist.Strings[LWhitelistIndex], 'OriginalValue') and not SameStr('', LPicture.Value) then
                    begin
                      AControlList.Add(LPicture.CloneInstance());
                      LHasPicture := True;
                      Break;
                    end;
                end;

                Break; // only one picture needed, break whitelist loop
              end;
          end;
        end;
      end;

      case LHosterType of
        htFile:
          begin

            for LControlIndex := 0 to TabSheetController.MirrorController.MirrorCount - 1 do
              if (LBlackList.IndexOf(TabSheetController.MirrorController.Mirror[LControlIndex].Hoster) = -1) then
              begin
                AMirrorList.Add(TabSheetController.MirrorController.Mirror[LControlIndex].CloneInstance());
              end;

            LHasMirror := True;

          end;
        htImage:
          begin

            if Assigned(LPicture) and not LHasPicture then
            begin
              for LControlIndex := 0 to LPicture.MirrorCount - 1 do
                if (LBlackList.IndexOf(LPicture.Mirror[LControlIndex].Name) = -1) and not SameStr('', LPicture.Mirror[LControlIndex].Value) then
                begin
                  AControlList.Add(LPicture.Mirror[LControlIndex].CloneInstance());
                  LHasPicture := True;
                  Break;
                end;

              if not LHasPicture then
              begin
                AControlList.Add(LPicture.CloneInstance());
                LHasPicture := True;
              end;
            end;

          end;
      end;

    finally
      LWhitelist.Free;
      LBlackList.Free;
    end;
  end;

  // Handle if nothing is defined i.e. "filters"-tag not specified
  if not LHasMirror then
  begin
    for LControlIndex := 0 to TabSheetController.MirrorController.MirrorCount - 1 do
      AMirrorList.Add(TabSheetController.MirrorController.Mirror[LControlIndex].CloneInstance());
  end;
  if Assigned(LPicture) and not LHasPicture then
  begin
    for LControlIndex := 0 to LPicture.MirrorCount - 1 do
      if not SameStr('', LPicture.Mirror[LControlIndex].Value) then
      begin
        AControlList.Add(LPicture.Mirror[LControlIndex].CloneInstance());
        LHasPicture := True;
        Break;
      end;

    if not LHasPicture then
      AControlList.Add(LPicture.CloneInstance());
  end;

  LPicture := nil;
  LControlBasic := nil;
end;

procedure TICMSWebsiteContainer.WebsiteChange(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  LNewValue: Boolean;
begin
  FActiveChanged := True;
  FDataChanged := True;

  with FICMSWebsiteContainerActiveController do
  begin
    FICMSWebsiteContainerActiveController.FCanUpdatePartly := False;
    LNewValue := FICMSWebsiteContainerActiveController.Active;
    if not(LNewValue = FActiveBuffer) then
    begin
      FActiveBuffer := LNewValue;
      FTabConnection.PublishController.OnUpdateCMSWebsite.Invoke(TopIndex, Index, LNewValue);
    end;
    FActiveChanged := False;
  end;
end;

procedure TICMSWebsiteContainer.ControlChange(const Sender: IControlBasic);
var
  LNewValue: Boolean;
begin
  FActiveChanged := True;
  FDataChanged := True;

  with FICMSWebsiteContainerActiveController do
    if CanUpdatePartly then
    begin
      LNewValue := FICMSWebsiteContainerActiveController.Active(ptControls);
      if not(LNewValue = FControlsPreviousValue) then
      begin
        FControlsPreviousValue := LNewValue;
        FTabConnection.PublishController.OnUpdateCMSWebsite.Invoke(TopIndex, Index, LNewValue);
      end;
    end;
end;

procedure TICMSWebsiteContainer.MirrorChange(const Sender: IInterface);
var
  LNewValue: Boolean;
begin
  FActiveChanged := True;
  FDataChanged := True;

  with FICMSWebsiteContainerActiveController do
    if CanUpdatePartly then
    begin
      LNewValue := FICMSWebsiteContainerActiveController.Active(ptMirrors);
      if not(LNewValue = FMirrorPreviousValue) then
      begin
        FMirrorPreviousValue := LNewValue;
        FTabConnection.PublishController.OnUpdateCMSWebsite.Invoke(TopIndex, Index, LNewValue);
      end;
    end;
end;

function TICMSWebsiteContainer.GetTabSheetController: ITabSheetController;
begin
  Result := FTabConnection;
end;

procedure TICMSWebsiteContainer.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabConnection := ATabSheetController;
  FICMSWebsiteContainerActiveController.FTabConnection := ATabSheetController;
end;

function TICMSWebsiteContainer.GetCMS: WideString;
begin
  Result := FCMSCollectionItem.Name;
end;

function TICMSWebsiteContainer.GetCMSInnerIndex: Integer;
begin
  Result := FCMSCollectionItem.Index;
end;

function TICMSWebsiteContainer.GetCMSPluginPath: WideString;
begin
  Result := FCMSCollectionItem.GetPath;
end;

function TICMSWebsiteContainer.GetName: WideString;
begin
  Result := FCMSWebsiteCollectionItem.Name;
end;

function TICMSWebsiteContainer.GetTopIndex: Integer;
begin
  Result := FTopIndex;
end;

procedure TICMSWebsiteContainer.SetTopIndex(ATopIndex: Integer);
begin
  FTopIndex := ATopIndex;
end;

function TICMSWebsiteContainer.GetIndex: Integer;
begin
  Result := FIndex;
end;

procedure TICMSWebsiteContainer.SetIndex(AIndex: Integer);
begin
  FIndex := AIndex;
end;

function TICMSWebsiteContainer.GetActive: Boolean;
begin
  if FActiveChanged then
  begin
    FActiveBuffer := FICMSWebsiteContainerActiveController.Active;
    FActiveChanged := False;
  end;

  Result := FActiveBuffer;
end;

function TICMSWebsiteContainer.GetEnabled: Boolean;
begin
  Result := FCMSWebsiteCollectionItem.Enabled;
end;

function TICMSWebsiteContainer.GetAccountName: WideString;
begin
  Result := FCMSWebsiteCollectionItem.AccountName;
end;

procedure TICMSWebsiteContainer.SetAccountName(AAccountName: WideString);
begin
  FCMSWebsiteCollectionItem.AccountName := AAccountName;
end;

function TICMSWebsiteContainer.GetAccountPassword: WideString;
begin
  Result := FCMSWebsiteCollectionItem.AccountPassword;
end;

procedure TICMSWebsiteContainer.SetAccountPassword(AAccountPassword: WideString);
begin
  FCMSWebsiteCollectionItem.AccountPassword := AAccountPassword;
end;

function TICMSWebsiteContainer.GetSettingsFileName: WideString;
begin
  Result := FCMSWebsiteCollectionItem.GetPath;
end;

function TICMSWebsiteContainer.GetHostWithPath: WideString;
begin
  Result := FCMSWebsiteCollectionItem.HostWithPath;
end;

function TICMSWebsiteContainer.GetWebsite: WideString;
begin
  Result := FCMSWebsiteCollectionItem.Website;
end;

function TICMSWebsiteContainer.GetSubject: WideString;
begin
  Result := ParseIScript(LoadFromFile(SubjectFileName)).CompiledText;
end;

function TICMSWebsiteContainer.GetSubjectFileName: WideString;
begin
  Result := FCMSWebsiteCollectionItem.GetSubjectFileName;
end;

procedure TICMSWebsiteContainer.SetSubjectFileName(ASubjectFileName: WideString);
begin
  FCMSWebsiteCollectionItem.SubjectFileName := ExtractRelativePath(GetTemplatesCMSFolder, ASubjectFileName);
end;

function TICMSWebsiteContainer.GetTags: WideString;
begin
  Result := '';
  if Assigned(FTabConnection.ControlController.FindControl(cTags)) then
    Result := FTabConnection.ControlController.FindControl(cTags).Value;
end;

function TICMSWebsiteContainer.GetMessage: WideString;
begin
  Result := ParseIScript(LoadFromFile(MessageFileName)).CompiledText;
end;

function TICMSWebsiteContainer.GetMessageFileName: WideString;
begin
  Result := FCMSWebsiteCollectionItem.GetMessageFileName;
end;

procedure TICMSWebsiteContainer.SetMessageFileName(AMessageFileName: WideString);
begin
  FCMSWebsiteCollectionItem.MessageFileName := ExtractRelativePath(GetTemplatesCMSFolder, AMessageFileName);
end;

constructor TICMSWebsiteContainer.Create;
begin
  FICMSWebsiteContainerActiveController := TICMSWebsiteContainerActiveController.Create(ATabConnection, ACMSWebsitesCollectionItem);

  FTopIndex := -1;
  FIndex := -1;
  FTabConnection := ATabConnection;
  FCMSCollectionItem := ACMSCollectionItem;
  FCMSWebsiteCollectionItem := ACMSWebsitesCollectionItem;

  FActiveChanged := True; // Need to be true to make first update.
  FActiveBuffer := False;

  FDataChanged := True; // Need to be true to make first update.
  FDataBuffer := nil;

  FControlsPreviousValue := False;
  FMirrorPreviousValue := False;

  FIWebsiteChange := TICMSItemChangeEventHandler.Create(WebsiteChange);
  FCMSCollectionItem.OnWebsitesChange.Add(FIWebsiteChange);

  FIControlChange := TIControlEventHandler.Create(ControlChange);
  FTabConnection.ControlController.OnControlChange.Add(FIControlChange);

  FIMirrorChange := TINotifyEventHandler.Create(MirrorChange);
  FTabConnection.MirrorController.OnChange.Add(FIMirrorChange);
end;

function TICMSWebsiteContainer.CheckIScript(AIScript: WideString): RIScriptResult;
begin
  with TIScirptParser.Create(CMS, Website, GenerateData) do
    try
      Result := ErrorAnalysis(AIScript);
    finally
      Free;
    end;
end;

function TICMSWebsiteContainer.ParseIScript(AIScript: WideString): RIScriptResult;
begin
  with TIScirptParser.Create(CMS, Website, GenerateData) do
    try
      Result := Execute(AIScript);
    finally
      Free;
    end;
end;

function TICMSWebsiteContainer.GenerateData: ITabSheetData;
var
  LControlList: TControlDataList;
  LMirrorList: TMirrorContainerList;

  LControlIndex: Integer;
begin
  if FDataChanged then
  begin
    HandleBlackWhitelist(FCMSWebsiteCollectionItem, LControlList, LMirrorList);

    for LControlIndex := 0 to TabSheetController.ControlController.ControlCount - 1 do
    begin
      if not(TabSheetController.ControlController.Control[LControlIndex].ControlID = cPicture) then
        LControlList.Add(TabSheetController.ControlController.Control[LControlIndex]);
    end;

    FDataBuffer := TITabSheetData.Create(TabSheetController.TypeID, LControlList, LMirrorList);
    FDataChanged := False;
  end;

  Result := FDataBuffer;
end;

function TICMSWebsiteContainer.GeneratePublishItem: IPublishItem;
begin
  Result := nil;
  if ValidateFiles then
    Result := TIPublishItem.Create(AccountName, AccountPassword, SettingsFileName, HostWithPath, Website, Subject, Tags, Message, GetCMSPluginPath, GenerateData);
end;

function TICMSWebsiteContainer.GeneratePublishTab: IPublishTab;
var
  PublishTab: TIPublishTab;
begin
  PublishTab := TIPublishTab.Create(TabSheetController.ReleaseName);

  PublishTab.Add(GeneratePublishItem);

  Result := PublishTab;
end;

function TICMSWebsiteContainer.GeneratePublishJob: IPublishJob;
var
  PublishJob: TIPublishJob;
begin
  PublishJob := TIPublishJob.Create(TabSheetController.ReleaseNameShort + ' @ ' + Website);

  PublishJob.Add(GeneratePublishTab);

  Result := PublishJob;
end;

destructor TICMSWebsiteContainer.Destroy;
begin
  FCMSCollectionItem.OnWebsitesChange.Remove(FIWebsiteChange);
  FTabConnection.MirrorController.OnChange.Remove(FIMirrorChange);
  FTabConnection.ControlController.OnControlChange.Remove(FIControlChange);
  FDataBuffer := nil;
  FIMirrorChange := nil;
  FIControlChange := nil;
  FIWebsiteChange := nil;
  FTabConnection := nil;
  FCMSCollectionItem := nil;
  FCMSWebsiteCollectionItem := nil;
  FICMSWebsiteContainerActiveController.Free;
  inherited Destroy;
end;

{ TICMSContainer }

function TICMSContainer.CreateNewWebsiteContainer(AWebsiteIndex: Integer): ICMSWebsiteContainer;
begin
  Result := TICMSWebsiteContainer.Create(FTabConnection, FCMSCollectionItem, TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AWebsiteIndex]));
end;

procedure TICMSContainer.SettingsUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer);

  function FindCMSWebsiteItem(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to Count - 1 do
      if SameText(AName, Website[I].Name) then
        Exit(I);
  end;

  function FindNextEnabledCMSWebsiteItem(AStartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with FCMSCollectionItem.Websites do
      for I := AStartIndex to Count - 1 do
        if TCMSWebsitesCollectionItem(Items[I]).Enabled then
          Exit(I);
  end;

  function CMSWebsiteItemToInternalIndex(AIndex: Integer): Integer;
  begin
    if (AIndex = -1) then
      Exit(-1);
    Result := FindCMSWebsiteItem(TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AIndex]).name);
  end;

var
  Index, CMSWebsiteIndex: Integer;
  CMSWebsiteName: string;
begin
  with TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AIndex]) do
  begin
    CMSWebsiteIndex := Index;
    CMSWebsiteName := Name;
  end;

  case ACMSItemChangeType of
    cctAdd:
      ;
    cctDelete:
      begin
        Index := FindCMSWebsiteItem(CMSWebsiteName);
        if not(Index = -1) then
          FWebsiteList.Delete(Index);
      end;
    cctEnabled:
      begin
        if AParam = 0 then
        begin
          Index := FindCMSWebsiteItem(CMSWebsiteName);
          if not(Index = -1) then
            FWebsiteList.Delete(Index);
        end
        else
        begin
          Index := FindCMSWebsiteItem(CMSWebsiteName);
          if (Index = -1) then
          begin
            Index := CMSWebsiteItemToInternalIndex(FindNextEnabledCMSWebsiteItem(CMSWebsiteIndex + 1));
            if (Index = -1) then
              FWebsiteList.Add(CreateNewWebsiteContainer(CMSWebsiteIndex))
            else
              FWebsiteList.Insert(Index, CreateNewWebsiteContainer(CMSWebsiteIndex));
          end;
        end;
      end;
  end;

  UpdateInternalListItemIndex;
  UpdateCMSWebsiteList;
end;

procedure TICMSContainer.UpdateInternalListItemIndex;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Website[I] do
    begin
      TopIndex := Self.Index;
      Index := I;
    end;
end;

procedure TICMSContainer.UpdateCMSWebsiteList;
begin
  if TabSheetController.IsTabActive then
    FTabConnection.PublishController.OnUpdateCMSWebsiteList.Invoke(Self, Index);
end;

function TICMSContainer.GetTabSheetController: ITabSheetController;
begin
  Result := FTabConnection;
end;

procedure TICMSContainer.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabConnection := ATabSheetController;
end;

function TICMSContainer.GetName: WideString;
begin
  Result := FCMSCollectionItem.Name;
end;

function TICMSContainer.GetIndex: Integer;
begin
  Result := FIndex;
end;

procedure TICMSContainer.SetIndex(AIndex: Integer);
begin
  FIndex := AIndex;
  UpdateInternalListItemIndex;
end;

function TICMSContainer.GetWebsite(AIndex: Integer): ICMSWebsiteContainer;
begin
  Result := FWebsiteList[AIndex];
end;

constructor TICMSContainer.Create(const ATabConnection: ITabSheetController; ACMSCollectionItem: TCMSCollectionItem);
var
  I: Integer;
begin
  FIndex := -1;
  FTabConnection := ATabConnection;
  FWebsiteList := TInterfaceList<ICMSWebsiteContainer>.Create;
  FCMSCollectionItem := ACMSCollectionItem;

  with FCMSCollectionItem do
  begin
    for I := 0 to Websites.Count - 1 do
      if TCMSWebsitesCollectionItem(Websites.Items[I]).Enabled then
        FWebsiteList.Add(CreateNewWebsiteContainer(I));
    UpdateInternalListItemIndex;
    FSettingsChangeEventHandler := TICMSItemChangeEventHandler.Create(SettingsUpdate);
    OnSettingsChange.Add(FSettingsChangeEventHandler);
  end;
end;

function TICMSContainer.Count: Integer;
begin
  Result := FWebsiteList.Count;
end;

destructor TICMSContainer.Destroy;
begin
  FCMSCollectionItem.OnSettingsChange.Remove(FSettingsChangeEventHandler);
  FCMSCollectionItem := nil;
  FWebsiteList.Free;
  FTabConnection := nil;
  inherited;
end;

{ TIPublishController }

function TIPublishController.CreateNewCMSContainer(ACMSIndex: Integer): ICMSContainer;
begin
  with SettingsManager.Settings.Plugins do
    Result := TICMSContainer.Create(TabSheetController, TCMSCollectionItem(CMS.Items[ACMSIndex]));
end;

procedure TIPublishController.CMSUpdate(ACMSChangeType: TPluginChangeType; AIndex: Integer; AParam: Integer);

  function FindPrevEnabledCMSItem(AEndIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with SettingsManager.Settings.Plugins.CMS do
      for I := Min(AEndIndex, Count) - 1 downto 0 do
        with TCMSCollectionItem(Items[I]) do
          if Enabled then
            Exit(I);
  end;

  function FindNextEnabledCMSItem(AStartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with SettingsManager.Settings.Plugins.CMS do
      for I := AStartIndex to Count - 1 do
        if TCMSCollectionItem(Items[I]).Enabled then
          Exit(I);
  end;

  function CMSItemToInternalIndex(AIndex: Integer): Integer;
  begin
    if (AIndex = -1) then
      Exit(-1);
    Result := FindCMSContainer(TCMSCollectionItem(SettingsManager.Settings.Plugins.CMS.Items[AIndex]).name);
  end;

var
  Index, Position, CMSIndex: Integer;
  CMSName: string;
  buf: ICMSContainer;
begin
  with TCMSCollectionItem(SettingsManager.Settings.Plugins.CMS.Items[AIndex]) do
  begin
    CMSIndex := Index;
    CMSName := Name;
  end;

  case ACMSChangeType of
    pctAdd:
      ; // nothing
    pctMove:
      begin
        Index := FindCMSContainer(CMSName);

        if not(Index = -1) then
        begin
          buf := CMS[Index];
          FCMSList.Delete(Index);

          Position := FindPrevEnabledCMSItem(CMSIndex);
          if (Position = -1) then
            FCMSList.Insert(0, buf)
          else
          begin
            Position := CMSItemToInternalIndex(Position);
            FCMSList.Insert(Position + 1, buf);
          end;
        end;
      end;
    pctDelete:
      begin
        Index := FindCMSContainer(CMSName);
        if not(Index = -1) then
          FCMSList.Delete(Index);
      end;
    pctEnabled:
      begin
        if AParam = 0 then
        begin
          Index := FindCMSContainer(CMSName);
          if not(Index = -1) then
            FCMSList.Delete(Index);
        end
        else
        begin
          Index := FindCMSContainer(CMSName);
          if (Index = -1) then
          begin
            Index := CMSItemToInternalIndex(FindNextEnabledCMSItem(CMSIndex + 1));
            if (Index = -1) then
              FCMSList.Add(CreateNewCMSContainer(CMSIndex))
            else
              FCMSList.Insert(Index, CreateNewCMSContainer(CMSIndex));
          end;
        end;
      end;
  end;

  UpdateInternalListItemIndex;
  UpdateCMSList;
end;

procedure TIPublishController.TabChange(const Sender: IInterface);
begin
  UpdateCMSList;
end;

procedure TIPublishController.UpdateInternalListItemIndex;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    CMS[I].Index := I;
end;

procedure TIPublishController.UpdateCMSList;
begin
  if TabSheetController.IsTabActive then
    OnUpdateCMSList.Invoke(Self);
end;

function TIPublishController.FindCMSContainer(AName: WideString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SameText(AName, CMS[I].Name) then
      Exit(I);
end;

constructor TIPublishController.Create;
begin
  FTabSheetController := ATabConnection;

  FCMSList := TInterfaceList<ICMSContainer>.Create;

  FUpdateCMSList := TIUpdateCMSListEvent.Create;
  FUpdateCMSWebsiteList := TIUpdateCMSWebsiteListEvent.Create;
  FUpdateCMSWebsite := TIUpdateCMSWebsiteEvent.Create;

  FIChange := TINotifyEventHandler.Create(TabChange);
  FPluginChangeEventHandler := TIPluginChangeEventHandler.Create(CMSUpdate);
end;

function TIPublishController.GetTabSheetController: ITabSheetController;
begin
  Result := FTabSheetController;
end;

procedure TIPublishController.SetTabSheetController(const ATabSheetController: ITabSheetController);
begin
  FTabSheetController := ATabSheetController;
end;

function TIPublishController.GetActive: WordBool;
begin
  Result := FActive;
end;

procedure TIPublishController.SetActive(AActive: WordBool);
var
  LCMSIndex: Integer;
begin
  if not(Active = AActive) then
  begin
    case AActive of
      True:
        begin
          with SettingsManager.Settings.Plugins do
          begin
            for LCMSIndex := 0 to CMS.Count - 1 do
              if TCMSCollectionItem(CMS.Items[LCMSIndex]).Enabled then
                FCMSList.Add(CreateNewCMSContainer(LCMSIndex));
            UpdateInternalListItemIndex;

            OnCMSChange.Add(FPluginChangeEventHandler);
          end;

          TabSheetController.PageController.OnChange.Add(FIChange);
        end;
      False:
        begin
          TabSheetController.PageController.OnChange.Remove(FIChange);

          with SettingsManager.Settings.Plugins do
            OnCMSChange.Remove(FPluginChangeEventHandler);
        end;
    end;
    FActive := AActive;
  end;
end;

function TIPublishController.GetCMS(const IndexOrName: OleVariant): ICMSContainer;
var
  Index: Integer;
begin
  Result := nil;

  if not VarIsNull(IndexOrName) then
  begin
    Index := -1;
    if VarIsNumeric(IndexOrName) then
      Index := IndexOrName
    else
      Index := FindCMSContainer(IndexOrName);

    if not((Index < 0) or (Index > FCMSList.Count)) then
      Result := FCMSList[Index];
  end;
end;

function TIPublishController.GetUpdateCMSList: IUpdateCMSListEvent;
begin
  Result := FUpdateCMSList;
end;

function TIPublishController.GetUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent;
begin
  Result := FUpdateCMSWebsiteList;
end;

function TIPublishController.GetUpdateCMSWebsite: IUpdateCMSWebsiteEvent;
begin
  Result := FUpdateCMSWebsite;
end;

function TIPublishController.Count: Integer;
begin
  Result := FCMSList.Count;
end;

function TIPublishController.GeneratePublishTab: IPublishTab;
var
  PublishTab: TIPublishTab;

  I, J: Integer;
begin
  PublishTab := TIPublishTab.Create(TabSheetController.ReleaseName);

  for I := 0 to Count - 1 do
    for J := 0 to CMS[I].Count - 1 do
      with CMS[I].Website[J] do
        if Active then
          PublishTab.Add(GeneratePublishItem);

  Result := PublishTab;
end;

function TIPublishController.GeneratePublishJob: IPublishJob;
var
  LPublishJob: TIPublishJob;
begin
  LPublishJob := TIPublishJob.Create('All active for ' + TabSheetController.ReleaseNameShort);

  LPublishJob.Add(GeneratePublishTab);

  Result := LPublishJob;
end;

destructor TIPublishController.Destroy;
begin
  // TODO: test this
  UpdateCMSList;

  FPluginChangeEventHandler := nil;
  FIChange := nil;

  FUpdateCMSWebsite := nil;
  FUpdateCMSWebsiteList := nil;
  FUpdateCMSList := nil;
  FCMSList.Free;

  FTabSheetController := nil;

  inherited Destroy;
end;

end.
