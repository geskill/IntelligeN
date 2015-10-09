unit uAppInterface;

interface

uses
  // MultiEvent
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Plugin
  uPlugInConst;

type
  IMenuItem = interface;
  IMenuItems = interface;
  IDirectlinksPanel = interface;
  IMirrorControl = interface;
  IMirrorController = interface;
  IPicture = interface;
  IControlController = interface;
  IPublishController = interface;
  IPublishItem = interface;
  IPublishTab = interface;
  IPublishJob = interface;
  ITabSheetController = interface;
  IPageController = interface;

  IViewChangeEventHandler = interface(IUnknown)
    ['{5129F9C3-B9B5-4FE5-88E1-DB7284D55D93}']
    procedure Invoke(const ANewViewType: TTabViewType); safecall;
  end;

  IViewChangeEvent = interface(IUnknown)
    ['{4638F6A6-C775-4411-84A0-ED9277CB86D0}']
    procedure Add(const AHandler: IViewChangeEventHandler); safecall;
    procedure Remove(const AHandler: IViewChangeEventHandler); safecall;
    procedure Invoke(const ANewViewType: TTabViewType); safecall;
  end;

  IPopupMenuChange = interface(IUnknown)
    ['{F672ACC7-3930-4112-8BC7-578A4E291C9D}']
    procedure Invoke(const ASender: Integer); safecall;
  end;

  IMenuItem = interface(IUnknown)
    ['{EB29098A-A769-4E1D-91A0-F728A1C7C92D}']
    function GetMenuItems: IMenuItems; safecall;
    function InsertMenuItem(AIndex: Integer; const ACaption: WideString; const AHint: WideString; AShortCut: Integer; AImageIndex: Integer; ATag: Integer; const AOnClick: INotifyEventHandler): IMenuItem; safecall;
    function GetIndex: Integer; safecall;
    function GetName: WideString; safecall;
    function GetCaption: WideString; safecall;
    procedure SetCaption(const AValue: WideString); safecall;
    function GetHint: WideString; safecall;
    procedure SetHint(const AValue: WideString); safecall;
    function GetShortCut: Integer; safecall;
    procedure SetShortCut(AValue: Integer); safecall;
    function GetImageIndex: Integer; safecall;
    procedure SetImageIndex(AImageIndex: Integer); safecall;
    function GetTag: Integer; safecall;
    procedure SetTag(AValue: Integer); safecall;
    function GetOnClick: INotifyEventHandler; safecall;
    procedure SetOnClick(const AValue: INotifyEventHandler); safecall;
  end;

  IMenuItems = interface(IUnknown)
    ['{9A35E2C0-792E-4372-9483-D779C92B0B07}']
    function GetCount: Integer; safecall;
    function GetItem(AIndex: Integer): IMenuItem; safecall;
    function RemoveItem(const AMenuItem: IMenuItem): WordBool; safecall;
  end;

  IMainMenu = interface(IUnknown)
    ['{B8435927-2FE9-4487-9955-4138579288D8}']
    function GetMenuItems: IMenuItems; safecall;
    function InsertMenuItem(AIndex: Integer; const ACaption: WideString; const AHint: WideString; AShortCut: Integer; AImageIndex: Integer; ATag: Integer; const AOnClick: INotifyEventHandler; const ASubMenuItem: WordBool = True): IMenuItem; safecall;
  end;

  IDirectlinksMirror = interface(IDirectlink)
    ['{617FFD5F-82B1-443C-B535-0A7413069F76}']
    function GetDirectlinksPanel: IDirectlinksPanel;
    procedure SetDirectlinksPanel(ADirectlinksPanel: IDirectlinksPanel);

    function GetLinksInfo: TLinksInfo;
    procedure SetLinksInfo(ALinksInfo: TLinksInfo);

    function GetTitle: WideString;
    procedure SetTitle(ATitle: WideString);

    procedure SetValue(AValue: WideString);

    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    procedure UpdateGUI;
    procedure Mody;
    procedure CheckStatus;

    function GetPartName(AFileName: WideString): WideString;

    property DirectlinksPanel: IDirectlinksPanel read GetDirectlinksPanel write SetDirectlinksPanel;

    property LinksInfo: TLinksInfo read GetLinksInfo write SetLinksInfo;

    property Title: WideString read GetTitle write SetTitle;
    property Value: WideString read GetValue write SetValue;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IDirectlinksPanel = interface(IDirectlinkContainer)
    ['{DBFC2ED7-FFB2-4611-9F69-05CD827F3A7A}']
    // Base
    function GetDirectlink(const Index: Integer): IDirectlinksMirror; safecall;

    // Additional
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);

    function GetActiveDirectlinkIndex: Integer;
    function GetActiveDirectlink: IDirectlinksMirror;

    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    // Base
    property Directlink[const Index: Integer]: IDirectlinksMirror read GetDirectlink;

    // Additional
    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;

    function Add(ALinks: WideString = ''): Integer;
    procedure Remove(ATabIndex: Integer);

    property ActiveMirrorIndex: Integer read GetActiveDirectlinkIndex;
    property ActiveMirror: IDirectlinksMirror read GetActiveDirectlink;

    property FileName: WideString read GetFileName;

    property Visible: Boolean read GetVisible write SetVisible;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  ICrypterPanel = interface(ICrypter)
    ['{C29FF83C-BE8D-4E2A-93F7-660D749948FF}']
    // Additional
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);

    procedure SetValue(AValue: WideString);

    function GetStatus: Byte;

    function GetCrypterFolderInfo: TCrypterFolderInfo;
    procedure SetCrypterFolderInfo(ACrypterFolderInfo: TCrypterFolderInfo);

    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);

    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    procedure UpdateGUI;

    // Base
    property Value: WideString read GetValue write SetValue;

    // Additional
    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;

    property Status: Byte read GetStatus;

    property CrypterFolderInfo: TCrypterFolderInfo read GetCrypterFolderInfo write SetCrypterFolderInfo;

    procedure CreateFolder;
    procedure CheckFolder(const AUseCheckDelay: Boolean = False);

    property Visible: Boolean read GetVisible write SetVisible;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IMirrorControl = interface(IMirrorContainer)
    ['{9E4D7459-1D7B-4E8E-9177-8365D631E5F5}']
    // Base
    function GetDirectlink(const Index: Integer): IDirectlinksMirror; overload; safecall;

    function GetCrypter(const IndexOrName: OleVariant): ICrypterPanel; safecall;

    // Additional
    function GetMirrorController: IMirrorController;
    procedure SetMirrorController(const AMirrorController: IMirrorController);

    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);

    function GetTabIndex: Integer;
    procedure SetTabIndex(ATabIndex: Integer);

    function GetDirectlink: IDirectlinksPanel; overload;

    function GetLeft: Integer;
    procedure SetLeft(ALeft: Integer);
    function GetTop: Integer;
    procedure SetTop(ATop: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AWidth: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AHeight: Integer);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    // Base
    property Directlink[const Index: Integer]: IDirectlinksMirror read GetDirectlink;

    function FindCrypter(const AName: WideString): ICrypterPanel; safecall;

    property Crypter[const IndexOrName: OleVariant]: ICrypterPanel read GetCrypter;

    // Additional
    property MirrorController: IMirrorController read GetMirrorController write SetMirrorController;
    property Index: Integer read GetIndex write SetIndex;
    property TabIndex: Integer read GetTabIndex write SetTabIndex;

    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Focus: Boolean read GetFocus write SetFocus;

    function AddCrypter(AName: WideString): Integer;
    function RemoveCrypter(AIndex: Integer): Boolean;
  end;

  IMirrorController = interface(IMirrorControllerBase)
    ['{230C0B4E-F11B-4DE9-B105-EED8D92B92CA}']
    // Base
    function GetMirror(const IndexOrName: OleVariant): IMirrorControl; safecall;

    // Additional
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);

    // Events
    function GetSpaceMouseDown: INotifyEventHandler;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
    function GetChange: INotifyEvent;
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);

    // Base
    function FindMirror(const AHoster: WideString): IMirrorControl; safecall;

    property Mirror[const IndexOrName: OleVariant]: IMirrorControl read GetMirror; default;

    // Additional
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;

    function IndexOf(const Item: IMirrorControl): Integer;
    function Add: Integer;
    procedure Insert(index: Integer; const Item: IMirrorControl); overload;
    function Insert(index: Integer): IMirrorControl; overload;
    function Remove(index: Integer): Boolean;

    // Cloning
    function CloneInstance(): IMirrorControllerBase;

    // Events
    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown; { only for internal usage }
    property OnChange: INotifyEvent read GetChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange; { only for internal usage }
  end;

  IControlBasic = interface(IControlBase)
    ['{DE8F253F-D695-41D4-A350-3CF191644466}']
    function GetControlController: IControlController;
    procedure SetControlController(const AControlController: IControlController);
    function GetTypeID: TTypeID;
    function GetControlID: TControlID;
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetTitle: WideString;
    procedure SetTitle(ATitle: WideString);
    function GetLeft: Integer;
    procedure SetLeft(ALeft: Integer);
    function GetTop: Integer;
    procedure SetTop(ATop: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AWidth: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AHeight: Integer);
    function GetHint: WideString;
    procedure SetHint(AHint: WideString);
    procedure SetValue(AValue: WideString);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    property ControlController: IControlController read GetControlController write SetControlController;

    property TypeID: TTypeID read GetTypeID;
    property ControlID: TControlID read GetControlID;
    property name: WideString read GetName write SetName;
    property Title: WideString read GetTitle write SetTitle;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: WideString read GetHint write SetHint;
    property Value: WideString read GetValue write SetValue;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IEdit = interface(IControlBasic)
    ['{3AB76C7D-3EB8-499F-9258-BE97296A7ECB}']
  end;

  IComboBox = interface(IControlBasic)
    ['{C8536847-A684-44B8-9CA0-428459E157D6}']
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    function GetList: WideString;
    procedure SetList(AList: WideString);

    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
  end;

  IComboBoxList = interface(IComboBox)
    ['{06F8C809-6FF0-4CC8-B057-F42802759581}']
  end;

  ICheckComboBox = interface(IControlBasic)
    ['{5F2E9174-0AB9-4936-94E1-7021F79EB4BC}']
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    function GetList: WideString;
    procedure SetList(AList: WideString);

    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
  end;

  IDateEdit = interface(IControlBasic)
    ['{7441FAFF-6C09-43D0-96C7-4E1FED109D83}']
  end;

  IRichEdit = interface(IControlBasic)
    ['{C4878DD0-57BF-4A32-809A-9F7AC7B6CC5E}']
  end;

  IPictureMirror = interface
    ['{5FBF0A22-0BD2-4B9C-A094-F4AD990A2605}']
    function GetPicture: IPicture;
    procedure SetPicture(APicture: IPicture);
    function GetName: WideString;
    function GetOriginalValue: WideString;
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
    function GetErrorMsg: WideString;
    procedure SetErrorMsg(AErrorMsg: WideString);

    property Picture: IPicture read GetPicture write SetPicture;
    property Name: WideString read GetName;
    property OriginalValue: WideString read GetOriginalValue;
    property Value: WideString read GetValue write SetValue;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;

    procedure LocalUpload(ALocalPath: WideString);
    procedure RemoteUpload;
  end;

  IPicture = interface(IComboBox)
    ['{F686511D-EC7D-4FB1-AD88-121511434F93}']
    function GetMirror(AIndex: Integer): IPictureMirror;
    procedure SetMirror(AIndex: Integer; APictureMirror: IPictureMirror);
    function GetMirrorCount: Integer;
    function AddMirror(AName: WideString): Integer;
    function RemoveMirror(AIndex: Integer): Boolean;

    property Mirror[AIndex: Integer]: IPictureMirror read GetMirror write SetMirror;
    property MirrorCount: Integer read GetMirrorCount;

    procedure RemoteUpload(const AAfterCrawling: WordBool = False);
  end;
{$IFDEF MAINAPP}

  IPictureEx = interface(IPicture)
    ['{F686511D-EC7D-4FB1-AD88-121511434F93}']
    function GetValuePicture(AIndex: Integer): TPictureInfo;
    procedure SetValuePicture(AIndex: Integer; APictureInfo: TPictureInfo);
  end;
{$ENDIF}

  ITrailer = interface(IComboBox)
    ['{7D38C9A3-16BD-4550-A383-61C5A4E04A4D}']
    procedure AddValue(AValue: WideString; ATitle: WideString; ASender: WideString); overload;
    function GetValueTitle(AIndex: Integer): WideString;
  end;

  IControlEventHandler = interface(IUnknown)
    ['{207E1DBF-69A1-4E05-9CEA-BE5BA4091EF9}']
    procedure Invoke(const Sender: IControlBasic); safecall;
  end;

  IReleaseNameChange = interface(IUnknown)
    ['{783B9FD1-BCE5-478E-A61C-CB68A717C8DF}']
    procedure Invoke(const AReleaseName: WideString); safecall;
  end;

  IControlChangeEvent = interface(IUnknown)
    ['{2713C4FB-6F32-4862-8C64-3EFC9F804B53}']
    procedure Add(const AHandler: IControlEventHandler); safecall;
    procedure Remove(const AHandler: IControlEventHandler); safecall;
    procedure Invoke(const ASender: IControlBasic); safecall;
  end;

  IControlController = interface(IControlControllerBase)
    ['{E9432D30-D4CA-4045-BEA3-55C02E56243A}']
    // Base
    function GetControl(const IndexOrName: OleVariant): IControlBasic; safecall;

    // Additional
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetTypeID: TTypeID;
    procedure SetTypeID(ATypeID: TTypeID);

    // Events
    function GetSpaceMouseDown: INotifyEventHandler;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEventHandler);
    function GetControlChange: IControlChangeEvent;
    function GetControlEnter: IControlEventHandler;
    procedure SetControlEnter(AControlEnter: IControlEventHandler);
    function GetControlExit: IControlEventHandler;
    procedure SetControlExit(AControlExit: IControlEventHandler);
    function GetReleaseNameChange: IReleaseNameChange;
    procedure SetReleaseNameChange(AReleaseNameChange: IReleaseNameChange);
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);

    // Base
    function FindControl(const AControlID: TControlID): IControlBasic; safecall;

    property Control[const IndexOrName: OleVariant]: IControlBasic read GetControl; default;

    // Additional
    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property TypeID: TTypeID read GetTypeID write SetTypeID;

    procedure NewControl(AType: TControlID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer);

    // Cloning
    function CloneInstance(): IControlControllerBase;

    // Events
    property OnSpaceMouseDown: INotifyEventHandler read GetSpaceMouseDown write SetSpaceMouseDown; { only for internal usage }
    property OnControlChange: IControlChangeEvent read GetControlChange;
    property OnControlEnter: IControlEventHandler read GetControlEnter write SetControlEnter; { only for internal usage }
    property OnControlExit: IControlEventHandler read GetControlExit write SetControlExit; { only for internal usage }
    property OnReleaseNameChange: IReleaseNameChange read GetReleaseNameChange write SetReleaseNameChange; { only for internal usage }
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange; { only for internal usage }
  end;

  ICMSWebsite = interface
    ['{6E5EEC82-3E30-41AF-99B1-D99140A170E1}']
    function GetAccountName: WideString;
    function GetAccountPassword: WideString;
    function GetSettingsFileName: WideString;
    function GetWebsite: WideString;
    function GetSubject: WideString;
    function GetTags: WideString;
    function GetMessage: WideString;

    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;

    property SettingsFileName: WideString read GetSettingsFileName;

    property Website: WideString read GetWebsite;
    property Subject: WideString read GetSubject;
    property Tags: WideString read GetTags;
    property Message: WideString read GetMessage;
  end;

  ICMSWebsiteContainer = interface(ICMSWebsite)
    ['{A78282FB-748E-453C-BD2A-5DFE847DE8D6}']
    function GetTabSheetController: ITabSheetController;
    // procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetCMS: WideString;
    function GetName: WideString;
    function GetTopIndex: Integer;
    procedure SetTopIndex(ATopIndex: Integer);
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetActive: Boolean;

    function GetSubjectFileName: WideString;
    procedure SetSubjectFileName(ASubjectFileName: WideString);

    function GetMessageFileName: WideString;
    procedure SetMessageFileName(AMessageFileName: WideString);

    property TabSheetController: ITabSheetController read GetTabSheetController; // write SetTabSheetController;
    property CMS: WideString read GetCMS;
    property Name: WideString read GetName;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property Index: Integer read GetIndex write SetIndex;
    property Active: Boolean read GetActive;

    function CheckIScript(AIScript: WideString): RIScriptResult;
    function ParseIScript(AIScript: WideString): RIScriptResult;
    function GenerateData: ITabSheetData;

    function GeneratePublishItem: IPublishItem;
    function GeneratePublishTab: IPublishTab;
    function GeneratePublishJob: IPublishJob;

    property SubjectFileName: WideString read GetSubjectFileName write SetSubjectFileName;
    property MessageFileName: WideString read GetMessageFileName write SetMessageFileName;
  end;

  ICMSContainer = interface
    ['{80C890DA-8445-46DD-BDEA-3F10DE5D7AAA}']
    // function GetTabSheetController: ITabSheetController;
    // procedure SetTabSheetController(const ATabSheetController: ITabSheetController);

    function GetName: WideString;
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetWebsite(AIndex: Integer): ICMSWebsiteContainer;

    // property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    property Name: WideString read GetName;
    property Index: Integer read GetIndex write SetIndex;
    property Website[index: Integer]: ICMSWebsiteContainer read GetWebsite;
    function Count: Integer;
  end;

  IUpdateCMSListEventHandler = interface(IUnknown)
    ['{65FF1964-E1A9-4580-A228-028F51DE7EA1}']
    procedure Invoke(const Sender: IPublishController); safecall;
  end;

  IUpdateCMSListEvent = interface(IUnknown)
    ['{2C937211-6106-4BC0-B9D0-BBE243428C16}']
    procedure Add(const AHandler: IUpdateCMSListEventHandler); safecall;
    procedure Remove(const AHandler: IUpdateCMSListEventHandler); safecall;
    procedure Invoke(const ASender: IPublishController); safecall;
  end;

  IUpdateCMSWebsiteListEventHandler = interface(IUnknown)
    ['{9DD93BC1-2DDB-4529-B2DD-B8F90F0CD25E}']
    procedure Invoke(const Sender: ICMSContainer; CMSIndex: Integer); safecall;
  end;

  IUpdateCMSWebsiteListEvent = interface(IUnknown)
    ['{0F3CC77B-A54B-43CD-89DC-F5017D1F8B3A}']
    procedure Add(const AHandler: IUpdateCMSWebsiteListEventHandler); safecall;
    procedure Remove(const AHandler: IUpdateCMSWebsiteListEventHandler); safecall;
    procedure Invoke(const ASender: ICMSContainer; ACMSIndex: Integer); safecall;
  end;

  IUpdateCMSWebsiteEventHandler = interface(IUnknown)
    ['{6391C77C-B4A1-46DC-AAE3-C028E9ED3DC5}']
    procedure Invoke(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool); safecall;
  end;

  IUpdateCMSWebsiteEvent = interface(IUnknown)
    ['{7D9BC317-E628-4294-B8FB-C4DA61C3ADB4}']
    procedure Add(const AHandler: IUpdateCMSWebsiteEventHandler); safecall;
    procedure Remove(const AHandler: IUpdateCMSWebsiteEventHandler); safecall;
    procedure Invoke(ACMSIndex, AWebsiteIndex: Integer; ANewStatus: WordBool); safecall;
  end;

  IPublishController = interface
    ['{D7D1CA65-ED2D-4854-AEE6-E6E3D9DD801F}']
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);

    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);

    function GetCMS(const IndexOrName: OleVariant): ICMSContainer;

    function GetUpdateCMSList: IUpdateCMSListEvent;
    function GetUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent;
    function GetUpdateCMSWebsite: IUpdateCMSWebsiteEvent;

    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;

    property Active: WordBool read GetActive write SetActive;

    property CMS[const IndexOrName: OleVariant]: ICMSContainer read GetCMS;
    function Count: Integer;

    function GeneratePublishTab: IPublishTab;
    function GeneratePublishJob: IPublishJob;

    property OnUpdateCMSList: IUpdateCMSListEvent read GetUpdateCMSList;
    property OnUpdateCMSWebsiteList: IUpdateCMSWebsiteListEvent read GetUpdateCMSWebsiteList;
    property OnUpdateCMSWebsite: IUpdateCMSWebsiteEvent read GetUpdateCMSWebsite;
  end;

  IIScriptController = interface
    ['{CC20CAD1-9847-45D2-ABE4-272B2C659F4F}']
    function GetDataChanged: WordBool;

    function GetData: WideString;

    property Data: WideString read GetData;

    procedure InsertText(AText: WideString);
  end;

  ITabSheetController = interface
    ['{1973A687-DD39-472A-8B07-D541CFA08780}']
    function GetPageController: IPageController;
    function GetIsTabActive: WordBool;
    function GetTabSheetIndex: Integer;
    function GetViewType: TTabViewType;
    procedure SetViewType(AViewType: TTabViewType);
    function GetFileName: WideString;
    function GetFileType: WideString;
    function GetReleaseName: WideString;
    procedure SetReleaseName(AReleaseName: WideString);
    function GetTypeID: TTypeID;
    function GetActiveWebsite: WideString;
    procedure SetActiveWebsite(AWebsite: WideString);

    function GetControlController: IControlController;
    function GetMirrorController: IMirrorController;
    function GetPublishController: IPublishController;

    property PageController: IPageController read GetPageController;

    property IsTabActive: WordBool read GetIsTabActive;
    property TabSheetIndex: Integer read GetTabSheetIndex;

    property ViewType: TTabViewType read GetViewType write SetViewType;

    property FileName: WideString read GetFileName;
    property FileType: WideString read GetFileType;

    property ReleaseName: WideString read GetReleaseName write SetReleaseName;

    procedure Save(AFileName, AFileType: WideString);
    procedure ResetDataChanged(AFileName, AFileType: WideString);

    property TypeID: TTypeID read GetTypeID;

    property ActiveWebsite: WideString read GetActiveWebsite write SetActiveWebsite;

    property ControlController: IControlController read GetControlController;
    property MirrorController: IMirrorController read GetMirrorController;
    property PublishController: IPublishController read GetPublishController;
  end;

  ICrawlerManager = interface
    ['{EC5351B9-7BF9-4317-88AB-DDAD6D9B8D8C}']
    function IsIdle: WordBool;

    procedure AddCrawlerJob(const AControlController: IControlController);
    procedure RemoveCrawlerJob(const AControlController: IControlController);
  end;

  IHosterManager = interface
    ['{03675561-B598-4929-B5C9-FBEF9FB52656}']
    function IsIdle: WordBool;

    procedure AddHosterCheckJob(const ADirectlink: IDirectlink);
    procedure RemoveHosterCheckJob(const ADirectlink: IDirectlink);
  end;

  ICrypterManager = interface
    ['{3ABA9E7F-6D3D-4F35-94E8-45ED37228710}']
    procedure AddCrypterJob(const ACrypterPanel: ICrypterPanel);
    procedure AddCrypterCheckJob(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: WordBool = False);
  end;

  IPublishItem = interface(ICMSWebsite)
    ['{5AE2BAB5-6B50-40E7-85B7-C01D060C83DF}']
    function GetCMSPluginPath: WideString;
    function GetData: ITabSheetData;

    property CMSPluginPath: WideString read GetCMSPluginPath;
    property Data: ITabSheetData read GetData;
  end;

  IPublishTab = interface
    ['{71E8F700-FE93-4D6C-AF3F-7D08168848AC}']
    function GetReleaseName: WideString;
    function GetItem(const IndexOrName: OleVariant): IPublishItem;

    property ReleaseName: WideString read GetReleaseName;
    property Item[const IndexOrName: OleVariant]: IPublishItem read GetItem;
    function Count: Integer;
  end;

  IPublishJob = interface
    ['{D874E548-AFD7-4EDE-B1D9-C865CE5F2EF1}']
    function GetUniqueID: Longword;
    procedure SetUniqueID(AUniqueID: Longword);
    function GetDescription: WideString;
    function GetUpload(const IndexOrName: OleVariant): IPublishTab;

    property UniqueID: Longword read GetUniqueID write SetUniqueID;
    property Description: WideString read GetDescription;
    property Upload[const IndexOrName: OleVariant]: IPublishTab read GetUpload;
    function Count: Integer;
  end;

  IPublishManager = interface
    ['{D49455B7-1B06-4884-9A40-EF2245FD3A7C}']
    function IsIdle: WordBool;

    function AddPublishJob(const APublishJob: IPublishJob): Longword;
    procedure RemovePublishJob(const APublishJob: IPublishJob); overload;
    procedure RemovePublishJob(const AUniqueID: Longword); overload;
    procedure RemoveAllJobs;

    procedure Resume;
    procedure Pause;
  end;

  IImageHosterManager = interface
    ['{CC554556-2480-49A7-90B8-A214E6619F25}']
    procedure AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
    procedure AddRemoteUploadJob(const APictureMirror: IPictureMirror);
  end;

  IPageController = interface
    ['{735F0449-3C70-4701-BBC3-9FE3F229DD96}']
    function GetCrawlerManager: ICrawlerManager;
    function GetHosterManager: IHosterManager;
    function GetCrypterManager: ICrypterManager;
    function GetPublishManager: IPublishManager;
    function GetImageHosterManager: IImageHosterManager;
    function GetActiveTabSheetIndex: Integer;
    function GetActiveTabSheetController: ITabSheetController;
    function GetTabSheetController(index: Integer): ITabSheetController;
    function GetChange: INotifyEvent;
    function GetViewChange: IViewChangeEvent;

    procedure CallAutoCompletion;
    procedure CallSeriesAutoCompletion;
    procedure CallCrypterCrypt;
    procedure CallSeriesCrypterCrypt;
    procedure CallCrypterCheck;
    procedure CallSeriesCrypterCheck;
    procedure CallPublish;
    procedure CallSeriesPublish;

    procedure CallControlAligner;

    procedure OpenToNewTab(AFileName: WideString = '');

    function Add(AFileName: WideString; ATypeID: TTypeID; AEmpty: WordBool = False): Integer;
    property CrawlerManager: ICrawlerManager read GetCrawlerManager;
    property HosterManager: IHosterManager read GetHosterManager;
    property CrypterManager: ICrypterManager read GetCrypterManager;
    property PublishManager: IPublishManager read GetPublishManager;
    property ImageHosterManager: IImageHosterManager read GetImageHosterManager;
    property ActiveTabSheetIndex: Integer read GetActiveTabSheetIndex;
    property ActiveTabSheetController: ITabSheetController read GetActiveTabSheetController;
    property TabSheetController[index: Integer]: ITabSheetController read GetTabSheetController;
    function TabSheetCount: Integer;

    property OnChange: INotifyEvent read GetChange;
    property OnViewChange: IViewChangeEvent read GetViewChange;
  end;

  IAppController = interface
    ['{80AF47FD-847E-429D-922C-0F3B812AB637}']
    function GetMainMenu: IMainMenu;
    function GetPageController: IPageController;

    function GetFileHosters: WideString;
    function GetImageHosters: WideString;
    function GetCustomisedHoster(const AHoster: WideString; AShortName: WordBool = False): WideString;
    function GetControlValues(const ATypeID: TTypeID; const AControlID: TControlID): WideString;

    property MainMenu: IMainMenu read GetMainMenu;
    property PageController: IPageController read GetPageController;
  end;

implementation

end.
