unit uAppInterface;

interface

uses
  // Common
  uConst,
  // Plugin
  uPlugInConst;

type
  IMenuItem = interface;
  IMenuItems = interface;
  IMirrorControl = interface;
  IMirrorController = interface;
  IComponentController = interface;
  ITabSheetController = interface;
  IPageController = interface;

  INotifyEvent = interface(IUnknown)
    ['{EE9407DD-2337-4DFC-BC35-A40C4FA0A1A7}']
    procedure OnNotify(const Sender: IUnknown); stdcall;
  end;

  IPopupMenuChange = interface(IUnknown)
    ['{F672ACC7-3930-4112-8BC7-578A4E291C9D}']
    procedure OnNotify(const Sender: Integer); stdcall;
  end;

  IMenuItem = interface(IUnknown)
    ['{EB29098A-A769-4E1D-91A0-F728A1C7C92D}']
    function GetMenuItems: IMenuItems; stdcall;
    function InsertMenuItem(AIndex: Integer; const aCaption: WideString; const AHint: WideString; aShortCut: Word; aImageIndex: Integer;
      aTag: Integer; const aOnClick: INotifyEvent): IMenuItem; stdcall;
    function GetIndex: Integer; stdcall;
    function GetName: WideString; stdcall;
    function GetCaption: WideString; stdcall;
    procedure SetCaption(const AValue: WideString); stdcall;
    function GetHint: WideString; stdcall;
    procedure SetHint(const AValue: WideString); stdcall;
    function GetShortCut: Word; stdcall;
    procedure SetShortCut(AValue: Word); stdcall;
    function GetImageIndex: Integer; stdcall;
    procedure SetImageIndex(aImageIndex: Integer); stdcall;
    function GetTag: Integer; stdcall;
    procedure SetTag(AValue: Integer); stdcall;
    function GetOnClick: INotifyEvent; stdcall;
    procedure SetOnClick(const AValue: INotifyEvent); stdcall;
  end;

  IMenuItems = interface(IUnknown)
    ['{9A35E2C0-792E-4372-9483-D779C92B0B07}']
    function GetCount: Integer; stdcall;
    function GetItem(index: Integer): IMenuItem; stdcall;
    function RemoveItem(const aMenuItem: IMenuItem): WordBool; stdcall;
  end;

  IMainMenu = interface(IUnknown)
    ['{B8435927-2FE9-4487-9955-4138579288D8}']
    function GetMenuItems: IMenuItems; stdcall;
    function InsertMenuItem(AIndex: Integer; const aCaption: WideString; const AHint: WideString; aShortCut: Word; aImageIndex: Integer;
      aTag: Integer; const aOnClick: INotifyEvent; const ASubMenuItem: Boolean = True): IMenuItem; stdcall;
  end;

  IWebsiteEditor = interface
    ['{75F63C46-C88E-48C5-BDC1-C7D81BEFEC1A}']

    procedure AddEdit(AName: WideString; ADefaultValue: WideString = ''; ATopValue: Boolean = False);
    procedure AddCheckbox(AName: WideString; ADefaultValue: Boolean = False; ATopValue: Boolean = False);
    procedure AddCategoryTab(AName: WideString);

    function ShowModal: Integer;
  end;

  IDirectlinksMirror = interface
    ['{617FFD5F-82B1-443C-B535-0A7413069F76}']
    function GetSize: Extended;
    procedure SetSize(ASize: Extended);
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;

    function GetLinksInfo: TLinksInfo;
    procedure SetLinksInfo(ALinksInfo: TLinksInfo);
    function GetTitle: WideString;
    procedure SetTitle(ATitle: WideString);
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    procedure Mody;
    procedure RefreshInfo;
    procedure CheckStatus;

    function GetFileName: WideString;
    function GetPartName(AFileName: WideString): WideString;

    property Size: Extended read GetSize write SetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property Parts: Integer read GetParts;

    property LinksInfo: TLinksInfo read GetLinksInfo write SetLinksInfo;

    property Title: WideString read GetTitle write SetTitle;
    property Value: WideString read GetValue write SetValue;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IDirectlinksPanel = interface
    ['{DBFC2ED7-FFB2-4611-9F69-05CD827F3A7A}']
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);
    function GetFileName: WideString;
    function GetActiveMirrorIndex: Integer;
    function GetActiveMirror: IDirectlinksMirror;
    function GetMirror(index: Integer): IDirectlinksMirror;
    function GetMirrorCount: Integer;
    function GetSize: Extended; overload;
    function GetSize(index: Integer): Extended; overload;
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;
    function Add(ALinks: WideString = ''): Integer;
    procedure Remove(ATabIndex: Integer);
    property FileName: WideString read GetFileName;
    property ActiveMirrorIndex: Integer read GetActiveMirrorIndex;
    property ActiveMirror: IDirectlinksMirror read GetActiveMirror;
    property Mirror[index: Integer]: IDirectlinksMirror read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
    property Size: Extended read GetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property Parts: Integer read GetParts;
    property Visible: Boolean read GetVisible write SetVisible;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  ICrypterPanel = interface
    ['{C29FF83C-BE8D-4E2A-93F7-660D749948FF}']
    function GetMirrorControl: IMirrorControl;
    procedure SetMirrorControl(AMirrorControl: IMirrorControl);
    function GetName: WideString;
    function GetLink: WideString;
    procedure SetLink(ALink: WideString);
    function GetStatus: Byte;
    function GetSize: Extended;
    function GetHoster: WideString;
    function GetHosterShort: WideString;
    function GetParts: Integer;
    function GetStatusImage: WideString;
    function GetStatusImageText: WideString;
    function GetCrypterFolderInfo: TCrypterFolderInfo;
    procedure SetCrypterFolderInfo(ACrypterFolderInfo: TCrypterFolderInfo);
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    property MirrorControl: IMirrorControl read GetMirrorControl write SetMirrorControl;
    property name: WideString read GetName;
    property Link: WideString read GetLink write SetLink;
    property Status: Byte read GetStatus;
    property Size: Extended read GetSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
    property CrypterFolderInfo: TCrypterFolderInfo read GetCrypterFolderInfo write SetCrypterFolderInfo;
    procedure RefreshGrid;
    procedure CreateFolder;
    procedure CheckFolder(const AUseCheckDelay: Boolean = False);
    property Visible: Boolean read GetVisible write SetVisible;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IMirrorControl = interface
    ['{9E4D7459-1D7B-4E8E-9177-8365D631E5F5}']
    function GetMirrorController: IMirrorController;
    procedure SetMirrorController(const AMirrorController: IMirrorController);
    function GetIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function GetTabIndex: Integer;
    procedure SetTabIndex(ATabIndex: Integer);
    function GetSize: Extended;
    function GetPartSize: Extended;
    function GetHoster: WideString; overload;
    function GetHoster(AShortName: Boolean): WideString; overload;
    function GetParts: Integer;
    function GetDirectlink: IDirectlinksPanel;
    function GetDirectlinksMirror(index: Integer): WideString;
    procedure SetDirectlinksMirror(index: Integer; ADirectlinks: WideString);
    function GetDirectlinksMirrorCount: Integer;
    function GetCrypter(index: Integer): ICrypterPanel;
    function GetCrypterCount: Integer;
    function AddCrypter(AName: WideString): Integer;
    function RemoveCrypter(AIndex: Integer): Boolean;

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
    property MirrorController: IMirrorController read GetMirrorController write SetMirrorController;
    property index: Integer read GetIndex write SetIndex;
    property TabIndex: Integer read GetTabIndex write SetTabIndex;
    property Size: Extended read GetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property Parts: Integer read GetParts;
    property Directlink: IDirectlinksPanel read GetDirectlink;
    property DirectlinksMirror[index: Integer]: WideString read GetDirectlinksMirror write SetDirectlinksMirror;
    property DirectlinksMirrorCount: Integer read GetDirectlinksMirrorCount;
    property Crypter[index: Integer]: ICrypterPanel read GetCrypter;
    property CrypterCount: Integer read GetCrypterCount;

    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Focus: Boolean read GetFocus write SetFocus;
  end;

  IMirrorController = interface
    ['{230C0B4E-F11B-4DE9-B105-EED8D92B92CA}']
    function GetTabSheetController: ITabSheetController;
    procedure SetTabSheetController(const ATabSheetController: ITabSheetController);
    function GetMirror(index: Integer): IMirrorControl;
    function GetMirrorCount: Integer;

    function GetSpaceMouseDown: INotifyEvent;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEvent);
    function GetChange: INotifyEvent;
    procedure SetChange(AChange: INotifyEvent);
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);

    property TabSheetController: ITabSheetController read GetTabSheetController write SetTabSheetController;
    function IndexOf(const Item: IMirrorControl): Integer;
    function Add: Integer;
    procedure Insert(index: Integer; const Item: IMirrorControl); overload;
    function Insert(index: Integer): IMirrorControl; overload;
    function Remove(index: Integer): Boolean;
    property Mirror[index: Integer]: IMirrorControl read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;

    property OnSpaceMouseDown: INotifyEvent read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnChange: INotifyEvent read GetChange write SetChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;
  end;

  IBasic = interface
    ['{DE8F253F-D695-41D4-A350-3CF191644466}']
    function GetComponentController: IComponentController;
    procedure SetComponentController(const AComponentController: IComponentController);
    function GetTemplateTypeID: TTemplateTypeID;
    function GetComponentID: TComponentID;
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
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);
    function GetFocus: Boolean;
    procedure SetFocus(AFocus: Boolean);

    property ComponentController: IComponentController read GetComponentController write SetComponentController;

    procedure AddValue(AValue: WideString; ASender: WideString); overload;
    function GetValueName(AIndex: Integer): WideString;
    function GetValueContent(AIndex: Integer): WideString;
    function GetValueCount: Integer;

    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID;
    property ComponentID: TComponentID read GetComponentID;
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

  IEdit = interface(IBasic)
    ['{3AB76C7D-3EB8-499F-9258-BE97296A7ECB}']
  end;

  IComboBox = interface(IBasic)
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

  ICheckComboBox = interface(IBasic)
    ['{5F2E9174-0AB9-4936-94E1-7021F79EB4BC}']
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(ADropDownRows: Integer);
    function GetList: WideString;
    procedure SetList(AList: WideString);

    property List: WideString read GetList write SetList;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows;
  end;

  IDateEdit = interface(IBasic)
    ['{7441FAFF-6C09-43D0-96C7-4E1FED109D83}']
  end;

  IRichEdit = interface(IBasic)
    ['{C4878DD0-57BF-4A32-809A-9F7AC7B6CC5E}']
  end;
{$IFDEF MAINAPP}

  IPicture = interface(IComboBox)
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

  IControlEvent = interface(IUnknown)
    ['{207E1DBF-69A1-4E05-9CEA-BE5BA4091EF9}']
    procedure OnNotify(const Sender: IBasic); stdcall;
  end;

  IReleaseNameChange = interface(IUnknown)
    ['{783B9FD1-BCE5-478E-A61C-CB68A717C8DF}']
    procedure OnNotify(const AReleaseName: WideString); stdcall;
  end;

  IComponentController = interface
    ['{E9432D30-D4CA-4045-BEA3-55C02E56243A}']
    function GetTemplateTypeID: TTemplateTypeID;
    procedure SetTemplateTypeID(ATemplateTypeID: TTemplateTypeID);
    function GetControl(index: Integer): IBasic;
    procedure SetControl(index: Integer; AControl: IBasic);

    function GetSpaceMouseDown: INotifyEvent;
    procedure SetSpaceMouseDown(ASpaceMouseDown: INotifyEvent);
    function GetControlChange: IControlEvent;
    procedure SetControlChange(AControlChange: IControlEvent);
    function GetControlEnter: IControlEvent;
    procedure SetControlEnter(AControlEnter: IControlEvent);
    function GetControlExit: IControlEvent;
    procedure SetControlExit(AControlExit: IControlEvent);
    function GetReleaseNameChange: IReleaseNameChange;
    procedure SetReleaseNameChange(AReleaseNameChange: IReleaseNameChange);
    function GetPopupMenuChange: IPopupMenuChange;
    procedure SetPopupMenuChange(APopupMenuChange: IPopupMenuChange);

    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID write SetTemplateTypeID;
    procedure NewControl(AType: TComponentID; ATitle, AValue, AHint, AList: WideString; ALeft, ATop, AWidth, AHeight: Integer);
    function FindControl(ComponentID: TComponentID): IBasic;
    property Control[index: Integer]: IBasic read GetControl write SetControl;
    function ControlCount: Integer;

    property OnSpaceMouseDown: INotifyEvent read GetSpaceMouseDown write SetSpaceMouseDown;
    property OnControlChange: IControlEvent read GetControlChange write SetControlChange;
    property OnControlEnter: IControlEvent read GetControlEnter write SetControlEnter;
    property OnControlExit: IControlEvent read GetControlExit write SetControlExit;
    property OnReleaseNameChange: IReleaseNameChange read GetReleaseNameChange write SetReleaseNameChange;
    property OnPopupMenuChange: IPopupMenuChange read GetPopupMenuChange write SetPopupMenuChange;
  end;

  ICMSWebsite = interface
    ['{A78282FB-748E-453C-BD2A-5DFE847DE8D6}']
    function GetName: WideString;
    function GetEnabled: Boolean;
    function GetSubject: WideString;
    procedure SetSubject(ASubject: WideString);
    function GetAccountName: WideString;
    procedure SetAccountName(AAccountName: WideString);
    // function GetAccountPassword: WideString;
    // procedure SetAccountPassword(AAccountPassword: WideString);
    function GetTemplateFileName: WideString;
    procedure SetTemplateFileName(ATemplateFileName: WideString);

    property name: WideString read GetName;
    property Enabled: Boolean read GetEnabled;
    property Subject: WideString read GetSubject write SetSubject;
    property AccountName: WideString read GetAccountName write SetAccountName;
    // property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
    property TemplateFileName: WideString read GetTemplateFileName write SetTemplateFileName;
  end;

  ICMSContainer = interface
    ['{80C890DA-8445-46DD-BDEA-3F10DE5D7AAA}']
    function GetName: WideString;
    function GetEnabled: Boolean;
    function GetWebsite(AIndex: Integer): ICMSWebsite;

    property name: WideString read GetName;
    property Enabled: Boolean read GetEnabled;
    property Website[index: Integer]: ICMSWebsite read GetWebsite;
    function Count: Integer;
  end;

  IPublishController = interface
    ['{D7D1CA65-ED2D-4854-AEE6-E6E3D9DD801F}']
    function GetCMS(AIndex: Integer): ICMSContainer;

    property CMS[index: Integer]: ICMSContainer read GetCMS;
    function Count: Integer;
  end;

  ITabSheetController = interface
    ['{1973A687-DD39-472A-8B07-D541CFA08780}']
    function GetPageController: IPageController;
    function GetFileName: WideString;
    function GetFileType: WideString;
    function GetReleaseName: WideString;
    procedure SetReleaseName(AReleaseName: WideString);
    function GetComponentController: IComponentController;
    procedure SetComponentController(const AComponentController: IComponentController);
    function GetMirrorController: IMirrorController;
    procedure SetMirrorController(const AMirrorController: IMirrorController);
    function GetPublishController: IPublishController;
    procedure SetPublishController(const APublishController: IPublishController);

    property PageController: IPageController read GetPageController;
    property FileName: WideString read GetFileName;
    property FileType: WideString read GetFileType;
    property ReleaseName: WideString read GetReleaseName write SetReleaseName;

    procedure Save(AFileName, AFileType: WideString);
    procedure ResetDataChanged(AFileName, AFileType: WideString);

    property ComponentController: IComponentController read GetComponentController write SetComponentController;
    property MirrorController: IMirrorController read GetMirrorController write SetMirrorController;
    property PublishController: IPublishController read GetPublishController write SetPublishController;
  end;

  IHosterManager = interface
    ['{03675561-B598-4929-B5C9-FBEF9FB52656}']
    procedure AddHosterCheckJob(const ADirectlinksMirror: IDirectlinksMirror);
    // procedure RemoveHosterCheckJob(const ADirectlinksMirror: IDirectlinksMirror);
  end;

  ICrypterManager = interface
    ['{3ABA9E7F-6D3D-4F35-94E8-45ED37228710}']
    procedure AddCrypterJob(const ACrypterPanel: ICrypterPanel);
    procedure AddCrypterCheckJob(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: Boolean = False);
  end;

  IPageController = interface
    ['{735F0449-3C70-4701-BBC3-9FE3F229DD96}']
    function GetHosterManager: IHosterManager;
    function GetCrypterManager: ICrypterManager;
    function GetActiveTabSheetIndex: Integer;
    function GetActiveTabSheetController: ITabSheetController;
    function GetTabSheetController(index: Integer): ITabSheetController;

    procedure CallComponentParser;

    procedure OpenToNewTab(AFileName: WideString = '');

    function Add(AFileName: WideString; ATemplateTypeID: TTemplateTypeID; AEmpty: Boolean = False): Integer;
    property HosterManager: IHosterManager read GetHosterManager;
    property CrypterManager: ICrypterManager read GetCrypterManager;
    property ActiveTabSheetIndex: Integer read GetActiveTabSheetIndex;
    property ActiveTabSheetController: ITabSheetController read GetActiveTabSheetController;
    property TabSheetController[index: Integer]: ITabSheetController read GetTabSheetController;
    function TabSheetCount: Integer;
  end;

  IAppController = interface
    ['{80AF47FD-847E-429D-922C-0F3B812AB637}']
    function GetMainMenu: IMainMenu;
    function GetPageController: IPageController;
    function GetPublishController: IPublishController;
    procedure SetPublishController(const APublishController: IPublishController);

    function GetCustomisedHoster(const AHoster: WideString; AShortName: Boolean = False): WideString;
    function GetControlValues(const ATemplateTypeID: TTemplateTypeID; const AComponentID: TComponentID): WideString;
    function ParseIScript(const s: WideString; const ATabSheetController: ITabSheetController): WideString;

    property MainMenu: IMainMenu read GetMainMenu;
    property PageController: IPageController read GetPageController;
    property PublishController: IPublishController read GetPublishController write SetPublishController;
  end;

implementation

end.
