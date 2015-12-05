unit uApiUpdateInterface;

interface

uses
  // Common
  uBaseConst,
  // Export
  uDynamicExport,
  // Api
  uApiUpdateConst, uApiUpdateInterfaceBase;

type
  IStatusFile = interface(IFile)
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);

    property Status: WordBool read GetStatus write SetStatus;
  end;

  IUpdateManagerVersion = interface(IFileVersion)
    ['{DDAF7DDA-822F-4EAC-98C5-0E64F35AAD93}']
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);

    property ID: Integer read GetID write SetID;
    property Active: WordBool read GetActive write SetActive;
  end;

  IUpdateManagerSystemFileBase = interface(IUpdateSystemFileBase)
    ['{125B2271-7206-4C59-A837-52F55A60F9AA}']
    function GetID: Integer;
    procedure SetID(AID: Integer);

    function GetFullFileName(AIntelligeNFileSystem: TIntelligeNFileSystem): WideString;

    property ID: Integer read GetID write SetID;
  end;

  IUpdateManagerLocalFile = interface(IFile)
    ['{FF118ECC-48CE-4F93-9841-97DFE234F928}']
    function GetOnline: WordBool;
    procedure SetOnline(AOnline: WordBool);
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);
    function GetCondition: TUpdateCondition;
    procedure SetCondition(ACondition: TUpdateCondition);
    function GetAction: TUpdateAction;
    procedure SetAction(AAction: TUpdateAction);
    function GetActions: TUpdateActions;
    procedure SetActions(AActions: TUpdateActions);

    property Online: WordBool read GetOnline write SetOnline;
    property Status: WordBool read GetStatus write SetStatus;
    property Condition: TUpdateCondition read GetCondition write SetCondition;
    property Action: TUpdateAction read GetAction write SetAction;
    property Actions: TUpdateActions read GetActions write SetActions;
  end;

  IUpdateManagerLocalSystemFile = interface(IUpdateSystemFile)
    ['{4666FFA3-9D57-45B5-A15F-D29C36103ABB}']
    function GetFileBase: IUpdateManagerSystemFileBase;
    procedure SetFileBase(const AFileBase: IUpdateManagerSystemFileBase);
    function GetLocalFile: IUpdateManagerLocalFile;
    procedure SetLocalFile(const ALocalFile: IUpdateManagerLocalFile);

    function GetCompressedFileName: WideString;

    property FileBase: IUpdateManagerSystemFileBase read GetFileBase write SetFileBase;
    property LocalFile: IUpdateManagerLocalFile read GetLocalFile write SetLocalFile;
  end;

  IUpdateManagerOnlineSystemFile = interface(IUpdateSystemFile)
    ['{6220391C-D96C-4FB9-B2CD-6F09689F4FF1}']
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetFileBase: IUpdateManagerSystemFileBase;
    procedure SetFileBase(const AFileBase: IUpdateManagerSystemFileBase);

    property ID: Integer read GetID write SetID;
    property FileBase: IUpdateManagerSystemFileBase read GetFileBase write SetFileBase;
  end;

  IServer = interface
    function GetName: WideString;
    procedure SetName(AName: WideString);

    property Name: WideString read GetName write SetName;
  end;

  IUpdateServer = interface(IServer)
    function GetAccessToken: WideString;
    procedure SetAccessToken(AAccessToken: WideString);

    property AccessToken: WideString read GetAccessToken write SetAccessToken;
  end;

  IFTPServer = interface(IServer)
    function GetPort: WideString;
    procedure SetPort(APort: WideString);
    function GetPath: WideString;
    procedure SetPath(APath: WideString);
    function GetUsername: WideString;
    procedure SetUsername(AUsername: WideString);
    function GetPassword: WideString;
    procedure SetPassword(APassword: WideString);

    property Port: WideString read GetPort write SetPort;
    property Path: WideString read GetPath write SetPath;
    property Username: WideString read GetUsername write SetUsername;
    property Password: WideString read GetPassword write SetPassword;
  end;

implementation

end.
